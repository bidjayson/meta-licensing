;; MetaLicensing - Creative IP Licensing Platform

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-REQUEST (err u101))
(define-constant ERR-REQUEST-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-SCORE (err u103))
(define-constant ERR-REQUEST-EXPIRED (err u104))
(define-constant ERR-ALREADY-REVIEWED (err u105))
(define-constant ERR-INVALID-AMOUNT (err u106))
(define-constant ERR-INSUFFICIENT-FUNDS (err u107))

;; Contract Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-CREATOR-SCORE u100)
(define-constant REVIEW-PERIOD u1008) ;; ~1 week in blocks

;; Data Variables
(define-data-var request-counter uint u0)
(define-data-var royalty-pool uint u0)
(define-data-var platform-paused bool false)

;; License Request Structure
(define-map license-requests uint {
    id: uint,
    requester: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    licensing-fee: uint,
    phase: (string-ascii 20), ;; "review", "approved", "rejected"
    created-at: uint,
    review-ends-at: uint,
    approve-votes: uint,
    reject-votes: uint,
    executed: bool
})

;; Creator Reputation System
(define-map creator-scores principal {
    score: uint,
    last-activity: uint,
    successful-licenses: uint
})

;; Review Records
(define-map reviews {request-id: uint, reviewer: principal} {
    direction: bool, ;; true for approve, false for reject
    timestamp: uint
})

;; Helper Functions
(define-private (get-creator-score (creator principal))
    (get score (default-to {score: u100, last-activity: u0, successful-licenses: u0} 
                          (map-get? creator-scores creator)))
)

(define-private (update-creator-activity (creator principal))
    (let (
        (current-score (default-to {score: u100, last-activity: u0, successful-licenses: u0} 
                                  (map-get? creator-scores creator)))
    )
        (map-set creator-scores creator (merge current-score {last-activity: block-height}))
        (ok true)
    )
)

;; Administrative Functions
(define-public (initialize-platform (initial-pool uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set royalty-pool initial-pool)
        (ok true)
    )
)

(define-public (pause-platform)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set platform-paused true)
        (ok true)
    )
)

;; Core Functions
(define-public (create-license-request 
    (title (string-utf8 100))
    (description (string-utf8 500))
    (licensing-fee uint))
    (let (
        (creator-score (get-creator-score tx-sender))
        (new-id (+ (var-get request-counter) u1))
    )
        (asserts! (not (var-get platform-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (>= creator-score MIN-CREATOR-SCORE) ERR-INSUFFICIENT-SCORE)
        (asserts! (> licensing-fee u0) ERR-INVALID-AMOUNT)
        (asserts! (<= licensing-fee (var-get royalty-pool)) ERR-INSUFFICIENT-FUNDS)
        
        (map-set license-requests new-id {
            id: new-id,
            requester: tx-sender,
            title: title,
            description: description,
            licensing-fee: licensing-fee,
            phase: "review",
            created-at: block-height,
            review-ends-at: (+ block-height REVIEW-PERIOD),
            approve-votes: u0,
            reject-votes: u0,
            executed: false
        })
        
        (var-set request-counter new-id)
        (unwrap! (update-creator-activity tx-sender) ERR-NOT-AUTHORIZED)
        (ok new-id)
    )
)

(define-public (cast-review (request-id uint) (approve bool))
    (let (
        (request (unwrap! (map-get? license-requests request-id) ERR-REQUEST-NOT-FOUND))
        (review-key {request-id: request-id, reviewer: tx-sender})
    )
        (asserts! (not (var-get platform-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get phase request) "review") ERR-INVALID-REQUEST)
        (asserts! (< block-height (get review-ends-at request)) ERR-REQUEST-EXPIRED)
        (asserts! (is-none (map-get? reviews review-key)) ERR-ALREADY-REVIEWED)
        (asserts! (>= (get-creator-score tx-sender) MIN-CREATOR-SCORE) ERR-INSUFFICIENT-SCORE)
        
        (map-set reviews review-key {
            direction: approve,
            timestamp: block-height
        })
        
        (if approve
            (map-set license-requests request-id (merge request {
                approve-votes: (+ (get approve-votes request) u1)
            }))
            (map-set license-requests request-id (merge request {
                reject-votes: (+ (get reject-votes request) u1)
            }))
        )
        
        (unwrap! (update-creator-activity tx-sender) ERR-NOT-AUTHORIZED)
        (ok true)
    )
)

(define-public (finalize-request (request-id uint))
    (let (
        (request (unwrap! (map-get? license-requests request-id) ERR-REQUEST-NOT-FOUND))
        (total-votes (+ (get approve-votes request) (get reject-votes request)))
        (approved (> (get approve-votes request) (get reject-votes request)))
    )
        (asserts! (is-eq (get phase request) "review") ERR-INVALID-REQUEST)
        (asserts! (> block-height (get review-ends-at request)) ERR-REQUEST-EXPIRED)
        (asserts! (> total-votes u2) ERR-INSUFFICIENT-SCORE) ;; Minimum 3 votes
        
        (if approved
            (begin
                (map-set license-requests request-id (merge request {
                    phase: "approved",
                    executed: true
                }))
                (var-set royalty-pool (- (var-get royalty-pool) (get licensing-fee request)))
                ;; Update requester's success count
                (let ((requester-score (default-to {score: u100, last-activity: u0, successful-licenses: u0} 
                                                   (map-get? creator-scores (get requester request)))))
                    (map-set creator-scores (get requester request) (merge requester-score {
                        successful-licenses: (+ (get successful-licenses requester-score) u1),
                        score: (+ (get score requester-score) u10)
                    }))
                )
                (ok "approved")
            )
            (begin
                (map-set license-requests request-id (merge request {phase: "rejected"}))
                (ok "rejected")
            )
        )
    )
)

(define-public (update-creator-score (creator principal) (new-score uint))
    (let (
        (current (default-to {score: u100, last-activity: u0, successful-licenses: u0} 
                            (map-get? creator-scores creator)))
    )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-score u1000) ERR-INVALID-AMOUNT)
        
        (map-set creator-scores creator (merge current {score: new-score}))
        (ok new-score)
    )
)

;; Read-only functions
(define-read-only (get-request (request-id uint))
    (map-get? license-requests request-id)
)

(define-read-only (get-creator-info (creator principal))
    (map-get? creator-scores creator)
)

(define-read-only (get-review (request-id uint) (reviewer principal))
    (map-get? reviews {request-id: request-id, reviewer: reviewer})
)

(define-read-only (get-platform-stats)
    {
        total-requests: (var-get request-counter),
        royalty-pool: (var-get royalty-pool),
        platform-paused: (var-get platform-paused)
    }
)