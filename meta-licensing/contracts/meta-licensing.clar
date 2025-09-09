;; MetaLicensing - Creative IP Licensing Platform with Adaptive Rights Management

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-LICENSE-REQUEST (err u101))
(define-constant ERR-LICENSE-REQUEST-NOT-FOUND (err u102))
(define-constant ERR-INSUFFICIENT-CREATOR-SCORE (err u103))
(define-constant ERR-LICENSE-REQUEST-EXPIRED (err u104))
(define-constant ERR-ALREADY-REVIEWED (err u105))
(define-constant ERR-INVALID-AMOUNT (err u106))
(define-constant ERR-MILESTONE-NOT-READY (err u107))
(define-constant ERR-CREATOR-SCORE-LOCKED (err u108))
(define-constant ERR-INVALID-PHASE (err u109))
(define-constant ERR-ATTRIBUTION-ERROR (err u110))
(define-constant ERR-INSUFFICIENT-FUNDS (err u111))
(define-constant ERR-LICENSE-ALREADY-GRANTED (err u112))

;; Contract Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant CREATOR-SCORE-DECAY-RATE u5) ;; 5% per cycle
(define-constant MIN-LICENSE-REQUEST-SCORE u100)
(define-constant REVIEW-PERIOD u1008) ;; ~1 week in blocks
(define-constant SUBMISSION-PERIOD u144) ;; ~1 day in blocks
(define-constant ADAPTIVE-SCALING u10000)

;; Data Variables
(define-data-var license-request-counter uint u0)
(define-data-var royalty-pool-balance uint u0)
(define-data-var creator-score-decay-cycle uint u0)
(define-data-var attribution-oracle-address (optional principal) none)
(define-data-var platform-paused bool false)
(define-data-var min-community-consensus uint u1000)

;; License Request Structure
(define-map license-requests uint {
    id: uint,
    requester: principal,
    title: (string-utf8 100),
    description: (string-utf8 500),
    licensing-fee: uint,
    phase: (string-ascii 20), ;; "submission", "review", "granting", "completed", "rejected"
    created-at: uint,
    review-ends-at: uint,
    approve-votes: uint,
    reject-votes: uint,
    score-weighted-approve: uint,
    score-weighted-reject: uint,
    milestones-completed: uint,
    total-milestones: uint,
    usage-score: uint,
    content-dna-tags: (list 5 (string-ascii 20)),
    executed: bool,
    royalties-distributed: uint
})

;; Creator Reputation System
(define-map creator-scores principal {
    base-creator-score: uint,
    decay-adjusted: uint,
    last-activity: uint,
    successful-licenses: uint,
    failed-licenses: uint,
    review-accuracy: uint,
    locked-creator-score: uint,
    score-source: (string-ascii 50)
})

;; Review Records
(define-map reviews {license-request-id: uint, reviewer: principal} {
    review-weight: uint,
    creator-score-at-review: uint,
    review-direction: bool, ;; true for approve, false for reject
    timestamp: uint,
    adaptive-weight: uint
})

;; License Request Content DNA System
(define-map license-request-dna {license-request-id: uint, tag: (string-ascii 20)} {
    confidence-score: uint,
    historical-success-rate: uint,
    similar-requests: (list 10 uint),
    risk-assessment: uint
})

;; Milestone Tracking
(define-map license-request-milestones {license-request-id: uint, milestone-id: uint} {
    description: (string-utf8 200),
    target-date: uint,
    completion-date: (optional uint),
    required-amount: uint,
    verification-method: (string-ascii 30),
    completed: bool,
    attribution-verified: bool
})

;; Royalty Pool Management
(define-map royalty-allocations uint {
    license-request-id: uint,
    allocated-amount: uint,
    released-amount: uint,
    locked-until: uint,
    reallocation-target: (optional uint)
})

;; Creator Score Appeals
(define-map creator-score-appeals principal {
    appeal-reason: (string-utf8 300),
    requested-adjustment: int,
    submitted-at: uint,
    status: (string-ascii 20), ;; "pending", "approved", "rejected"
    reviewed-by: (optional principal)
})

;; Attribution Oracle Data Integration
(define-map attribution-requests uint {
    request-type: (string-ascii 30),
    license-request-id: uint,
    data-hash: (buff 32),
    timestamp: uint,
    verified: bool,
    result: (optional uint)
})

;; Helper function to calculate creator score with decay
(define-private (calculate-current-creator-score (creator principal))
    (let (
        (stored-score (default-to {
            base-creator-score: u0,
            decay-adjusted: u0,
            last-activity: u0,
            successful-licenses: u0,
            failed-licenses: u0,
            review-accuracy: u100,
            locked-creator-score: u0,
            score-source: "none"
        } (map-get? creator-scores creator)))
        (blocks-since-activity (- block-height (get last-activity stored-score)))
        (decay-cycles (/ blocks-since-activity u144))
        (total-decay-rate (* decay-cycles CREATOR-SCORE-DECAY-RATE))
        (decay-multiplier (if (>= total-decay-rate u100) u0 (- u100 total-decay-rate)))
        (current-creator-score (/ (* (get base-creator-score stored-score) decay-multiplier) u100))
    )
        current-creator-score
    )
)

;; Utility Functions - Fixed to return proper response type
(define-private (update-creator-activity (creator principal))
    (let (
        (current-score (default-to {
            base-creator-score: u0,
            decay-adjusted: u0,
            last-activity: u0,
            successful-licenses: u0,
            failed-licenses: u0,
            review-accuracy: u100,
            locked-creator-score: u0,
            score-source: "activity"
        } (map-get? creator-scores creator)))
    )
        (map-set creator-scores creator (merge current-score {
            last-activity: block-height
        }))
        (ok true)
    )
)

;; Administrative Functions
(define-public (initialize-platform (initial-royalty-pool uint) (attribution-oracle-addr principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set royalty-pool-balance initial-royalty-pool)
        (var-set attribution-oracle-address (some attribution-oracle-addr))
        (ok true)
    )
)

(define-public (update-platform-parameters (new-min-consensus uint) (new-min-score uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> new-min-consensus u0) ERR-INVALID-AMOUNT)
        (asserts! (> new-min-score u0) ERR-INVALID-AMOUNT)
        (var-set min-community-consensus new-min-consensus)
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

;; License Request Lifecycle Management
(define-public (create-license-request 
    (title (string-utf8 100))
    (description (string-utf8 500))
    (licensing-fee uint)
    (milestones uint)
    (content-dna-tags (list 5 (string-ascii 20))))
    (let (
        (current-creator-score (calculate-current-creator-score tx-sender))
        (new-license-request-id (+ (var-get license-request-counter) u1))
        (current-block block-height)
    )
        (asserts! (not (var-get platform-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (>= current-creator-score MIN-LICENSE-REQUEST-SCORE) ERR-INSUFFICIENT-CREATOR-SCORE)
        (asserts! (> licensing-fee u0) ERR-INVALID-AMOUNT)
        (asserts! (> milestones u0) ERR-INVALID-AMOUNT)
        (asserts! (<= licensing-fee (var-get royalty-pool-balance)) ERR-INSUFFICIENT-FUNDS)
        
        (map-set license-requests new-license-request-id {
            id: new-license-request-id,
            requester: tx-sender,
            title: title,
            description: description,
            licensing-fee: licensing-fee,
            phase: "submission",
            created-at: current-block,
            review-ends-at: (+ current-block SUBMISSION-PERIOD REVIEW-PERIOD),
            approve-votes: u0,
            reject-votes: u0,
            score-weighted-approve: u0,
            score-weighted-reject: u0,
            milestones-completed: u0,
            total-milestones: milestones,
            usage-score: u0,
            content-dna-tags: content-dna-tags,
            executed: false,
            royalties-distributed: u0
        })
        
        (var-set license-request-counter new-license-request-id)
        (unwrap! (update-creator-activity tx-sender) ERR-NOT-AUTHORIZED)
        (ok new-license-request-id)
    )
)

(define-public (advance-license-request-phase (license-request-id uint))
    (let (
        (license-request (unwrap! (map-get? license-requests license-request-id) ERR-LICENSE-REQUEST-NOT-FOUND))
        (current-phase (get phase license-request))
        (current-block block-height)
    )
        (asserts! (not (var-get platform-paused)) ERR-NOT-AUTHORIZED)
        
        (if (is-eq current-phase "submission")
            (begin
                (asserts! (> current-block (+ (get created-at license-request) SUBMISSION-PERIOD)) ERR-INVALID-PHASE)
                (map-set license-requests license-request-id (merge license-request {phase: "review"}))
                (ok "moved-to-review")
            )
            (if (is-eq current-phase "review")
                (begin
                    (asserts! (> current-block (get review-ends-at license-request)) ERR-INVALID-PHASE)
                    (let ((license-request-approved (evaluate-license-request-outcome license-request-id)))
                        (if license-request-approved
                            (begin
                                (map-set license-requests license-request-id (merge license-request {phase: "granting"}))
                                (unwrap! (allocate-royalties license-request-id (get licensing-fee license-request)) ERR-INSUFFICIENT-FUNDS)
                                (ok "moved-to-granting")
                            )
                            (begin
                                (map-set license-requests license-request-id (merge license-request {phase: "rejected"}))
                                (ok "license-request-rejected")
                            )
                        )
                    )
                )
                ERR-INVALID-PHASE
            )
        )
    )
)

;; Creator Score-Weighted Review System
(define-public (cast-review (license-request-id uint) (review-direction bool))
    (let (
        (license-request (unwrap! (map-get? license-requests license-request-id) ERR-LICENSE-REQUEST-NOT-FOUND))
        (base-weight (calculate-current-creator-score tx-sender))
        (current-block block-height)
        (review-key {license-request-id: license-request-id, reviewer: tx-sender})
        (adaptive-weight (calculate-adaptive-weight base-weight))
    )
        (asserts! (not (var-get platform-paused)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get phase license-request) "review") ERR-INVALID-PHASE)
        (asserts! (< current-block (get review-ends-at license-request)) ERR-LICENSE-REQUEST-EXPIRED)
        (asserts! (is-none (map-get? reviews review-key)) ERR-ALREADY-REVIEWED)
        (asserts! (> base-weight u0) ERR-INSUFFICIENT-CREATOR-SCORE)
        
        (map-set reviews review-key {
            review-weight: base-weight,
            creator-score-at-review: base-weight,
            review-direction: review-direction,
            timestamp: current-block,
            adaptive-weight: adaptive-weight
        })
        
        (if review-direction
            (map-set license-requests license-request-id (merge license-request {
                approve-votes: (+ (get approve-votes license-request) u1),
                score-weighted-approve: (+ (get score-weighted-approve license-request) adaptive-weight)
            }))
            (map-set license-requests license-request-id (merge license-request {
                reject-votes: (+ (get reject-votes license-request) u1),
                score-weighted-reject: (+ (get score-weighted-reject license-request) adaptive-weight)
            }))
        )
        
        (unwrap! (update-creator-activity tx-sender) ERR-NOT-AUTHORIZED)
        (ok true)
    )
)

;; Milestone and Royalty Management
(define-public (complete-milestone (license-request-id uint) (milestone-id uint) (verification-data (buff 32)))
    (let (
        (license-request (unwrap! (map-get? license-requests license-request-id) ERR-LICENSE-REQUEST-NOT-FOUND))
        (milestone-key {license-request-id: license-request-id, milestone-id: milestone-id})
        (milestone (unwrap! (map-get? license-request-milestones milestone-key) ERR-MILESTONE-NOT-READY))
        (current-block block-height)
    )
        (asserts! (is-eq (get phase license-request) "granting") ERR-INVALID-PHASE)
        (asserts! (is-eq tx-sender (get requester license-request)) ERR-NOT-AUTHORIZED)
        (asserts! (not (get completed milestone)) ERR-MILESTONE-NOT-READY)
        
        ;; Submit attribution verification request
        (unwrap! (request-attribution-verification license-request-id milestone-id verification-data) ERR-ATTRIBUTION-ERROR)
        
        (map-set license-request-milestones milestone-key (merge milestone {
            completion-date: (some current-block),
            completed: true
        }))
        
        ;; Update license request milestone completion count
        (map-set license-requests license-request-id (merge license-request {
            milestones-completed: (+ (get milestones-completed license-request) u1)
        }))
        
        ;; Release royalties if milestone verified
        (unwrap! (release-milestone-royalties license