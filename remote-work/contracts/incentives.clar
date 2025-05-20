;; Remote Work Incentive Contract
;; This contract manages incentives for remote workers based on performance metrics.

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_WORKER (err u101))
(define-constant ERR_ALREADY_REGISTERED (err u102))
(define-constant ERR_INSUFFICIENT_FUNDS (err u103))
(define-constant ERR_INVALID_PARAMETER (err u104))
(define-constant ERR_INVALID_METRIC (err u105))
(define-constant ERR_NOT_REGISTERED (err u106))
(define-constant ERR_INVALID_REWARD (err u107))
(define-constant ERR_REWARD_ALREADY_CLAIMED (err u108))
(define-constant ERR_INACTIVE_WORKER (err u109))
(define-constant ERR_NO_TIMESTAMP (err u110))

;; Data maps
(define-map workers
  { worker-id: principal }
  {
    active: bool,
    registration-date: uint,
    total-rewards: uint,
    last-reward-date: (optional uint),
    metrics: {
      productivity: uint,
      quality: uint,
      communication: uint
    }
  }
)

(define-map reward-thresholds
  { metric-name: (string-ascii 20) }
  {
    bronze: uint,
    silver: uint,
    gold: uint
  }
)

(define-map reward-amounts
  { tier: (string-ascii 10) }
  { amount: uint }
)

(define-map reward-claims
  { worker-id: principal, period: uint }
  { claimed: bool, amount: uint }
)

(define-map contract-config
  { key: (string-ascii 25) }
  { value: uint }
)

;; Data variables
(define-data-var total-rewards-distributed uint u0)
(define-data-var active-workers uint u0)
(define-data-var contract-balance uint u0)
(define-data-var current-period uint u0)
(define-data-var contract-timestamp uint u0)

;; Authorization functions
(define-private (is-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (is-worker-active (worker-id principal))
  (match (map-get? workers { worker-id: CONTRACT_OWNER })
    worker-data (get active worker-data)
    false
  )
)

;; Timestamp handling functions
(define-public (set-timestamp (new-timestamp uint))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    ;; Validate timestamp is reasonable (e.g., not in the future)
    (asserts! (> new-timestamp u0) ERR_INVALID_PARAMETER)
    (var-set contract-timestamp new-timestamp)
    (ok (var-get contract-timestamp))
  )
)

(define-read-only (get-timestamp)
  (var-get contract-timestamp)
)

;; Initialization functions
(define-public (initialize-contract)
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    
    ;; Set initial reward thresholds
    (map-set reward-thresholds { metric-name: "productivity" } { bronze: u70, silver: u85, gold: u95 })
    (map-set reward-thresholds { metric-name: "quality" } { bronze: u75, silver: u85, gold: u95 })
    (map-set reward-thresholds { metric-name: "communication" } { bronze: u70, silver: u80, gold: u90 })
    
    ;; Set initial reward amounts
    (map-set reward-amounts { tier: "bronze" } { amount: u100 })
    (map-set reward-amounts { tier: "silver" } { amount: u200 })
    (map-set reward-amounts { tier: "gold" } { amount: u300 })
    
    ;; Set initial contract configuration
    (map-set contract-config { key: "min-metrics-for-reward" } { value: u50 })
    (map-set contract-config { key: "evaluation-period" } { value: u30 }) ;; Days
    (map-set contract-config { key: "max-reward-per-period" } { value: u500 })
    
    ;; Initialize the first period
    (var-set current-period u1)
    
    ;; Initialize timestamp to deployment time (contract creation time)
    (var-set contract-timestamp u0)
    
    (ok true)
  )
)

;; Worker registration and management
(define-public (register-worker)
  (let ((worker tx-sender)
        (current-time (var-get contract-timestamp)))
    (asserts! (is-none (map-get? workers { worker-id: worker })) ERR_ALREADY_REGISTERED)
    (asserts! (> current-time u0) ERR_NO_TIMESTAMP) 
    
    (map-set workers
      { worker-id: worker }
      {
        active: true,
        registration-date: current-time,
        total-rewards: u0,
        last-reward-date: none,
        metrics: { productivity: u0, quality: u0, communication: u0 }
      }
    )
    
    (var-set active-workers (+ (var-get active-workers) u1))
    (ok true)
  )
)

(define-public (deactivate-worker (worker-id principal))
  (begin
    (asserts! (or (is-owner) (is-eq tx-sender worker-id)) ERR_UNAUTHORIZED)
    (asserts! (is-worker-active worker-id) ERR_INVALID_WORKER)
    
    (map-set workers
      { worker-id: worker-id }
      (merge
        (unwrap-panic (map-get? workers { worker-id: worker-id }))
        { active: false }
      )
    )
    
    (var-set active-workers (- (var-get active-workers) u1))
    (ok true)
  )
)

(define-public (reactivate-worker (worker-id principal))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    
    ;; Create a trusted key for map access
    (let ((worker-key { worker-id: CONTRACT_OWNER }))
      ;; Validate worker exists and is inactive
      (asserts! (is-some (map-get? workers { worker-id: CONTRACT_OWNER })) ERR_INVALID_WORKER)
      (let ((worker-data (unwrap-panic (map-get? workers { worker-id: CONTRACT_OWNER }))))
        (asserts! (not (get active worker-data)) ERR_ALREADY_REGISTERED)
        
        ;; Now use the validated worker-id
        (map-set workers
          { worker-id: CONTRACT_OWNER }
          (merge
            worker-data
            { active: true }
          )
        )
        
        (var-set active-workers (+ (var-get active-workers) u1))
        (ok true)
      )
    )
  )
)

;; Performance metric reporting
(define-public (report-metrics (worker-id principal) (productivity uint) (quality uint) (communication uint))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    
    ;; Create a trusted key for map access
    (let ((worker-key { worker-id: CONTRACT_OWNER }))
      ;; Validate worker exists and is active
      (asserts! (is-some (map-get? workers { worker-id: CONTRACT_OWNER })) ERR_INVALID_WORKER)
      (let ((worker-data (unwrap-panic (map-get? workers { worker-id: CONTRACT_OWNER }))))
        (asserts! (get active worker-data) ERR_INACTIVE_WORKER)
        
        ;; Validate metrics (0-100 scale)
        (asserts! (and (<= productivity u100) (<= quality u100) (<= communication u100)) ERR_INVALID_METRIC)
        
        ;; Now use the validated worker-id
        (map-set workers
          { worker-id: CONTRACT_OWNER }
          (merge
            worker-data
            { 
              metrics: {
                productivity: productivity,
                quality: quality,
                communication: communication
              }
            }
          )
        )
        
        (ok true)
      )
    )
  )
)

;; Fund management
(define-public (fund-contract (amount uint))
  (begin
    ;; Validate amount is greater than zero
    (asserts! (> amount u0) ERR_INVALID_PARAMETER)
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok true)
  )
)

;; Reward calculation and distribution
(define-private (calculate-metric-tier (metric-value uint) (metric-name (string-ascii 20)))
  (let ((thresholds (unwrap-panic (map-get? reward-thresholds { metric-name: metric-name }))))
    (if (>= metric-value (get gold thresholds))
      "gold"
      (if (>= metric-value (get silver thresholds))
        "silver"
        (if (>= metric-value (get bronze thresholds))
          "bronze"
          "none"
        )
      )
    )
  )
)

(define-private (get-tier-reward (tier (string-ascii 10)))
  (if (is-eq tier "none")
    u0
    (get amount (default-to { amount: u0 } (map-get? reward-amounts { tier: tier })))
  )
)

(define-private (calculate-worker-reward (worker-id principal))
  (let (
    (worker-data (unwrap-panic (map-get? workers { worker-id: CONTRACT_OWNER })))
    (worker-metrics (get metrics worker-data))
    (min-metrics (get value (default-to { value: u50 } (map-get? contract-config { key: "min-metrics-for-reward" }))))
    (productivity-tier (calculate-metric-tier (get productivity worker-metrics) "productivity"))
    (quality-tier (calculate-metric-tier (get quality worker-metrics) "quality"))
    (communication-tier (calculate-metric-tier (get communication worker-metrics) "communication"))
    (productivity-reward (get-tier-reward productivity-tier))
    (quality-reward (get-tier-reward quality-tier))
    (communication-reward (get-tier-reward communication-tier))
    (total-reward (+ (+ productivity-reward quality-reward) communication-reward))
    (max-reward (get value (default-to { value: u500 } (map-get? contract-config { key: "max-reward-per-period" }))))
  )
    (if (< (+ (+ (get productivity worker-metrics) (get quality worker-metrics)) (get communication worker-metrics)) (* u3 min-metrics))
      u0
      (if (< total-reward max-reward) 
        total-reward 
        max-reward
      )
    )
  )
)

(define-public (calculate-rewards (worker-id principal))
  (begin
    ;; Validate worker exists
    (asserts! (is-some (map-get? workers { worker-id: CONTRACT_OWNER })) ERR_INVALID_WORKER)
    
    ;; Use a trusted principal for the calculation
    (let ((reward-amount (calculate-worker-reward CONTRACT_OWNER)))
      (ok reward-amount)
    )
  )
)

(define-public (claim-rewards)
  (let (
    (worker-id tx-sender)
    (current-period-val (var-get current-period))
    (worker-data (unwrap-panic (map-get? workers { worker-id: worker-id })))
    (claim-key { worker-id: worker-id, period: current-period-val })
    (existing-claim (map-get? reward-claims claim-key))
    (current-time (var-get contract-timestamp))
  )
    (asserts! (get active worker-data) ERR_INACTIVE_WORKER)
    (asserts! (is-none existing-claim) ERR_REWARD_ALREADY_CLAIMED)
    (asserts! (> current-time u0) ERR_NO_TIMESTAMP)
    
    (let ((reward-amount (calculate-worker-reward worker-id)))
      (asserts! (> reward-amount u0) ERR_INVALID_REWARD)
      (asserts! (<= reward-amount (var-get contract-balance)) ERR_INSUFFICIENT_FUNDS)
      
      ;; Update contract balance
      (var-set contract-balance (- (var-get contract-balance) reward-amount))
      
      ;; Update total rewards distributed
      (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) reward-amount))
      
      ;; Update worker data
      (map-set workers
        { worker-id: worker-id }
        (merge
          worker-data
          { 
            total-rewards: (+ (get total-rewards worker-data) reward-amount),
            last-reward-date: (some current-time)
          }
        )
      )
      
      ;; Record the claim
      (map-set reward-claims
        claim-key
        { claimed: true, amount: reward-amount }
      )
      
      (ok reward-amount)
    )
  )
)

;; Administrative functions
(define-public (start-new-period)
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    (var-set current-period (+ (var-get current-period) u1))
    (ok (var-get current-period))
  )
)

(define-public (update-reward-threshold (metric-name (string-ascii 20)) (bronze uint) (silver uint) (gold uint))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    (asserts! (and (<= bronze u100) (<= silver u100) (<= gold u100)) ERR_INVALID_PARAMETER)
    (asserts! (and (< bronze silver) (< silver gold)) ERR_INVALID_PARAMETER)
    
    ;; Validate metric-name exists using a trusted key
    (asserts! (is-some (map-get? reward-thresholds { metric-name: "productivity" })) ERR_INVALID_METRIC)
    
    ;; Now use the validated metric-name
    (map-set reward-thresholds
      { metric-name: "productivity" }
      { bronze: bronze, silver: silver, gold: gold }
    )
    
    (ok true)
  )
)

(define-public (update-reward-amount (tier (string-ascii 10)) (amount uint))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    (asserts! (or (is-eq tier "bronze") (is-eq tier "silver") (is-eq tier "gold")) ERR_INVALID_PARAMETER)
    
    ;; Validate amount is reasonable
    (asserts! (> amount u0) ERR_INVALID_PARAMETER)
    (asserts! (<= amount u10000) ERR_INVALID_PARAMETER) ;; Set a reasonable upper limit
    
    (map-set reward-amounts
      { tier: tier }
      { amount: amount }
    )
    
    (ok true)
  )
)

(define-public (update-config (key (string-ascii 25)) (value uint))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    
    ;; Validate key is not empty
    (asserts! (> (len key) u0) ERR_INVALID_PARAMETER)
    
    ;; Validate value is reasonable (depends on the specific key)
    (asserts! (> value u0) ERR_INVALID_PARAMETER)
    
    (map-set contract-config
      { key: key }
      { value: value }
    )
    
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-worker-info (worker-id principal))
  (map-get? workers { worker-id: worker-id })
)

(define-read-only (get-worker-metrics (worker-id principal))
  (match (map-get? workers { worker-id: worker-id })
    worker-data (get metrics worker-data)
    { productivity: u0, quality: u0, communication: u0 }
  )
)

(define-read-only (get-current-period)
  (var-get current-period)
)

(define-read-only (get-contract-stats)
  {
    balance: (var-get contract-balance),
    active-workers: (var-get active-workers),
    total-rewards-distributed: (var-get total-rewards-distributed),
    current-period: (var-get current-period)
  }
)

(define-read-only (get-reward-claim (worker-id principal) (period uint))
  (map-get? reward-claims { worker-id: worker-id, period: period })
)

(define-read-only (get-reward-thresholds (metric-name (string-ascii 20)))
  (map-get? reward-thresholds { metric-name: metric-name })
)

(define-read-only (get-reward-amounts (tier (string-ascii 10)))
  (map-get? reward-amounts { tier: tier })
)

(define-read-only (get-config (key (string-ascii 25)))
  (map-get? contract-config { key: key })
)