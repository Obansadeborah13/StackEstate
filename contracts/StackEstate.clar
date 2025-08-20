;; Smart Contract: StackEstate
;; Description: Real estate tokenization with fractional shares,
;;              primary & secondary markets, fee routing,
;;              sliced rent distribution, governance & safety.
;;              primary & secondary markets, fee routing,
;;              sliced rent distribution, governance & safety.
;; Language: Clarity for Stacks
;; License: MIT
;; -------------------------------------------------------------

;; Helper functions
(define-private (min (a uint) (b uint))
  (if (<= a b) a b))

;; Distribution state
(define-data-var distribution-state
  {
    pid: uint,
    owner: principal,
    total-shares: uint,
    remaining: uint
  }
  {
    pid: u0,
    owner: contract-owner,
    total-shares: u0,
    remaining: u0
  })

;; Process distribution for a single investor
(define-private (process-single-investor (who principal) (sh uint) (info {pid: uint, owner: principal, total-shares: uint, remaining: uint}))
  (let ((portion (/ (* (get remaining info) sh) (get total-shares info))))
    (if (> portion u0)
        (match (stx-transfer? portion (get owner info) who)
          success portion
          error u0)
        u0)))

;; Process a range of investors

;; Process a range of investors
(define-private (process-range-investors (property-id uint) (start uint) (total-shares uint) (remaining uint))
  (match (map-get? properties property-id)
    prop 
      (let ((who (default-to contract-owner (map-get? investor-index {property-id: property-id, index: start}))))
        (let ((sh (default-to ZERO (map-get? property-owners {property-id: property-id, investor: who}))))
          (let ((portion (/ (* remaining sh) total-shares)))
            ;; Handle transfer if needed
            (if (> portion ZERO)
                (match (stx-transfer? portion (get owner prop) who)
                  success-transfer
                    {paid: portion, next-index: (+ start ONE)}
                  error-transfer
                    {paid: u0, next-index: (+ start ONE)})
                ;; No transfer needed, continue to next
                {paid: u0, next-index: (+ start ONE)}))))
    {paid: u0, next-index: (+ start ONE)}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Traits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-trait nft-trait
  (
    (transfer (uint principal principal) (response bool uint))
    (get-owner (uint) (response principal uint))
    (get-last-token-id () (response uint uint))
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants & Errors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant ERR-NOT-OWNER        (err u401))
(define-constant ERR-UNAUTHORIZED     (err u402))
(define-constant ERR-BAD-REQ          (err u400))
(define-constant ERR-NOT-FOUND        (err u404))
(define-constant ERR-NOT-FOR-SALE     (err u405))
(define-constant ERR-INSUFFICIENT     (err u406))
(define-constant ERR-NO-RENT          (err u407))
(define-constant ERR-PAUSED           (err u408))
(define-constant ERR-LISTING-CLOSED   (err u409))
(define-constant ERR-INVALID-STATE    (err u410))
(define-constant ERR-DUP              (err u411))

;; platform config
(define-data-var fee-bps uint u200)                 ;; 2.00% platform fee
(define-data-var treasury principal tx-sender)      ;; protocol treasury

;; investor registry constraints
(define-constant MAX-INVESTORS-PER-PROP u2000)      ;; hard upper bound
(define-constant ONE u1)
(define-constant ZERO u0)

;; pause switch
(define-data-var paused bool false)

;; contract owner (deployer)
(define-constant contract-owner tx-sender)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Admin / Auth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-map operators principal bool)

(define-private (only-owner) 
  (ok (asserts! (is-eq tx-sender contract-owner) ERR-UNAUTHORIZED)))

(define-private (only-operator-or-owner)
  (ok (asserts! (or (is-eq tx-sender contract-owner)
                    (default-to false (map-get? operators tx-sender)))
                ERR-UNAUTHORIZED)))

(define-public (set-operator (who principal) (is-op bool))
  (begin
    (try! (only-owner))
    (map-set operators who is-op)
    (ok true)
  ))

(define-public (set-fee-bps (new-bps uint))
  (begin
    (try! (only-owner))
    (asserts! (<= new-bps u1000) ERR-BAD-REQ) ;; cap at 10%
    (var-set fee-bps new-bps)
    (ok new-bps)
  ))

(define-public (set-treasury (to principal))
  (begin
    (try! (only-owner))
    (var-set treasury to)
    (ok to)
  ))

(define-public (set-paused (state bool))
  (begin
    (try! (only-operator-or-owner))
    (var-set paused state)
    (ok state)
  ))

(define-private (when-active)
  (ok (asserts! (not (var-get paused)) ERR-PAUSED)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Property NFT ownership (deeded property controller)
(define-map token-owners uint principal)

;; Property metadata & sale config
(define-map properties
  uint
  {
    name: (string-ascii 50),
    location: (string-ascii 100),
    total-shares: uint,
    available-shares: uint,
    price-per-share: uint,
    min-chunk: uint,                   ;; minimum shares per buy
    owner: principal,
    for-sale: bool,
    active: bool
  }
)

;; Primary share ledger
(define-map property-owners
  {property-id: uint, investor: principal}
  uint
)

;; Investor registry (for distribution / pagination)
(define-map investor-index
  {property-id: uint, index: uint}     ;; index -> principal
  principal
)
(define-map investor-position
  {property-id: uint, investor: principal} ;; principal -> index
  uint
)
(define-map investor-count
  uint                                 ;; property-id
  uint                                 ;; count
)

;; Secondary listings
(define-data-var last-listing-id uint u0)
(define-map listings
  uint
  {
    property-id: uint,
    seller: principal,
    shares: uint,
    price-per-share: uint,
    active: bool
  }
)

;; Optional: bookkeeping for rent distributions (slice mode)
(define-map dist-snapshot
  uint
  {
    open: bool,
    total-shares: uint,
    remaining: uint,          ;; remaining STX (for reference)
    last-idx: uint            ;; last processed investor index
  }
)

;; ID counter
(define-data-var last-property-id uint u0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NFT Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-read-only (get-owner (token-id uint))
  (match (map-get? token-owners token-id)
    owner (ok owner)
    ERR-NOT-FOUND))

(define-read-only (get-last-token-id)
  (ok (var-get last-property-id)))

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-UNAUTHORIZED)
    (match (map-get? token-owners token-id)
      current
        (begin
          (asserts! (is-eq current sender) ERR-UNAUTHORIZED)
          (map-set token-owners token-id recipient)
          ;; also move controller for the property
          (match (map-get? properties token-id)
            some-prop
              (begin
                (map-set properties token-id (merge some-prop { owner: recipient }))
                (ok true))
            ERR-NOT-FOUND))
      ERR-NOT-FOUND)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils: investor registry handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-private (is-zero (x uint)) (is-eq x ZERO))

(define-private (ensure-investor-in-registry (pid uint) (who principal))
  (let (
        (cur-sh (default-to ZERO (map-get? property-owners {property-id: pid, investor: who})))
       )
    (if (> cur-sh ZERO)
        (ok true)
        (let (
              (cnt (default-to ZERO (map-get? investor-count pid)))
             )
          (begin
            (asserts! (< cnt MAX-INVESTORS-PER-PROP) ERR-BAD-REQ)
            (map-set investor-index {property-id: pid, index: cnt} who)
            (map-set investor-position {property-id: pid, investor: who} cnt)
            (map-set investor-count pid (+ cnt ONE))
            (ok true))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Property Lifecycle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (mint-property
  (name (string-ascii 50))
  (location (string-ascii 100))
  (total-shares uint)
  (price-per-share uint)
  (min-chunk uint)
)
  (begin
    (try! (when-active))
    (asserts! (> total-shares ZERO) ERR-BAD-REQ)
    (asserts! (> price-per-share ZERO) ERR-BAD-REQ)
    (asserts! (> min-chunk ZERO) ERR-BAD-REQ)
    (let ((new-id (+ (var-get last-property-id) ONE)))
      (begin
        (var-set last-property-id new-id)
        (map-set token-owners new-id tx-sender)
        (map-set properties new-id {
          name: name,
          location: location,
          total-shares: total-shares,
          available-shares: total-shares,
          price-per-share: price-per-share,
          min-chunk: min-chunk,
          owner: tx-sender,
          for-sale: true,
          active: true
        })
        (ok new-id)))))

(define-public (set-for-sale (property-id uint) (status bool))
  (let ((prop (map-get? properties property-id)))
    (match prop
      p (begin
          (asserts! (is-eq tx-sender (get owner p)) ERR-NOT-OWNER)
          (map-set properties property-id (merge p {for-sale: status}))
          (ok true))
      ERR-NOT-FOUND)))

(define-public (deactivate-property (property-id uint) (status bool))
  (let ((p (map-get? properties property-id)))
    (match p
      prop (begin
             (asserts! (is-eq tx-sender (get owner prop)) ERR-NOT-OWNER)
             (map-set properties property-id (merge prop {active: (not status)}))
             (ok true))
      ERR-NOT-FOUND)))

(define-public (update-price (property-id uint) (new-price uint))
  (let ((p (map-get? properties property-id)))
    (match p
      prop (begin
             (asserts! (is-eq tx-sender (get owner prop)) ERR-NOT-OWNER)
             (asserts! (> new-price ZERO) ERR-BAD-REQ)
             (map-set properties property-id (merge prop {price-per-share: new-price}))
             (ok new-price))
      ERR-NOT-FOUND)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary Sale: buy shares from property
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (buy-shares (property-id uint) (num-shares uint))
  (begin
    (try! (when-active))
    (asserts! (> num-shares ZERO) ERR-BAD-REQ)
    (let ((prop (map-get? properties property-id)))
      (match prop
        p (begin
            (asserts! (get active p) ERR-INVALID-STATE)
            (asserts! (get for-sale p) ERR-NOT-FOR-SALE)
            (asserts! (>= num-shares (get min-chunk p)) ERR-BAD-REQ)
            (let (
                  (avail (get available-shares p))
                  (pps (get price-per-share p))
                  (feeBps (var-get fee-bps))
                  (treas (var-get treasury))
                  (seller (get owner p))
                  (gross (* pps num-shares))
                  (fee (/ (* gross feeBps) u10000))
                  (net (- gross fee))
                 )
              (begin
                (asserts! (<= num-shares avail) ERR-INSUFFICIENT)
                ;; transfer STX: buyer -> seller and treasury
                (try! (stx-transfer? net tx-sender seller))
                (try! (if (> fee ZERO)
                         (stx-transfer? fee tx-sender treas)
                         (ok true)))
                ;; update supply
                (map-set properties property-id (merge p {available-shares: (- avail num-shares)}))
                ;; ledger & registry
                (try! (ensure-investor-in-registry property-id tx-sender))
                (let ((old (default-to ZERO (map-get? property-owners {property-id: property-id, investor: tx-sender}))))
                  (map-set property-owners {property-id: property-id, investor: tx-sender} (+ old num-shares))
                  (ok true)))))
        ERR-NOT-FOUND))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secondary Market: peer-to-peer listings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (list-shares (property-id uint) (shares uint) (price-per-share uint))
  (begin
    (try! (when-active))
    (asserts! (> shares ZERO) ERR-BAD-REQ)
    (asserts! (> price-per-share ZERO) ERR-BAD-REQ)
    (let ((prop (map-get? properties property-id)))
      (match prop
        p (begin
            (asserts! (get active p) ERR-INVALID-STATE)
            (let ((owned (default-to ZERO (map-get? property-owners {property-id: property-id, investor: tx-sender}))))
              (begin
                (asserts! (>= owned shares) ERR-INSUFFICIENT)
                (let ((lid (+ (var-get last-listing-id) ONE)))
                  (begin
                    (var-set last-listing-id lid)
                    ;; lock shares: reduce available balance of seller
                    (map-set property-owners {property-id: property-id, investor: tx-sender} (- owned shares))
                    (map-set listings lid {
                      property-id: property-id,
                      seller: tx-sender,
                      shares: shares,
                      price-per-share: price-per-share,
                      active: true
                    })
                    (ok lid))))))
        ERR-NOT-FOUND))))

(define-public (cancel-listing (listing-id uint))
  (let ((li (map-get? listings listing-id)))
    (match li
      l (begin
          (asserts! (is-eq (get seller l) tx-sender) ERR-UNAUTHORIZED)
          (asserts! (get active l) ERR-LISTING-CLOSED)
          ;; unlock shares
          (let ((owned (default-to ZERO (map-get? property-owners {property-id: (get property-id l), investor: tx-sender}))))
            (map-set property-owners {property-id: (get property-id l), investor: tx-sender} (+ owned (get shares l))))
          (map-set listings listing-id (merge l {active: false}))
          (ok true))
      ERR-NOT-FOUND)))

(define-public (buy-from-listing (listing-id uint) (num-shares uint))
  (begin
    (try! (when-active))
    (asserts! (> num-shares ZERO) ERR-BAD-REQ)
    (let ((li (map-get? listings listing-id)))
      (match li
        l (let ((prop (map-get? properties (get property-id l))))
            (match prop
              p (begin
                  (asserts! (get active p) ERR-INVALID-STATE)
                  (asserts! (get active l) ERR-LISTING-CLOSED)
                  (asserts! (<= num-shares (get shares l)) ERR-INSUFFICIENT)
                  (let ((pps (get price-per-share l))
                        (seller (get seller l))
                        (pid (get property-id l))
                        (gross (* pps num-shares))
                        (fee (/ (* gross (var-get fee-bps)) u10000))
                        (net (- gross fee)))
                    ;; First, try to transfer the payment
                    (try! (stx-transfer? net tx-sender seller))
                    (try! (if (> fee ZERO)
                            (stx-transfer? fee tx-sender (var-get treasury))
                            (ok true)))
                    ;; After successful payment, update the listing
                    (let ((remaining (- (get shares l) num-shares)))
                      (map-set listings listing-id 
                        (merge l {shares: remaining, active: (is-zero remaining)}))
                      ;; Finally, credit the shares to the buyer
                      (try! (ensure-investor-in-registry pid tx-sender))
                      (let ((old (default-to ZERO 
                                 (map-get? property-owners 
                                   {property-id: pid, investor: tx-sender}))))
                        (map-set property-owners 
                          {property-id: pid, investor: tx-sender}
                          (+ old num-shares))
                        (ok true)))))
              ERR-NOT-FOUND))
        ERR-NOT-FOUND))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buyback (owner retires shares, increases scarcity)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (owner-buyback (property-id uint) (from principal) (shares uint) (price-per-share uint))
  (begin
    (try! (when-active))
    (asserts! (> shares ZERO) ERR-BAD-REQ)
    (let ((p (map-get? properties property-id)))
      (match p
        prop (begin
               (asserts! (is-eq tx-sender (get owner prop)) ERR-NOT-OWNER)
               (let ((held (default-to ZERO (map-get? property-owners {property-id: property-id, investor: from}))))
                 (asserts! (>= held shares) ERR-INSUFFICIENT)
                 (let ((amount (* shares price-per-share)))
                   ;; pay investor
                   (try! (stx-transfer? amount tx-sender from))
                   ;; burn shares by reducing total-shares (and do not re-add to available)
                   (map-set property-owners {property-id: property-id, investor: from} (- held shares))
                   (map-set properties property-id (merge prop {
                     total-shares: (- (get total-shares prop) shares)
                   }))
                   (ok true))))
        ERR-NOT-FOUND))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rent Distribution (slice mode: transfers from property owner)
;;
;; Pattern:
;; 1) owner calls begin-distribution(pid, total-rent)
;; 2) anyone calls distribute-slice(pid, count) multiple times
;;    - funds move from property owner to each investor pro-rata
;; 3) when all investors processed, distribution auto-closes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (begin-distribution (property-id uint) (total-rent uint))
  (begin
    (asserts! (> total-rent ZERO) ERR-BAD-REQ)
    (let (
          (p (map-get? properties property-id))
          (cnt (default-to ZERO (map-get? investor-count property-id)))
         )
      (match p
        prop (begin
               (asserts! (is-eq tx-sender (get owner prop)) ERR-NOT-OWNER)
               (asserts! (> (get total-shares prop) ZERO) ERR-BAD-REQ)
               (map-set dist-snapshot property-id {
                 open: true,
                 total-shares: (get total-shares prop),
                 remaining: total-rent,
                 last-idx: ZERO
               })
               ;; NOTE: no custody taken; actual transfers occur in slices
               (ok {investors: cnt, total-shares: (get total-shares prop)}))
        ERR-NOT-FOUND))))

(define-public (distribute-slice (property-id uint) (max-steps uint))
  (let ((snap (map-get? dist-snapshot property-id))
        (cnt (default-to ZERO (map-get? investor-count property-id))))
    (match snap
      s (begin
          (asserts! (get open s) ERR-INVALID-STATE)
          (let ((start (get last-idx s))
                (end (min cnt (+ (get last-idx s) max-steps)))
                (total (get total-shares s))
                (remaining0 (get remaining s)))
            (asserts! (> max-steps ZERO) ERR-BAD-REQ)
            (asserts! (> total ZERO) ERR-BAD-REQ)
            (var-set distribution-state 
              {
                pid: property-id,
                owner: tx-sender,
                total-shares: total,
                remaining: remaining0
              })
            ;; Process each investor in range
            (let ((current start)
                  (total-paid u0))
              (begin
                (let ((info (var-get distribution-state)))
                  (let ((who (default-to contract-owner 
                              (map-get? investor-index 
                                {property-id: (get pid info), index: current}))))
                    (let ((sh (default-to u0 
                              (map-get? property-owners 
                                {property-id: (get pid info), investor: who}))))
                      (let ((portion (process-single-investor who sh info)))
                        (begin
                          ;; Update snapshot
                          (map-set dist-snapshot property-id 
                            {
                              open: (< (+ current u1) cnt),
                              total-shares: total,
                              remaining: (if (> remaining0 portion) 
                                           (- remaining0 portion) 
                                           ZERO),
                              last-idx: (+ current u1)
                            })
                          (ok {
                            paid: portion,
                            next-index: (+ current u1),
                            done: (is-eq (+ current u1) cnt)
                          }))))))))))
      ERR-NOT-FOUND)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Views
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; View functions
(define-read-only (get-property (property-id uint))
  (map-get? properties property-id))

(define-read-only (get-shares (property-id uint) (owner principal))
  (default-to ZERO (map-get? property-owners {property-id: property-id, investor: owner})))

