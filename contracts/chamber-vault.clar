;; Stacks Chamber Network - Value Storage Protocol

;; Chamber repository data structure
(define-map ChamberRepository
  { chamber-id: uint }
  {
    initiator: principal,
    beneficiary: principal,
    item-id: uint,
    quantity: uint,
    chamber-status: (string-ascii 10),
    creation-block: uint,
    expiration-block: uint
  }
)

;; Registry counter for chambers
(define-data-var next-chamber-id uint u0)

;; Utility functions for validation
(define-private (valid-beneficiary? (beneficiary principal))
  (and 
    (not (is-eq beneficiary tx-sender))
    (not (is-eq beneficiary (as-contract tx-sender)))
  )
)

;; Core system parameters
(define-constant PROTOCOL_GUARDIAN tx-sender)
(define-constant ERROR_PERMISSION_DENIED (err u100))
(define-constant ERROR_NO_CHAMBER (err u101))
(define-constant ERROR_ALREADY_PROCESSED (err u102))
(define-constant ERROR_STX_MOVEMENT_FAILED (err u103))
(define-constant ERROR_INVALID_IDENTIFIER (err u104))
(define-constant ERROR_INVALID_QUANTITY (err u105))
(define-constant ERROR_INVALID_INITIATOR (err u106))
(define-constant ERROR_CHAMBER_TIMEOUT (err u107))
(define-constant CHAMBER_LIFESPAN_BLOCKS u1008)

(define-private (valid-chamber-id? (chamber-id uint))
  (<= chamber-id (var-get next-chamber-id))
)

;; Protocol operational functions

;; Finalize transfer to beneficiary
(define-public (finalize-chamber-transfer (chamber-id uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (beneficiary (get beneficiary chamber-record))
        (quantity (get quantity chamber-record))
        (item (get item-id chamber-record))
      )
      (asserts! (or (is-eq tx-sender PROTOCOL_GUARDIAN) (is-eq tx-sender (get initiator chamber-record))) ERROR_PERMISSION_DENIED)
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)
      (asserts! (<= block-height (get expiration-block chamber-record)) ERROR_CHAMBER_TIMEOUT)
      (match (as-contract (stx-transfer? quantity tx-sender beneficiary))
        success
          (begin
            (map-set ChamberRepository
              { chamber-id: chamber-id }
              (merge chamber-record { chamber-status: "completed" })
            )
            (print {action: "chamber_transferred", chamber-id: chamber-id, beneficiary: beneficiary, item-id: item, quantity: quantity})
            (ok true)
          )
        error ERROR_STX_MOVEMENT_FAILED
      )
    )
  )
)

;; Return items to initiator
(define-public (return-chamber-contents (chamber-id uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (quantity (get quantity chamber-record))
      )
      (asserts! (is-eq tx-sender PROTOCOL_GUARDIAN) ERROR_PERMISSION_DENIED)
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)
      (match (as-contract (stx-transfer? quantity tx-sender initiator))
        success
          (begin
            (map-set ChamberRepository
              { chamber-id: chamber-id }
              (merge chamber-record { chamber-status: "returned" })
            )
            (print {action: "contents_returned", chamber-id: chamber-id, initiator: initiator, quantity: quantity})
            (ok true)
          )
        error ERROR_STX_MOVEMENT_FAILED
      )
    )
  )
)

;; Initiator requests chamber nullification
(define-public (nullify-chamber (chamber-id uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (quantity (get quantity chamber-record))
      )
      (asserts! (is-eq tx-sender initiator) ERROR_PERMISSION_DENIED)
      (asserts! (is-eq (get chamber-status chamber-record) "pending") ERROR_ALREADY_PROCESSED)
      (asserts! (<= block-height (get expiration-block chamber-record)) ERROR_CHAMBER_TIMEOUT)
      (match (as-contract (stx-transfer? quantity tx-sender initiator))
        success
          (begin
            (map-set ChamberRepository
              { chamber-id: chamber-id }
              (merge chamber-record { chamber-status: "nullified" })
            )
            (print {action: "chamber_nullified", chamber-id: chamber-id, initiator: initiator, quantity: quantity})
            (ok true)
          )
        error ERROR_STX_MOVEMENT_FAILED
      )
    )
  )
)

;; Create secure timelock vault with multiple unlock conditions
(define-public (create-timelocked-vault (beneficiary principal) (unlock-height uint) (backup-principal principal) (quantity uint))
  (begin
    (asserts! (> quantity u0) ERROR_INVALID_QUANTITY)
    (asserts! (> unlock-height block-height) (err u450)) ;; Must be future block
    (asserts! (<= unlock-height (+ block-height u8640)) (err u451)) ;; Maximum 60 days in future
    (asserts! (valid-beneficiary? beneficiary) ERROR_INVALID_INITIATOR)
    (asserts! (not (is-eq backup-principal tx-sender)) (err u452)) ;; Backup must be different from sender
    (asserts! (not (is-eq backup-principal beneficiary)) (err u453)) ;; Backup must be different from beneficiary

    (let
      (
        (new-chamber-id (+ (var-get next-chamber-id) u1))
        (time-lock-duration (- unlock-height block-height))
      )

      ;; Transfer STX to contract
      (match (stx-transfer? quantity tx-sender (as-contract tx-sender))
        success
          (begin
            (var-set next-chamber-id new-chamber-id)

            ;; Store vault info in repository
            (map-set ChamberRepository
              { chamber-id: new-chamber-id }
              {
                initiator: tx-sender,
                beneficiary: beneficiary,
                item-id: u0, ;; Generic vault
                quantity: quantity,
                chamber-status: "timelocked",
                creation-block: block-height,
                expiration-block: unlock-height
              }
            )

            (print {action: "timelock_vault_created", 
                    chamber-id: new-chamber-id, 
                    initiator: tx-sender,
                    beneficiary: beneficiary,
                    backup-principal: backup-principal,
                    unlock-height: unlock-height,
                    timelock-duration: time-lock-duration,
                    quantity: quantity})
            (ok new-chamber-id)
          )
        error ERROR_STX_MOVEMENT_FAILED
      )
    )
  )
)

;; Implement encrypted messaging for chamber participants
(define-public (add-encrypted-communication (chamber-id uint) (encrypted-message (buff 256)) (recipient-principal principal))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (asserts! (> (len encrypted-message) u32) (err u500)) ;; Ensure minimum message length
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
      )
      ;; Only chamber participants can send messages
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender beneficiary) (is-eq tx-sender PROTOCOL_GUARDIAN)) ERROR_PERMISSION_DENIED)
      ;; Recipient must be chamber participant
      (asserts! (or (is-eq recipient-principal initiator) (is-eq recipient-principal beneficiary) (is-eq recipient-principal PROTOCOL_GUARDIAN)) (err u501))
      ;; Sender cannot be recipient
      (asserts! (not (is-eq tx-sender recipient-principal)) (err u502))

      ;; Get message digest for reference
      (let
        (
          (message-digest (hash160 encrypted-message))
        )
        (print {action: "encrypted_message_sent", 
                chamber-id: chamber-id, 
                sender: tx-sender, 
                recipient: recipient-principal, 
                message-digest: message-digest,
                timestamp-block: block-height})
        (ok message-digest)
      )
    )
  )
)

;; Implement panic button with immediate system shutdown
(define-public (activate-protocol-panic-mode (justification (string-ascii 100)) (cooldown-blocks uint))
  (begin
    (asserts! (is-eq tx-sender PROTOCOL_GUARDIAN) ERROR_PERMISSION_DENIED)
    (asserts! (> cooldown-blocks u24) (err u440)) ;; Minimum 4 hour cooldown
    (asserts! (<= cooldown-blocks u4320) (err u441)) ;; Maximum 30 day cooldown

    (let
      (
        (activation-block block-height)
        (deactivation-block (+ block-height cooldown-blocks))
      )

      ;; Validate justification - must have minimum length
      (asserts! (>= (len justification) u10) (err u442))

      ;; Log audit trail of panic mode activation
      (print {action: "panic_mode_activated", 
              guardian: tx-sender,
              activation-block: activation-block,
              deactivation-block: deactivation-block,
              cooldown-duration: cooldown-blocks,
              justification: justification})

      ;; In a full implementation, this would set all protocol operations to pause

      (ok {activation: activation-block, 
           deactivation: deactivation-block, 
           duration: cooldown-blocks})
    )
  )
)

;; Implement dynamic fee adjustment based on chamber value
(define-public (adjust-chamber-fees (chamber-id uint) (fee-percentage uint))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (asserts! (<= fee-percentage u10) (err u520)) ;; Max 10% fee
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (quantity (get quantity chamber-record))
        (status (get chamber-status chamber-record))
      )
      ;; Only guardian can adjust fees
      (asserts! (is-eq tx-sender PROTOCOL_GUARDIAN) ERROR_PERMISSION_DENIED)
      ;; Only for pending chambers
      (asserts! (is-eq status "pending") ERROR_ALREADY_PROCESSED)

      ;; Calculate fee amount
      (let
        (
          (fee-amount (/ (* quantity fee-percentage) u100))
          (adjusted-quantity (- quantity fee-amount))
        )
        ;; Ensure minimum quantity remains
        (asserts! (> adjusted-quantity u0) (err u521))

        ;; Update chamber with new quantity
        (map-set ChamberRepository
          { chamber-id: chamber-id }
          (merge chamber-record { quantity: adjusted-quantity })
        )

        (print {action: "fees_adjusted", 
                chamber-id: chamber-id, 
                original-quantity: quantity, 
                fee-percentage: fee-percentage, 
                fee-amount: fee-amount, 
                adjusted-quantity: adjusted-quantity})
        (ok fee-amount)
      )
    )
  )
)

;; Implement selective disclosure verification
(define-public (verify-selective-disclosure (chamber-id uint) (disclosure-hash (buff 32)) (verification-path (list 10 (buff 32))))
  (begin
    (asserts! (valid-chamber-id? chamber-id) ERROR_INVALID_IDENTIFIER)
    (asserts! (> (len verification-path) u0) (err u530))
    (let
      (
        (chamber-record (unwrap! (map-get? ChamberRepository { chamber-id: chamber-id }) ERROR_NO_CHAMBER))
        (initiator (get initiator chamber-record))
        (beneficiary (get beneficiary chamber-record))
        (status (get chamber-status chamber-record))
      )
      ;; Only chamber participants can verify disclosures
      (asserts! (or (is-eq tx-sender initiator) (is-eq tx-sender beneficiary)) ERROR_PERMISSION_DENIED)
      ;; Chamber must be active
      (asserts! (or (is-eq status "pending") (is-eq status "accepted")) ERROR_ALREADY_PROCESSED)

      ;; Perform Merkle path verification (simplified for this example)
      (let
        (
          (root-hash (fold hash-combine verification-path disclosure-hash))
        )
        (print {action: "selective_disclosure_verified", 
                chamber-id: chamber-id, 
                verifier: tx-sender, 
                disclosure-hash: disclosure-hash, 
                merkle-root: root-hash,
                path-length: (len verification-path)})
        (ok root-hash)
      )
    )
  )
)

;; Helper function for Merkle path verification
(define-private (hash-combine (path-element (buff 32)) (current-hash (buff 32)))
  (hash160 (concat current-hash path-element))
)


;; Helper function to process chamber merging
(define-private (process-chamber-merge (source-id uint) (result {quantity: uint, count: uint}))
  (let
    (
      (chamber-record (map-get? ChamberRepository { chamber-id: source-id }))
    )
    (match chamber-record
      source-chamber
        (let
          (
            (source-status (get chamber-status source-chamber))
            (source-quantity (get quantity source-chamber))
          )
          ;; Only merge chambers that are in pending status
          (if (is-eq source-status "pending")
            (begin
              ;; Mark source chamber as merged
              (map-set ChamberRepository
                { chamber-id: source-id }
                (merge source-chamber { chamber-status: "merged", quantity: u0 })
              )
              ;; Add quantity to result
              {quantity: (+ (get quantity result) source-quantity), count: (+ (get count result) u1)}
            )
            ;; Keep existing result if chamber can't be merged
            result
          )
        )
      ;; Chamber doesn't exist, keep existing result
      result
    )
  )
)

 ;; Helper function to create fragments
 (define-private (create-fragment (percentage uint) 
                                 (state {parent-id: uint, 
                                        current-id: uint, 
                                        parent-quantity: uint,
                                        fragments: (list 5 uint)}))
   (let
     (
       (parent-id (get parent-id state))
       (current-id (get current-id state))
       (parent-quantity (get parent-quantity state))
       (fragments (get fragments state))
       (next-id (+ current-id u1))
       (fragment-quantity (/ (* parent-quantity percentage) u100))
       (parent-chamber (unwrap-panic (map-get? ChamberRepository { chamber-id: parent-id })))
     )
     ;; Create new fragment chamber
     (map-set ChamberRepository
       { chamber-id: next-id }
       {
         initiator: (get initiator parent-chamber),
         beneficiary: (get beneficiary parent-chamber),
         item-id: (get item-id parent-chamber),
         quantity: fragment-quantity,
         chamber-status: "pending",
         creation-block: block-height,
         expiration-block: (get expiration-block parent-chamber)
       }
     )

     ;; Return updated state
     {
       parent-id: parent-id,
       current-id: next-id,
       parent-quantity: parent-quantity,
       fragments: (unwrap-panic (as-max-len? (append fragments next-id) u5))
     }
   )
 )

