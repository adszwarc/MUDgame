#lang racket

(require srfi/1)
(require srfi/13)

;; specifying objects descriptions
(define objects '((1 "a short sword")
                  (1 "a shapphire key")))

;; defining room descriptions
(define descriptions '((1 "You are in the chamber.")
                       (2 "You are in the corridor.")
                       (3 "You are in the great hall.")
                       (4 "You are in a chapel.")
                       (5 "You are in a storeroom.")
                       (6 "You are in secret room.")))

;; defining actions
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define drop '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define actions `(,@look ,@quit ,@pick ,@drop ,@inventory))

;; defining decisiontable
(define decisiontable `((1 ((east) 2) ,@actions)
                        (2 ((west) 1) ((east) 2) ,@actions)
                        (3 ((west) 2) ((north) 5) ((south) 4) ,@actions)))
  
;; load object database
(define objectdb (make-hash))
;; load inventory database
(define inventorydb (make-hash))



(define (add-object db id object)
  (if (hash-has-hey? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

(define (add-objects db)
  (for-each
   (λ (r)
     (add-object db (first r) (second))) objects))

(add-objects objectdb)

