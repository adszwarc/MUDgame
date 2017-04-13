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
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

(define (add-objects db)
  (for-each
   (λ (r)
     (add-object db (first r) (second r))) objects))

(add-objects objectdb)

(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))

(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (result (remove (λ (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "I don't see that item in the room!\n"))
            (else
             (printf "Added ~a to your bag.\n" (first item))
             (add-object inventorydb 'bag (first item))
             (hash-set! db id result))))))

(define (remove-object-from-inventory db id str)
  (when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
      (cond ((null? item)
             (printf "You are not carrying that item!\n"))
            (else
             (printf "Removed ~a from your bag.\n" (first item))
             (add-object objectdb id (first item))
             (hash-set! db 'bag result))))))

(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))

(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))

(define (display-inventory)
  (display-objects inventorydb 'bag))

