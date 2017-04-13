#lang racket

(require srfi/1)
(require srfi/13)

;; specifying objects descriptions
(define objects '((1 "a ruby key")
                  (2 "a shapphire key")
                  (6 "the Holy Grail")))

;; defining room descriptions
(define descriptions '((1 "You are in the chamber.")
                       (2 "You are in the corridor.")
                       (3 "You are in the great hall.")
                       (4 "You are in a chapel.")
                       (5 "You are in a storeroom.")
                       (6 "You are in the Treasury.")))

;; defining actions
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define pick '(((get) pick) ((pickup) pick) ((pick) pick)))
(define drop '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define inventory '(((inventory) inventory) ((bag) inventory)))
(define actions `(,@look ,@quit ,@pick ,@drop ,@inventory))

;; defining decisiontable
(define decisiontable `((1 ((east) 2) ,@actions)
                        (2 ((west) 1) ((east) 3) ,@actions)
                        (3 ((west) 2) ((north) 5) ((south) 4) ((east) 6) ,@actions)
                        (4 ((north) 3) ,@actions)
                        (5 ((south) 3) ,@actions)
                        (6 ,@actions)))
  
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
             (if (eq? (first item) "the Holy Grail")
                 (begin
                   (printf "\nYou have found the source of infinite wisdom and immortality!\n")
                   (exit))
             (hash-set! db id result)))))))

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

(define (slist->string l)
  (string-join (map symbol->string l)))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))


(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (λ (key) (car key)) keys)))

(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))

(define (get-description id)
  (car (assq-ref descriptions id)))

(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))

(define (display-description id)
  (printf "~a\n" (get-description id)))



(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (when description
      (display-description id)
      (display-objects objectdb id))
    (printf "> ")
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (printf "huh? I didn't get it!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #t))
              ((eq? response 'pick)
               (pick-item id input)
               (loop id #f))
              ((eq? response 'drop)
               (put-item id input)
               (loop id #f))
              ((eq? response 'inventory)
               (display-inventory)
               (loop id #f))              
              ((eq? response 'quit)
               (printf "Good bye!\n")
               (exit)))))))



