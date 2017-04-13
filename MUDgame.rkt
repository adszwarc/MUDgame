#lang racket

(require srfi/1)
(require srfi/13)

;; specifying objects descriptions
(define objects '((1 "a jadeite statue")
                  (2 "a shapphire key")
                  (6 "the Holy Grail")
                  (3 "a poisoned blade")))

;; defining room descriptions
(define descriptions '((1 "You are in the Chamber.")
                       (2 "You are in the Corridor.")
                       (3 "You are in the Great Hall.")
                       (4 "You are in a Chapel.")
                       (5 "You are in a Storeroom.")
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
  
;; loading object database
(define objectdb (make-hash))
;; loading inventory database
(define inventorydb (make-hash))


;; adding objects to database
(define (add-object db id object)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons object record)))
      (hash-set! db id (cons object empty))))

;; populating the rooms with items 
(define (add-objects db)
  (for-each
   (λ (r)
     (add-object db (first r) (second r))) objects))

(add-objects objectdb)

;; displaying objects in the room and inventory
(define (display-objects db id)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
           (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'bag)
            (printf "You are carrying ~a.\n" output)
            (printf "You can see ~a.\n" output))))))

;; grabbing item from the room
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
             ;; condition for positive endgame
             (if (eq? (first item) "the Holy Grail")
                 (begin
                   (printf "\nYou have found the source of infinite wisdom and immortality!\nYOU WIN!\n")
                   (exit))
             ;; condition for negative endgame
             (if (eq? (first item) "a poisoned blade")
                 (begin
                   (printf "\nYou have cut yourself with poisoned blade!\nYou died...\nGAME OVER!\n")
                   (exit))
             (hash-set! db id result))))))))

;; removing objects from player's inventory
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

;; dropping the item
(define (pick-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-room objectdb id item)))
;; picking up the item
(define (put-item id input)
  (let ((item (string-join (cdr (string-split input)))))
    (remove-object-from-inventory inventorydb id item)))
;; displaying contents of user's inventory
(define (display-inventory)
  (display-objects inventorydb 'bag))
;; creating string by mapping the parameter to list of atoms
(define (slist->string l)
  (string-join (map symbol->string l)))
;; returning string based on id
(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))
;; generating keyword list based on id
(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

;; checking for available directions
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

;; generating keyword list based on id
(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (λ (key) (car key)) keys)))
;; returning list of weights indicating if there is a match, it maches keyword list
;; against list of tokens
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       (* (/ (length set) (length x)) (length set))))
   keylist))
;; returning the largest value in the list
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))

;; returning matching id with given id and list of tokens
(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))
;; getting description of the room with given id
(define (get-description id)
  (car (assq-ref descriptions id)))
;; displaying description
(define (display-description id)
  (printf "~a\n" (get-description id)))


;; game loop
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



