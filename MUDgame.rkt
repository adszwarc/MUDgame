#lang racket

(require srfi/1)
(require srfi/13)

;; specifying the objects
(define objects `((1 "a short sword")
                  (1 "a shapphire")))

;; defining room descriptions
(define descriptions `((1 "You are in the chamber.")
                       (2 "You are in the corridor.")
                       (3 "You are in the great hall.")
                       (4 "You are in a storeroom.")))