#lang racket

(define word "Sample")


(
    define (theword x)
        (string->list x)
        
)

(theword word)


(define sentence "This is a test")
(define word-list (string-split sentence))

word-list