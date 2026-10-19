#lang racket

(define word "Sample")

(
    define (theword x)
        (string->list x)
        (
            if (empty? x)
            `()
            (append (rest x) (list (first x)))
        )        
)
(theword word)


(define sentence "This is a test")
(define word-list (string-split sentence))
word-list
(map string-length word-list)