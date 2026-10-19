#lang racket

(define word "Sample")

(
    define (theword x)
        (
            let([char-list (string->list x)])
            (
                if (empty? char-list)
                `()
                (list->string(append (rest char-list) (list (first char-list))))
            )
        )
)
(theword word)


(define sentence "This is a test")
(define word-list (string-split sentence))
word-list
(map string-length word-list)