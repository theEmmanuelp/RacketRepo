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


(
    define (rotate sen)
    (
        let ([word-list (string-split sen)])
            (map string-length word-list)
    )
)

(rotate "hello")
(rotate "The quick brown fox")