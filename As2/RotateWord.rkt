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
    (;;this let will help initailly split the sentence into a list
        let ([word-list (string-split sen)])
            ;;first find the length of all words 
            ;;for the remainder formula to rotate word
            (map string-length word-list)
            (map (lambda (x) (+ (string-length x) 1)) word-list)
    )
)

(rotate "hello")
(rotate "The quick brown fox")