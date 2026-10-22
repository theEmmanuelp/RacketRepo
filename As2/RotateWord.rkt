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
        let* ([word-list (string-split sen)]
                [len (length word-list)])
            ;;first find the length of all words 
            ;;for the remainder formula to rotate word
            (
                if (= len 0)
                ;; if empty return empty
                `()
                (;;first find the length of each word 'x'
                    let* ([word-lengths (map string-length word-list)]
                    ;;find the 'k' value by shifting to the left of the list 
                            [k (cons 1 (take word-lengths(sub1 len)))])
                    ;;using map use the following values of 'k' 'x' for k % x         
                    (map (lambda (k x) (modulo k x)) k word-lengths)
                )
            )
    )
)

(rotate "hello")
(rotate "The quick brown fox")