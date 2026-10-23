#lang racket

(define word "Sample")

(
    define (theword x k)
        (
            ;; turn a word into a list of letters
            let([char-list (string->list x)])
            (
                ;;empty return nothing
                if (empty? char-list)
                ""
                (
                    ;;using modulo help k stay in range
                    let ([rot (modulo k (length char-list))])
                    ;;drop: remove the first letter. take: put letter at the back 
                    (list->string (append (drop char-list rot) (take char-list rot)))
                )
            )
        )
)


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
                            [k (cons 1 (take word-lengths(sub1 len)))]
                    ;;using map use the following values of 'k' 'x' for k % x         
                    [shift (map (lambda (k x) (modulo k x)) k word-lengths)])

                    (string-join (map theword word-list shift))
                )
            )
    )
)

(rotate "hello")
(rotate "The quick brown fox")
(rotate "University of Lethbridge") ;;returns "Niversityu of thbridgele"
(rotate "I am a cat") ;;returns "I ma a atc"
(rotate "Racket is fun") ;;returns "Acketr is nfu"