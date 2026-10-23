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
(rotate "made by Emmanuel")

(define symbols "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(define codes
 (string-append
 ".- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- "
 "-. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. "
 "----- .---- ..--- ...-- ....- ..... -.... --... ---.. ----."))

;; Build the lookup hash table dynamically using map and lambda.
;; This zips the split codes list with the split characters list in parallel.
(
    define morse-hash
    (
        make-immutable-hash
        (
            map (lambda (c s) (cons c s))
                (string-split codes)
                (map string (string->list symbols))
        )
    )
)

;; Decode a single token (handles optional *n multipliers and unknown codes)
(
    define (decode-token token)
    (
        let* ([parts (string-split token "*")]
              [code (car parts)]
              ;; If there's a second part after *, convert it to a number; otherwise default to 1
              [n (if (> (length parts) 1)
                     (string->number (cadr parts))
                     1)]
              ;; Look up code in the hash table; default to "?" if not found
              [letter (hash-ref morse-hash code "?")])
        
        ;; Repeat the resulting letter string 'n' times without loops or recursion
        (apply string-append (build-list n (lambda (_) letter)))
    )
)


;; Decode a single word (tokens separated by spaces)
(
    define (decode-word word-str)
    (
        let ([tokens (string-split word-str)])
        (
            ;; Join the decoded letters together with no spaces between them
            string-join (map decode-token tokens) ""
        )
    )
)

;; Main decode function (words separated by "/")
(
    define (decode s)
    (
        ;; Split the sentence into words using a regular expression (#rx):
        ;; - \\s* matches zero or more whitespace characters (spaces, tabs)
        ;; - / matches the literal slash character used as the word separator
        ;; - \\s* matches any trailing whitespace after the slash
        ;; This ensures clean splitting regardless of uneven spacing around the slashes.
        let ([words (string-split s #rx"\\s*/\\s*")])
        (
            ;; Decode each word, then join them back together with a space between words
            string-join (map decode-word words) " "
        )
    )
)

(decode "... --- ...") ;;returns "SOS"
(decode "- .... . / -... .-. --- .-- -. / ..-. --- -..-") ;;returns "THE BROWN FOX"
(decode "-.*2 --- / .--*3") ;;returns "NNO WWW"
(decode ".-.. ---*2 -.- / .-.-.- .- --*2") ;;returns "LOOK ?AMM"
(decode "..---*3 / ......") ;;returns "222 ?"
(decode "...-- .....*4 --... -....") ;; 3576