#lang racket

(define symbols "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
(define codes
 (string-append
 ".- -... -.-. -.. . ..-. --. .... .. .--- -.- .-.. -- "
 "-. --- .--. --.- .-. ... - ..- ...- .-- -..- -.-- --.. "
 "----- .---- ..--- ...-- ....- ..... -.... --... ---.. ----."))

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
        ;; Split by "/" (with optional spaces around it) using a regular expression
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