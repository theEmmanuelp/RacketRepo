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



