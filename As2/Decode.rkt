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

 (hash-ref morse-hash ".-" "?")