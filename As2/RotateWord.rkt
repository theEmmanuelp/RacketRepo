#lang racket

(define word "Sample")


(
    define (TheWord x)
        (string->list x)
        (list->string x)
)

(TheWord word)