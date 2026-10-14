#lang racket

(define (depth x lst)

(define (shield value)
  (if (empty? value)
      '()
      ;; NEW: whole list is shielded
      (if (and (pair? value)
               (equal? (first value) '*))
          (shield (rest value))

          (if (pair? (first value))
              ;; SUBLIST CASE
              (if (equal? (first (first value)) '*)
                  ;; skip shielded sublist
                  (shield (rest value))
                  ;; keep non-shielded sublist
                  (cons (shield (first value))
                        (shield (rest value))))

              ;; ATOM CASE
              (cons (first value)
                    (shield (rest value)))))))

  

  (define (layersearch value level shallow)
    (if (empty? value)
        shallow
        (if (pair? (first value))
            ;; SUBLIST CASE
            (layersearch (rest value) level
                         (layersearch (first value) (+ level 1) shallow))
            ;; ATOM CASE — check for x
            (layersearch (rest value) level
                         (if (and (equal? (first value) x)
                                  (< level shallow))
                             level
                             shallow)))))

  (define (result)
    (layersearch (shield lst) 1 100))

  (if (= (result) 100)
      0
      (result)))



(define (within? find data limit)
  (define (atoms lst)
    ;go through the find list
    (if (empty? lst)
        '()
        (if (pair? (first lst))
            ;combine the list
            (append (atoms (first lst))
                    (atoms (rest lst)))
            ;add the value into the list
            (cons (first lst)
                  (atoms (rest lst))))))
  (define (check lst)
    (if (empty? lst)
        #t
        ;check if level <= limit
        (if (or (= (depth (first lst) data) 0)
                (> (depth (first lst) data) limit))
            #f
            (check (rest lst)))))
  (check (atoms find)))


(depth 'a '()) ;returns 0
(depth 'a '(b a c)) ;returns 1
(depth 'a '(b (c (a)) d)) ;returns 3
(depth 'a '((((a))) b a)) ;returns 1
(depth 'a '(x (y (a)) (a))) ;returns 2
(depth 'a '(x (* a) y)) ;returns 0
(depth 'a '(x (* a) ((a)))) ;returns 3
(depth 'a '(* a b)) ;returns 0
(depth '* '(x *)) ;returns 1
(depth 'b '(a (* (b)) (c (* b) (b)))) ;returns 3


(within? '() '() 1) ;returns #t
(within? '(a) '() 5) ;returns #f
(within? '(a b) '(b (a)) 2) ;returns #t
(within? '(a b) '(b (a)) 1) ;returns #f
(within? '(a (b (c))) '(c ((a)) (b)) 3) ;returns #t
(within? '(a (b (c))) '(c ((a)) (b)) 2) ;returns #f
(within? '(a b) '(a (* b) ((b))) 2) ;returns #f