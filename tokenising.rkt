#lang racket

(provide tokenise-sentences untokenise-sentences)

(define (tokenise-sentences str)
  (define (add-to-list-appropriately t tokens)
    (if (char? t)
        (if (or (null? tokens) (not (list? (first tokens))))
            (cons (list t) tokens)
            (cons (cons t (first tokens)) (rest tokens)))
        (cons t tokens)))
  (define (unmangle-tokens tokens)
    (reverse
     (map (lambda (t)
            (if (list? t)
                (list->string (reverse t))
                t))
          tokens)))
  (unmangle-tokens (foldl
                    (lambda (ch tokens)
                      (add-to-list-appropriately (match ch
                                                   [c #:when (char=? c #\ ) 'space]
                                                   [c #:when (char=? c #\.) 'fullstop]
                                                   [c #:when (char=? c #\,) 'comma]
                                                   [c #:when (char=? c #\?) 'question]
                                                   [c c])
                                                 tokens))
                    (list)
                    (string->list str))))

(define (untokenise-sentences tokens)
  (map
   (lambda (t) (if (string? t)
                   t
                   (match t
                     ['space " "]
                     ['fullstop "."]
                     ['comma ","]
                     ['question "?"])))
   tokens))