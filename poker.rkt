#lang racket

(require web-server/servlet
         web-server/servlet-env)
(require racket/match)
(require "pokelist.rkt")
(require "markov.rkt")
(require "tokenising.rkt")

(define name-len 3)  
(define name-chain (generate-chain name-len (map (lambda (x) (string->list (car x))) pokelist)))
(define new-names (map (lambda (x) (list->string (generate-from-chain name-len name-chain))) (range 10)))

(define desc-len 5)
(define description-chain (generate-chain
                           desc-len
                           (map (lambda (x)
                                  (tokenise-sentences
                                   (string-downcase (cdr x)))) pokelist)))
;(define new-descs (map (lambda (x) (string-join (untokenise-sentences (generate-from-chain desc-len description-chain)) "")) (range 10)))

(define (start req)
  (response/xexpr
   `(html (head (title "Pokér!")
                (link ((rel "stylesheet") (href "poker/poke.css") (type "text/css"))))
          (body (main
                 (h1 "Generate a new pokemon for yourself")
                 (p (h2 "Name")
                    ,(list->string (generate-from-chain name-len name-chain)))
                 (p (h2 "Description")
                    ,(string-join (untokenise-sentences (generate-from-chain desc-len description-chain)) ""))
                 (p ((class "little"))
                    "CSS by "
                    (a ((href "https://brandonme.net")) "Brandon O'Brien")))))))

(serve/servlet start
               #:servlet-path "/"
               #:extra-files-paths (list (build-path (current-directory) "static")))
