#lang racket

(require net/url)
(require json)

(define (.> key js)
  (hash-ref js (string->symbol key)))

(define (i> index js)
  (list-ref js index))

(define (get-entry my-url)
  (define google-port (get-pure-port (string->url my-url)))
  (define my-object (read-json google-port))
  (close-input-port google-port)
  my-object)

(define (get-list-of-dex-entries pokedex-document)
  (.> "pokemon_entries" pokedex-document))

(define (get-species-urls dex-entries)
  (map
   (lambda (entry) (.> "url" (.> "pokemon_species" entry)))
   dex-entries))

(define pokedex (get-entry "https://pokeapi.co/api/v2/pokedex/1"))

(define species-urls
  ((compose get-species-urls get-list-of-dex-entries) pokedex))

(define (clean-string str)
  (string-replace (string-replace str "\f" " ") "\n" " "))

(struct species-entry (name desc))

(define (get-species-entry species-document)
  (species-entry
   (.> "name" species-document)
   (clean-string (.> "flavor_text"
                     (findf
                      (lambda (obj)
                        (string=?  (.> "name" (.> "language" obj)) "en"))
                      (.> "flavor_text_entries" species-document))))))

(define (make-csv entry) (string-append (species-entry-name entry) "," "\"" (species-entry-desc entry) "\""))

(define all-entries
  (map
   (lambda (url)
     ((compose make-csv get-species-entry get-entry) url))
   (take species-urls 151)))

(display-lines-to-file all-entries "151.txt" #:mode 'text #:exists 'replace)
