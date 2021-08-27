#lang racket

(provide generate-chain generate-from-chain)

(define (generate-chain len sequences)
  (let ([chain (make-immutable-hash)])
    (generate-chain-iter len sequences chain)))

(define (generate-chain-iter len sequences chain)
  (let ([seq (first sequences)]
        [remaining-seqs (rest sequences)])
    (if (null? remaining-seqs)
        (train-chain len seq chain)
        (generate-chain-iter len remaining-seqs (train-chain len seq chain)))))

(define (train-chain len seq chain)
  (let ([prefix (make-list len 'start)])
    (train-chain-iter seq prefix chain)))

(define (train-chain-iter seq key chain)
  (let ([x (first seq)]
        [xs (rest seq)])
    (if (null? xs)
        (attach-to-chain (rotate-key key x) 'end (attach-to-chain key x chain))
        (train-chain-iter xs (rotate-key key x) (attach-to-chain key x chain)))))

(define (attach-to-chain key x chain)
  (let ([refd (hash-ref chain key '())])
    (hash-set chain key (append refd (list x)))))

(define (rotate-key key x)
  (append (rest key) (list x)))

(define (get-random-element xs)
  (list-ref xs (random (length xs))))

(define (generate-from-chain len chain)
  (let ([prefix (make-list len 'start)])
    (generate-from-chain-iter prefix chain (list))))

(define (generate-from-chain-iter key chain output)
  (let ([refd (get-random-element (hash-ref chain key))])
    (if (equal? refd 'end)
        output
        (generate-from-chain-iter (rotate-key key refd) chain (append output (list refd))))))
