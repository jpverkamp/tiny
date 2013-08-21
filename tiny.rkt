#lang racket

(require "virtual-machine.rkt")
(provide (all-defined-out))

(define-syntax-rule (define-simple-pair NAME OP1 OP2 f)
  (define-op (NAME a b)
    [OP1 ([a] [b]) (λ (a b) (memory a (f (memory a) (memory b))))]
    [OP2 ([a] b  ) (λ (a b) (memory a (f (memory a) b)))]))

(define-simple-pair AND #x00 #x01 bitwise-and)
(define-simple-pair OR  #x02 #x03 bitwise-ior)
(define-simple-pair XOR #x04 #x05 bitwise-xor)

(define-op (NOT a)
  [#x06 ([a]) (λ (a) (memory a (bitwise-not (memory a))))])

(define-simple-pair MOV #x07 #x08 (λ (a b) b))

(define-op (RANDOM a)
  [#x09 ([a]) (λ (a) (memory a (random 256)))])

(define-simple-pair ADD #x0a #x0b +)
(define-simple-pair SUB #x0c #x0d -)

(define-op (JMP x)
  [#x0e ([x]) (λ (x) (current-pc (memory x)))]
  [#x0f (x)   (λ (x) (current-pc x))])

(define-op (JZ x a)
  [#x10 ([x] [a]) (λ (x a) (when (zero? (memory a)) (current-pc (memory x))))]
  [#x11 ([x] a)   (λ (x a) (when (zero? a) (current-pc (memory x))))]
  [#x12 (x   [a]) (λ (x a) (when (zero? (memory a)) (current-pc x)))]
  [#x13 (x   a)   (λ (x a) (when (zero? a) (current-pc x)))])

(define-syntax-rule (define-comparison-jump NAME OP1 OP2 OP3 OP4 f)
  (define-op (NAME x a b)
    [OP1 ([x] [a] [b]) (λ (x a b) (when (f (memory a) (memory b)) (current-pc (memory x))))]
    [OP2 (x   [a] [b]) (λ (x a b) (when (f (memory a) (memory b)) (current-pc x)))]
    [OP3 ([x] [a] b)   (λ (x a b) (when (f (memory a) b) (current-pc (memory x))))]
    [OP4 (x   [a] b)   (λ (x a b) (when (f (memory a) b) (current-pc x)))]))

(define-comparison-jump JEQ #x14 #x15 #x16 #x17 =)
(define-comparison-jump JLS #x18 #x19 #x1a #x1b <)
(define-comparison-jump JGT #x1c #x1d #x1e #x1f >)

(define-op (HALT)
  [#xff () (λ () (currently-running #f))])

(define-syntax-rule (define-print NAME OP1 OP2 f)
  (define-op (NAME a)
    [OP1 ([a]) (λ (a) (f (memory a)))]
    [OP2 (a)   (λ (a) (f a))]))

(define-print APRINT #x20 #x21 (λ (byte) (display (integer->char byte))))
(define-print DPRINT #x22 #x23 (λ (byte) (display byte) (display " ")))

(define-op (MMOV a b)
  [#xf0 ([a] [b]) (λ (a b) (memory (memory a) (memory (memory b))))])