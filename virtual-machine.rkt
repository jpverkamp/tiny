#lang racket

(provide (all-defined-out))

; Are we debugging?
(define currently-debugging (make-parameter #f))

; Set this to halt the virtual machine
(define currently-running (make-parameter #f))

; Store instructions for the current virtual machine
(define-struct multiop (arity ops) #:transparent)
(define-struct op (name arity code pattern app) #:transparent)
(define current-instructions (make-parameter (make-hasheq)))
(define current-opcodes      (make-parameter (make-hasheq)))

; Represent memory as a hash to allow for unlimited memory
(define current-memory (make-parameter (make-hasheq)))
(define memory
  (case-lambda
    [(key)     (hash-ref! (current-memory) key 0)]
    [(key val) (hash-set! (current-memory) key val)]))

; Represent the current program counter
(define current-pc (make-parameter 0))

; Macro to define instructions
; Add them both to the name -> multiop hash and the opcode -> op hash
(define-syntax-rule (define-op (NAME ARGS ...) [OPCODE (PARAMS ...) APP] ...)
  (let ()
    (define arity (length '(ARGS ...)))
    
    (define ops 
      (for/list ([opcode  (in-list '(OPCODE ...))]
                 [pattern (in-list '((PARAMS ...) ...))]
                 [app     (in-list (list APP ...))])
        (op 'NAME arity opcode pattern app)))
    
    (hash-set! (current-instructions) 'NAME (multiop arity ops))
     
    (for/list ([opcode (in-list '(OPCODE ...))]
               [op     (in-list ops)])
      (hash-set! (current-opcodes) opcode op))
    
    (void)))

; ----- Virtual machine -----

; Parse instructions from input
(define (parse [in (current-input-port)])
  (port->list read in))

; Match two patterns of possibly matching lists
(define (matched-patterns? ls1 ls2)
  (or (and (null? ls1) (null? ls2))
      (and (not (null? ls1))
           (not (null? ls2))
           (or (and (list? (first ls1)) 
                    (list? (first ls2))
                    (matched-patterns? (rest ls1) (rest ls2)))
               (and (not (list? (first ls1)))
                    (not (list? (first ls2)))
                    (matched-patterns? (rest ls1) (rest ls2)))))))

; Assemble a list of ops
(define (assemble code)
  (cond
    [(null? code) '()]
    [else
     (define name (first code))
     (define multiop (hash-ref (current-instructions) name))
     (define params (take (rest code) (multiop-arity multiop)))
     (define op
       (let loop ([ops (multiop-ops multiop)])
         (cond
           [(null? ops)                
            (error 'assemble "unmatched pattern ~a for ~a\n" params name)]
           [(matched-patterns? params (op-pattern (first ops))) 
            (first ops)]
           [else
            (loop (rest ops))])))
     `(,(op-code op) ,@(flatten params) . ,(assemble (drop code (+ 1 (multiop-arity multiop)))))]))

; Run a given assembled code
(define (run code)
  (define vcode (list->vector code))
  (parameterize ([current-pc 0] [current-memory (make-hasheq)] [currently-running #t])
    (let loop ([ticks 0])
      (define op (hash-ref (current-opcodes) (vector-ref vcode (current-pc))))
      (define args 
        (for/list ([i (in-range (+ 1 (current-pc)) (+ 1 (current-pc) (op-arity op)))])
          (vector-ref vcode i)))
      
      (when (currently-debugging)
        (memory 0)
        (printf "tick: ~a, pc: ~a, current op: ~a = ~a, args = ~a, mem = ~s\n" 
                ticks
                (current-pc)
                (vector-ref vcode (current-pc))
                (op-name op)
                args
                (for/list ([i (in-range (apply min (hash-keys (current-memory)))
                                        (+ 1 (apply max (hash-keys (current-memory)))))])
                  (memory i))))
      
      (current-pc (+ (current-pc) 1 (op-arity op))) ; Apply first to not break jumps
      (apply (op-app op) args)
      (when (currently-running)
        (loop (+ ticks 1))))
    
    (current-memory)))