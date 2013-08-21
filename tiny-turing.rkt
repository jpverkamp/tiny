#lang racket

(require "tiny.rkt"
         "tiny-testing.rkt"
         "virtual-machine.rkt")
(provide (all-defined-out))

; Convert a Turing machine into a Tiny program
(define (turing->tiny states      ; A list of values (must be eq?-able) denoting states
                      symbols     ; A list of values (ditto) denoting symbols on the tape, default is 0
                      start-state ; The starting state (must be in states)
                      final-state ; The halt state (ditto)
                      transition  ; A list of lists of the form (current-state current-symbol next-state write-symbol move-tape)
                                  ;   states and symbols must come from their respective lists
                                  ;   move-tape must be either L or R for left and right respectively
                      initial     ; The initial tape (list of symbols)
                      )
  ; Assign an integer value to each state and symbol
  (define state->index (for/hash ([i (in-naturals)] [v (in-list states)]) (values v i)))
  (define symbol->index (for/hash ([i (in-naturals)] [v (in-list symbols)]) (values v i)))
  
  ; Return a parsed Tiny program
  `(; Store the current state in M[0]
    MOV [0] ,(hash-ref state->index start-state)
    ; Store the current tape pointer in M[1]
    MOV [1] 4
    ; M[2] stores the 3 offset so we can use mmov, M[3] is for the current state
    MOV [2] 3
    ; Encode initial state in M[4] ... (tape expands infinitely to the right)
    ,@(apply 
       append
       (for/list ([offset (in-naturals)]
                  [value (in-list initial)])
         `(MOV [,(+ 4 offset)] ,(hash-ref symbol->index value))))
    ; Halt if we're in the final state, otherwise enter the main body
    JEQ ,(+ 15 (* 3 (length initial))) [0] ,(hash-ref state->index final-state)
    JMP ,(+ 16 (* 3 (length initial)))
    HALT
    ; Encode the transitions
    ,@(apply
       append
       (for/list ([offset (in-naturals)]
                  [each (in-list transition)])
         ; Get the offset of this transition block
         (define block-offset (+ 16 (* 3 (length initial)) (* 29 offset)))
         ; Unpack each transition
         (define-values (current-state current-symbol next-state write-symbol move-tape)
           (apply values each))
         ; Jump over if we don't match
         `(MMOV [2] [1] ; Set M[M[2]] = M[3] to M[M[1]] = M[tape index]
           JEQ ,(+ block-offset 9) [0] ,(hash-ref state->index current-state)
           JMP ,(+ block-offset 29)
           JEQ ,(+ block-offset 15) [3] ,(hash-ref symbol->index current-symbol)
           JMP ,(+ block-offset 29)
           ; We match, update the symbol and state
           MOV [0] ,(hash-ref state->index next-state)
           MOV [3] ,(hash-ref symbol->index write-symbol)
           ; Write that value back into memory
           MMOV [1] [2] ; Set M[M[1]] = M[tape index] to M[M[2]] = M[3]
           ; Move the tape
           ,@(if (eq? move-tape 'R)
                 '(ADD [1] 1)
                 '(SUB [1] 1))
           ; Loop back to get a new function
           JMP ,(+ 9 (* 3 (length initial))))))
    ; Halt if we get an invalid transition
    HALT))

(define (make-tiny-turing states      ; A list of values (must be eq?-able) denoting states
                          symbols     ; A list of values (ditto) denoting symbols on the tape, default is 0
                          start-state ; The starting state (must be in states)
                          final-state ; The halt state (ditto)
                          transition  ; A list of lists of the form (current-state current-symbol next-state write-symbol move-tape)
                                      ;   states and symbols must come from their respective lists
                                      ;   move-tape must be either L or R for left and right respectively
                          )
  ; Convert to Tiny code
  (λ (initial)
    (define tiny-ed (turing->tiny states symbols start-state final-state transition initial))
    
    ; Print out the Tiny version
    (printf "Tiny version:")
    (for ([i (in-naturals)] [byte (in-list tiny-ed)])
      (cond
        [(symbol? byte) (printf "\n~a: ~a" i byte)]
        [else           (printf " ~a"      byte)]))
    (printf "\n\n")
    
    ; Assemble the bytecode and print it
    (printf "Bytecode:\n")
    (define bytecode (assemble tiny-ed))
    (let loop ([bytecode bytecode])
      (cond 
        [(<= (length bytecode) 4) 
         (printf "~a\n" (string-join (map format-hex bytecode) " "))]
        [else
         (printf "~a\n" (string-join (map format-hex (take bytecode 4)) " "))
         (loop (drop bytecode 4))]))
    
    ; Run it
    (printf "\nInput:\n~a\n" initial)
    (define final-memory
      (run bytecode))
    
    ; Print out the final state of the tape
    (printf "\nResult:\n~a\n"
            (for/list ([i (in-range 4 (apply max (hash-keys final-memory)))])
              (hash-ref final-memory i 0)))))
