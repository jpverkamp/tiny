#lang racket

(require "tiny.rkt"
         "tiny-testing.rkt"
         "virtual-machine.rkt")
(provide (all-defined-out))

; Convert a list of digits in any base to decimal
(define (digits->number digits base)
  (for/sum ([i (in-naturals)]
            [d (in-list (reverse digits))])
    (* d (expt base i))))

; Convert a decimal number to a list of digits in any base
(define (number->digits number base)
  (reverse 
   (let loop ([number number])
     (cond
       [(< number base) 
        (list number)]
       [else            
        (cons (remainder number base)
              (loop (quotient number base)))]))))

; Convert a Turing machine into a Tiny program
(define (make-tiny-turing 
         states      ; A list of values (must be eq?-able) denoting states
         symbols     ; A list of values (ditto) denoting symbols on the tape, default is 0
         start-state ; The starting state (must be in states)
         final-state ; The halt state (ditto)
         transition  ; A list of lists of the form (current-state current-symbol next-state write-symbol move-tape)
                     ;   states and symbols must come from their respective lists
                     ;   move-tape must be either L or R for left and right respectively
         )
  ; Store how many symbols we have
  (define symbol-count (length symbols))
  
  ; Assign an integer value to each state and symbol
  (define (hash-index ls)
    (let ([hash (for/hasheq ([i (in-naturals)] 
                             [v (in-list ls)]) 
                  (values v i))])
      (λ (key)
        (hash-ref hash key))))
  
  (define state->value (hash-index states))
  (define symbol->value (hash-index symbols))
  
  (define value->symbol
    (let ([hash (for/hasheq ([i (in-naturals)] 
                             [v (in-list symbols)]) 
                  (values i v))])
      (λ (key)
        (hash-ref hash key))))
  
  (λ (initial)       ; The initial tape (list of symbols)
    ; Convert to tiny code
    (define tiny-code
      (let ()
        ; M[0] is current state
        (define state '[0])
        
        ; M[1] is the leftward side of tape
        (define left  '[1])
        
        ; M[2] is the current state
        (define curr  '[2])
        
        ; M[3] is the rightward side of the tap
        (define right '[3])
        
        ; M[4+] are buffers
        (define (buffer n) `[,(+ n 4)])
        
        `(; 0: Create the initial state
          MOV ,state ,(state->value 'start)
          MOV ,left  0
          MOV ,curr  ,(symbol->value (first initial))
          MOV ,right ,(digits->number 
                       (map symbol->value (reverse (rest initial))) 
                       symbol-count)
          
          ; 12: Check for halt condiction
          JEQ 18 ,state ,(state->value 'halt)
          JMP 19
          HALT
          
          ; 19: Decode state transitions
          ,@(apply
             append
             (for/list ([i (in-naturals)]
                        [transition (in-list transition)])
               ; Calculate the offset for the start of this block
               (define block-size 72)
               (define block (+ 19 (* i block-size)))
               
               ; Unpack the transition
               (define-values (current-state current-symbol next-state write-symbol move-tape)
                 (apply values transition))
               
               ; Figure out which side we're moving from/to
               (define move-to   (if (eq? move-tape 'L) right left))
               (define move-from (if (eq? move-tape 'L) left  right))
               
               `(; +0: Check the current state and symbol
                 JEQ ,(+ block 6) ,state ,(state->value current-state)
                 JMP ,(+ block block-size)
                 JEQ ,(+ block 12) ,curr ,(symbol->value current-symbol)
                 JMP ,(+ block block-size)
                 
                 ; +12: Update the state and symbol
                 MOV ,state ,(state->value next-state)
                 MOV ,curr  ,(symbol->value write-symbol)
                 
                 ; +18: Move the current state into the buffer
                 ;      Multiply by symbol count, add current
                 MOV ,(buffer 0) ,(- symbol-count 1)
                 MOV ,(buffer 1) ,move-to
                 JZ  ,(+ block 35) ,(buffer 0)
                 ADD ,move-to ,(buffer 1)
                 SUB ,(buffer 0) 1
                 JMP ,(+ block 24)
                 ADD ,move-to ,curr
                 
                 ; +35: Get the next symbol from the other buffer
                 MOV ,curr ,move-from
                 JLS ,(+ block 50) ,curr ,symbol-count
                 SUB ,curr ,symbol-count
                 JMP ,(+ block 41)
                 
                 ; +47: Remove current from other buffer
                 ;      Subtract current, divide by symbol count
                 SUB ,move-from ,curr
                 MOV ,(buffer 0) 0
                 JZ  ,(+ block 67) ,move-from
                 ADD ,(buffer 0) 1
                 SUB ,move-from ,symbol-count
                 JMP ,(+ block 56)
                 MOV ,move-from ,(buffer 0)
                 
                 ; +67: Jump back to the loop
                 JMP 12
                 )))
          
          ; If we don't match any transition, stop
          HALT)))
    
    ; Print that out
    (printf "Tiny source:")
    (for ([i (in-naturals)]
          [byte (in-list tiny-code)])
      (cond
        [(symbol? byte) (printf "\n~a: ~a " i byte)]
        [else           (printf "~a " byte)]))
    (printf "\n")
    
    ; Assemble the bytecode and print it
    (printf "\nBytecode:\n")
    (define bytecode (assemble tiny-code))
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
    (define final-tape
      (map 
       value->symbol
       (append
        (number->digits (hash-ref final-memory 1 0) symbol-count)
        (list           (hash-ref final-memory 2 0))
        (number->digits (hash-ref final-memory 3 0) symbol-count))))
    (printf "\nResult:\n~a\n" final-tape)
    final-tape))