#lang racket

(require "tiny.rkt"
         "virtual-machine.rkt")
(provide (all-defined-out))

(define (format-hex byte)
  (format (if (< byte 16) "0x0~x" "0x~x") byte))

(define (bytecode->string code)
  (string-join (map format-hex code) " "))

(define (tiny code)
  (printf "source:~a\n" code)
  (define bytecode (assemble (call-with-input-string code parse)))
  (printf "bytecode:\n")
  (let loop ([bytecode bytecode])
    (cond 
      [(<= (length bytecode) 4) 
       (printf "~a\n" (string-join (map format-hex bytecode) " "))]
      [else
       (printf "~a\n" (string-join (map format-hex (take bytecode 4)) " "))
       (loop (drop bytecode 4))]))
  (printf "\nrunning:\n")
  (run bytecode)
  (printf "\n\n"))

#;(tiny "
MOV [0] 5
MOV [1] 7
ADD [0] [1]
DPRINT [0]
HALT
")

#;(tiny "
MOV [0] 5
MOV [1] 7
MOV [2] 0
MOV [3] 0
DPRINT [0]
APRINT 42
DPRINT [1]
APRINT 61
JEQ 32 [1] [3]
ADD [3] 1
ADD [2] [0]
JMP 20
MOV [0] [2]
DPRINT [0]
HALT
")
                         
