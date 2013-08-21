#lang racket

(require "tiny.rkt"
         "tiny-turing.rkt"
         "virtual-machine.rkt")
(provide (all-defined-out))

(define ones-to-twos
  (make-tiny-turing
   '(start one halt)
   '(0 1 2)
   'start
   'halt
   '((start 1 start 2 R)
     (start 0 halt  0 R))))

#;(ones-to-twos '(1 1 1))

(define double-list
  (make-tiny-turing
   '(start goto-end goto-start loop restart clear halt)
   '(0 1 start old new)
   'start
   'halt
   '(; Mark the starting position
     (start      1     goto-end   start R) 
     ; Go to the first 0, replace it with new
     (goto-end   old   goto-end   old   R)
     (goto-end   new   goto-end   new   R)
     (goto-end   0     goto-start new   L) 
     (goto-end   1     goto-end   1     R)
     ; Go back to the start
     (goto-start start loop       start R)
     (goto-start old   goto-start old   L) 
     (goto-start new   goto-start new   L)
     (goto-start 1     goto-start 1     L)
     ; Loop back or check if we're done
     (loop       old   loop       old   R)
     (loop       new   restart    new   L)
     (loop       1     goto-end   old   R)
     ; Go back to the start symbol
     (restart    old   restart    old   L)
     (restart    start clear      1     R)
     ; Write out all 1s
     (clear      old   clear      1     R)
     (clear      new   clear      1     R)
     (clear      0     halt       0     R))))

#;(double-list '(1 1 1))
#;(double-list (map (λ (_) (range 100))))