;;  Budău George 322CC

#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)

;; TODO 1: done~
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack (list))
(define (make-stack) empty-stack)

(define (push x stack) (cons x stack))
(define (top stack) (car stack))
(define (pop stack) (cdr stack))

;; TODO 2: done~
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (list-ref stack-machine 1))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (list-ref stack-machine 2))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (list-ref stack-machine 3))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (list-ref stack-machine 4))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (list-ref stack-machine 0))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (list-ref stack-machine 5))


(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3: done~
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (cond
    ((equal? symbol 'STACK) 0)
    ((equal? symbol 'CO-VARNAMES) 1)
    ((equal? symbol 'CO-CONSTS) 2)
    ((equal? symbol 'CO-NAMES) 3)
    ((equal? symbol 'CO-CODE) 4)
    (else 5)
    ))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-names (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (append (take stack-machine (get-symbol-index symbol)) (list item) (drop stack-machine (+ 1 (get-symbol-index symbol)))))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)
  (update-stack-machine (push value (get-stack stack-machine)) 'STACK stack-machine))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (update-stack-machine (pop (get-stack stack-machine)) 'STACK stack-machine))

;;  Functia top-exec-stack primește o masină stivă
;;  și intoarce o noua mașina aplicând top pe stiva de execuție.
(define (top-exec-stack stack-machine)
  (top (get-stack stack-machine)))

;;  Functia store-fast stocheaza varful stivei in varnames[value]
;;  si intoarce o noua masina stiva aplicand pop pe stiva de executie
(define (store-fast value varnames stack-machine)
  (update-stack-machine (hash-set varnames value (top (get-stack stack-machine))) 'CO-VARNAMES (pop-exec-stack stack-machine)))

;;  Functia update intoarce o noua masina stiva care
;;  are instruction-counter-ul incrementat cu 1
(define (update IC stack-machine)
  (update-stack-machine (+ 1 IC) 'IC stack-machine))

;;  Functia binary-add primeste o masina stiva
;;  aduna primele 2 valori din stiva, eliminandu-le, adaugand rezultatul sumei pe stiva
;;  intoarce o noua masina stiva ce contine stiva modificata
(define (binary-add stack-machine)
  (push-exec-stack
   (+ (top-exec-stack stack-machine) (top-exec-stack (pop-exec-stack stack-machine)))
   (update-stack-machine (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) 'STACK stack-machine)))

;;  Asemenea functiei binary-add
;;  intoarce o noua masina stiva ce contine in varful stivei rezultatul operatiei TOS1 - TOS0
(define (binary-sub stack-machine)
  (push-exec-stack
   (- (top-exec-stack (pop-exec-stack stack-machine)) (top-exec-stack stack-machine))
   (update-stack-machine (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) 'STACK stack-machine)))

;;  TOS1 % TOS
(define (binary-mod stack-machine)
  (push-exec-stack
   (modulo (top-exec-stack (pop-exec-stack stack-machine)) (top-exec-stack stack-machine))
   (update-stack-machine (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) 'STACK stack-machine)))

;;  Functia compare-op face o operatie booleana intre TOS1 si TOS
;;  operatia se afla la cmpop[value]
;;  operanzii sunt scosi de pe stiva, iar rezultatul operatiei va fi pus pe stiva
;;  intoarce o noua masina stiva
(define (compare-op value stack-machine)
  (push-exec-stack
   ((get-cmpop value) (top-exec-stack (pop-exec-stack stack-machine)) (top-exec-stack stack-machine))
   (update-stack-machine (get-stack (pop-exec-stack (pop-exec-stack stack-machine))) 'STACK stack-machine)))


;;  Dacă TOS e fals, IC se muta la target, dacă nu, se va continua cu următoarea instrucțiune
;;  intoarce o noua stiva pe care se face pop
(define (jump-false index target stack-machine)
 (if (equal? (top-exec-stack stack-machine) #f)
     (pop-exec-stack (update-stack-machine (quotient target 2) 'IC stack-machine))
     (pop-exec-stack (update index stack-machine))
     ))

;;  Functia jump-absolute sare la instrucțiunea de pe bytecode-ul target 
(define (jump-absolute target stack-machine)
  (update-stack-machine (quotient target 2) 'IC stack-machine))


;;  FOR_ITER va verifica dacă se poate face next pe iterator,
;;  iar în acest caz se va face push pe stiva la noul iterator, apoi la primul element
;;  (top-ul stivei este reprezentat de element)
(define (for-iter index delta stack-machine)
  (let ((TOS (top-exec-stack stack-machine)))
    (if (not (equal? TOS '()))
        (push-exec-stack (car TOS) (push-exec-stack (cdr TOS) (pop-exec-stack (update index stack-machine))))
        (pop-exec-stack (update-stack-machine (+ index (quotient (+ delta 2) 2)) 'IC stack-machine))
        )
    ))

;;  Functia apelează o funcție. Pe stivă se află un număr de argc argumente, urmat de numele funcției (pozitia argc a stivei).
;;  CALL_FUNCTION le va scoate de pe stivă, va apela funcția și va pune rezultatul pe stivă.
(define (call-function index argc stack-machine)
  (let ((function (get-function (list-ref (get-stack stack-machine) argc))) (TOS (top-exec-stack stack-machine)))
    (cond
      ((equal? function writeln)
         (writeln TOS)
         (run-stack-machine (update index (pop-exec-stack (pop-exec-stack stack-machine))))
         )
      ((equal? function range)
       (run-stack-machine (update index (push-exec-stack (range TOS) (pop-exec-stack (pop-exec-stack stack-machine))))))
      ((equal? function sqrt)
       (run-stack-machine (update index (push-exec-stack (sqrt TOS) (pop-exec-stack (pop-exec-stack stack-machine))))))
      ((equal? function 'multiply)
       (let iter ((argc argc) (result 1) (stack-machine stack-machine))
         (if (= argc 0)
             (run-stack-machine (update index (push-exec-stack result (pop-exec-stack stack-machine))))
             (iter (- argc 1) (* result (top-exec-stack stack-machine)) (pop-exec-stack stack-machine))
             )
         ))
    )))

;; TODO 4: done~
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.

(define (run-stack-machine stack-machine)
  (let* ((codes (get-code stack-machine))
        (consts (get-consts stack-machine))
        (names (get-names stack-machine))
        (stack (get-stack stack-machine))
        (index (get-IC stack-machine))
        (varnames (get-varnames stack-machine))
        (instruction (car (list-ref codes index)))
        (value (cdr (list-ref codes index))))
    (cond
      ((equal? instruction 'LOAD_CONST)
       (run-stack-machine (push-exec-stack (hash-ref consts value) (update index stack-machine))))
      ((equal? instruction 'STORE_FAST)
       (run-stack-machine (store-fast value varnames (update index stack-machine))))
      ((equal? instruction 'LOAD_FAST)
       (run-stack-machine (push-exec-stack (hash-ref varnames value) (update index stack-machine))))
      ((equal? instruction 'LOAD_GLOBAL)
       (run-stack-machine (push-exec-stack (hash-ref names value) (update index stack-machine))))
      ((equal? instruction 'BINARY_ADD)
       (run-stack-machine (binary-add (update index stack-machine))))
      ((equal? instruction 'BINARY_SUBTRACT)
       (run-stack-machine (binary-sub (update index stack-machine))))
      ((equal? instruction 'BINARY_MODULO)
       (run-stack-machine (binary-mod (update index stack-machine))))
      ((equal? instruction 'INPLACE_ADD)
       (run-stack-machine (binary-add (update index stack-machine))))
      ((equal? instruction 'INPLACE_SUBTRACT)
       (run-stack-machine (binary-sub (update index stack-machine))))
      ((equal? instruction 'INPLACE_MODULO)
       (run-stack-machine (binary-mod (update index stack-machine))))
      ((equal? instruction 'COMPARE_OP)
       (run-stack-machine (compare-op value (update index stack-machine))))
      ((equal? instruction 'POP_JUMP_IF_FALSE)
       (run-stack-machine (jump-false index value stack-machine)))
      ((equal? instruction 'JUMP_ABSOLUTE)
       (run-stack-machine (jump-absolute value stack-machine)))
      ((equal? instruction 'RETURN_VALUE) stack-machine)
      ((equal? instruction 'FOR_ITER)
       (run-stack-machine (for-iter index value stack-machine)))
      ((equal? instruction 'CALL_FUNCTION) (call-function index value stack-machine))
      (else (run-stack-machine (update index stack-machine)))
    )
  ))
