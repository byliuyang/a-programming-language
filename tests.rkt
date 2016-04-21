#lang plai-typed
; 1) Test for mutation in binary operator
(test (run '(with([b (box 0)])
              (* (seq (setbox b 10)
                        2)
                 (+ (seq (setbox b
                                     (+ (unbox b) 10))
                           1)
                    (unbox b)))))
      (numV 42))

; 2) Test for mutation with static scope
(test (run '(with([b (box 0)])
              (with ([f (fun (x) (+ (unbox b) x))])
                (seq (setbox b 2)
                       (f 1)))))
      (numV 3))

; 3) Test for nested mutations
(test (run '(with([b (box 2)])
              (with ([f (fun
                           (x y)
                         (if0 (unbox b)
                              (+ x (unbox b))
                              (seq
                                (setbox b y)
                                (* x (unbox b))
                                )))])
                (+ (f 1 0) (f 7 2)))))
      (numV 7))

; 4) Chained object
(test (run '(with ([b1 (box 1)])
              (with ([b2 b1])
                (with ([b3 b2])
                  (seq (setbox b1 2)
                         (unbox b3))))))
      (numV 2))

; 5) setbox on number
(test/exn (run '(with [(b 1)]
                  (setbox b 2)))
          "type")
; 6) unbox on number
(test/exn (run '(with [(b 1)]
                  (unbox b 2)))
          "type")

; 7) set static scope
(test (run '(with [(b 1)]
              (with ([f (fun (x) (+ x b))])
                (seq
                  (set b 10)
                  (f 2)))))
      (numV 12))
; 8) set chained static scope
(test (run '(with [(b1 1)]
              (with ([b2 b1])
                (seq
                  (set b1 10)
                  b2))))
      (numV 1))

; 9) set chained
(test (run '(with [(b1 (box 1))]
              (with ([b2 b1])
                (seq
                  (set b1 (box 2))
                     (unbox b2)))))
      (numV 1))

; 10) set on argument 
(test (run '(with ([f (fun (x) (seq (set x (+ x x))
                                        x))])
              (+ (f 1) (f 2))))
      (numV 6))
; ---------------------------------

; Tests for Arithmetics
; Test for plus
(test (run '(+ 1 2)) (numV 3))
; Test for multiply
(test (run '(* 1 2)) (numV 2))
; Test for binary minus
(test (run '(- 4 3)) (numV 1))

; Tests for if0
; Execute “then” expression
(test (run '(if0
             0
             (+ 1 2)
             (+ 5 6)))
      (numV 3))
; Execute “else” expression
(test (run '(if0
             10
             (+ 1 2)
             (+ 5 6)))
      (numV 11))
(test (run '(if0
             -1
             (+ 1 2)
             (+ 5 6)))
      (numV 11))
; unbound error
(test/exn (run '(if0
                 x
                 (+ 1 2)
                 (+ 5 6)))
          "unbound")
; Signal an error for non-numeric test values
(test/exn (run '(if0
                 (fun (x y) 5)
                 (+ 1 2)
                 (+ 5 6)))
          "type")

; Tests for with
(test (run '(with
             ((x 3)
              (y 5))
             (* x y)))
      (numV 15))
; Tests for with unbound 
(test/exn (run '(with
                 ((x 3))
                 (* x y)))
          "unbound")

; Tests for with unbound 
(test/exn (run '(+ (with
                 ((x 3))
                 (* x x)) x))
          "unbound")

(test (run '(with ([x 3])
                  (with
                   ([f (fun (y) (+ x y))])
                   (f 6))))
      (numV 9))
; Tests for nearest env
(test (run '(with ([x 3])
                  (with
                   ([x 6]
                    [y x])
                   (+ x y))))
      (numV 9))
; Tests for not unbound reference
(test/exn (run '(with
                 ([x 3]
                  [y x])
                 (* x y)))
          "unbound")
; Tests for environment
(test/exn (run '(+ (with
                    ((x 3))
                    (* x y))
                   x))
          "unbound")
; Tests for define repeated variable
(test/exn  (run '(with
                  ((x 3)
                   (y 5)
                   (x 7))
                  (* x y)))
           "multiple")
; Tests for with nested static binding
(test (run '(with ([x 3])
                  (with
                   ([f (fun (y) (+ x y))])
                   (with
                    ([x 5])
                    (f 6)))))
      (numV 9))

; Tests for function
(test  (run '((fun
               (_)
               (+ 1 2)) 2))
       (numV 3))
(test  (run '((fun (x y) (+ x y)) 1 2))
       (numV 3))
; Tests for nested fun
(test  (run '((fun
               (x)
               ((fun (y) (+ x y))
                2))
              1))
       (numV 3))
(test  (run '((fun
               (x)
               ((fun
                 (x)
                 (+ x x))
                2))
              1))
       (numV 4))
; Test for unbound error
(test/exn  (run '((fun (y) (+ x y)) 1)) "unbound")
; Test for repeated parameter
(test/exn  (run '((fun (x x) (+ x x)) 1 2)) "multiple")