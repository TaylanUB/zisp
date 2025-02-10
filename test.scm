(import
 (rnrs eval)
 (rnrs hashtables))

(define-syntax eval-when-compile
  (lambda (stx)
    (syntax-case stx ()
      ((_ imports body ...)
       (eval
         (syntax->datum #'(begin body ...))
         (apply environment (syntax->datum #'imports)))))))

(eval-when-compile
 ((rnrs))
 (display "foo\n"))

(define-syntax process-data
  (lambda (stx)
    (syntax-case stx ()
      ((_ file)
       (let ((ht (make-eqv-hashtable)))
         (hashtable-set! ht 1 2)
         ht)))))

(define lookup-table (process-data "lookup-table.dat"))

(define seconds-per-day (number->string (* 24 60 60)))

(define (foo arg1:Num arg2:Record)
  (do-stuff arg2.field ))



(define-class Person
  (fields
   name date-of-birth sex
   (age person-age set-person-age!))
  (methods
   ((jump height)
    (if (string=? name "lebron")
        (perform-jump height)))))



(define-record (r1 a b))

(define-record (r1 a b)
  (fields a b)
  (set-r1-a! a)
  (set-r1-b! b))

(define my-r1 (r1 1 2))


(define-record (r2 a b c d)
  (parent (r1 a b)))

(define-record (r2 a b c d)
  (parent (r1 a b))
  (fields c d)
  (set-r2-c! c)
  (set-r2-d! d))

(define-record (r2 a b c d)
  (parent (r1 (* 2 c) (* 4 d)))
  (fields x y z)
  (set-r2-x! a)
  (set-r2-y! b)
  (set-r2-z! (/ a b)))

(define my-r2 (r2 1 2 3 4))


(define-record (r3 a b c d e f)
  (parent r2))

(define-record r3
  (parent r2)
  (fields e f))

(define (init-r3! r a b c d e f)
  (init-r2! r a b c d)
  (set-r3-e! e)
  (set-r3-f! f))


(define r (make-r3))
(init-r3! r 1 2 3 4 5 6)

