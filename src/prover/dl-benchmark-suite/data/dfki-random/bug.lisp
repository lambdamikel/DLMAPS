(define-primitive-role r1)
(define-primitive-role r2)
(define-primitive-role r3)
(define-primitive-role r4)
(define-primitive-role r5)
(define-primitive-role r6)
(define-primitive-role r7)
(define-primitive-role r8)
(define-primitive-role r9)
(define-primitive-role r10)

(define-primitive-concept c1
    (and (at-least 1 r8)
         (at-most 8 r2)))

(define-concept c4
    (and c2
         (at-least 2 r8)))


(define-concept c9
    (and c2 c1
         (at-most 5 r2)))

(define-primitive-concept c25
    (and c5 c12
         (at-least 5 r8)))

(define-primitive-concept c36
    (and c9
         (at-least 3 r3)))

(define-primitive-concept c57
    (and c25 c36
         (at-least 2 r2)
         (at-most 8 r10)))
