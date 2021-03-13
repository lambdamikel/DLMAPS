(in-package :cl-user)

(full-reset)

(instance a1 a)
(instance a2 a)
(instance a3 a)

(instance b1 b)
(instance b2 c)

(related a1 b1 r)
(related a2 b2 r)


(process-tuple-at-a-time)
(enable-lazy-tuple-computation)

(pprint (retrieve (?x) (and (?x a) (?x ?y r) (?y b))))

(pprint (retrieve (?xx) (and (?y top) (project-to (?xx) (and (?xx a) (?xx ?yy r) (?yy b))))))

(pprint (retrieve () (and (?y top) (not (project-to () (and (?xx a) (?xx ?yy r) (?yy bottom) ))))) )

(pprint (retrieve () (and (?y top) (and (?xx a) (?xx ?yy r) (?yy b)))))




(pprint (retrieve (?x) (not (and (?x a) (?x ?y r) (?y b)))))

(pprint (retrieve (?x) (not (project-to (?x) (and (?x a) (?x ?y r) (?y b))))))



(pprint (retrieve (?x) (and  (project-to (?x) (and (?x a) (?x ?y r) (?y b))) (?x c) (?y d))))
