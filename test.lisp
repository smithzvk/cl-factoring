
(defpackage :cl-factoring-test
  (:use :cl :stefil :iterate :cl-factoring :cl-primality)
  (:export
   #:general-factoring-tests
   #:square-factoring-tests
   #:carmichael-composites))

(in-package :cl-factoring-test)

(in-root-suite)

(defsuite* generic-factoring-interface)

(deftest general-factoring-tests ()
  (iter
    (for n in
      '(594821878991 296181385662 55133120189 589686960742 844842681592 782102670870
        664213911229 200798217610 213642224732 67999737405 59224237970 847939737282
        956357310803 779487349554 897676050415 670071650171 177442922862 82364240230
        29246636606 937324343712 95983448704 277724899268 258955799550 886462274428
        233558644522 941193905440 961609313714 330429842121 792340346634 731048293620
        196397926502 287035705613 745565906165 491462797520 516540507109 996150822486
        137858408060 838488848869 63540587836 349373932734 116459759771 42826737972
        282847849048 16031631432 319604850753 306928205128 595491725498 581479398271
        621002796221 374944360706 203995326049 990222942201 969534087795 935088269353
        853141841045 139986277277 765816028577 854866588293 151170237996 52706005835
        766690466240 193150851336 482265507325 41472224801 101869686065 24503109994
        231946530639 426736643923 742488219057 203173813309 33683074919 516478800793
        450250670977 637972294425 299099850029 957524107285 742713051302 473329609121
        109260591611 859583875134 424559882258 427416923090 355186467546 497013410730
        251333910219 237718213221 198351484177 33469606497 280292261799 60557010647
        999112023247 844076813976 295771326151 939667572874 909642705403 82738404572
        145473248660 128092148325 998792068518 897116510827))
    (let ((factors (factor n)))
      (is (every 'primep factors))
      (is (= (apply #'* factors) n)))))

;; Squares are actually extremely easy to factor, but many methods need to have
;; special treatment of these numbers.  This test it to ensure that these
;; methods correctly provide that special case.

(deftest square-factoring-tests ()
  (iter
    (for n in
      '(4 9 16 25 36 49 64 81 100 440896 15129 366025 5476 556516 410881))
    (let ((factors (factor n)))
      (is (every 'primep factors))
      (is (= (apply #'* factors) n)))))

;; Here we test Carmicheal numbers which are known to be difficult in
;; probabilistic primality testing, so here is a test for factoring them.

(deftest carmichael-composites ()
  (let ((carmichaels
          ;; Taken from the wikipedia page.
          '((561 1105 1729 2465 2821 6601 8911)
            (41041 62745 63973 75361 101101 126217 172081 188461 278545 340561)
            (825265)
            (321197185)
            (5394826801)
            (232250619601)
            (9746347772161))))
    (iter (for cars in carmichaels)
      (iter (for num in cars)
        (let ((factors (factor num)))
          (is (every 'primep factors))
          (is (= (apply #'* factors) num)))))))

(defsuite* general-factoring)

(defsuite* specialized-factoring)

(defsuite* issues)

(deftest numbers-that-err ()
  (let ((n 327146098439579))
    (is (= n (cl-factoring-algorithms:hmpqs n)))))
