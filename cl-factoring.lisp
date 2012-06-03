
(defpackage :cl-factoring
  (:use :cl :iterate :cl-primality)
  (:export
   #:factor))

(in-package :cl-factoring)

;; @\section{Introduction}

;; This library presents a set of algorithms for computing integer
;; factorizations.  An interface is provided that takes an integer and will
;; return a list of prime numbers such that, when multiplied together, will
;; result in the original number.

;; There is no magic sauce here or mathematical breakthroughs; the time
;; complexities of each algorithm is exponential.

;; @\section{Special Purpose Methods}

;; In this section we present algorithms that perform integer factorization for
;; composite numbers where we know some extra information about them.  This
;; contains several interesting problems, chief among them is when we know that
;; the problem contains composites whose prime factors are all (or are all but
;; one) small integers.  This set of numbers contains most of the numbers that
;; users encounter in everyday life with the notable exception of the numbers
;; used in crypography.

;; @\subsection{Trial Division}

;; @The <<trial-division>> algorithm is given as a simple method of performing
;; factorizations.  This algorithm is O(sqrt(n)/2), and about the most naive
;; method one can conceive of.  It is still affective in many cases, though we
;; find that its performance is superseded by the Monte Carlo methods introduced
;; later.

(defun trial-division (n)
  (cond ((primep n) (list n))
        (t (apply #'append
                  (mapcar #'trial-division
                          (do ((i 2 (1+ i)))
                              ((or (integerp (/ n i))
                                   (> i (isqrt n)) )
                               (list i (/ n i)) )))))))

;; @\subsection{Pollard's Rho Method}

;; @Although the complexity has not been proven, this method appears to work in
;; some $O(n^{\frac 1 4} {\rm PolyLog}(n))$ making it assymptotically superior
;; to trial division and much faster in practice (this should be really tested,
;; particularly with a prime sieve in place).

(defun pollards-rho (n)
  "Find the prime factorization of N."
  (if (primep n)
      (list n)
      (let ((factor (pollards-rho-find-factor n)))
        (sort (append (pollards-rho factor)
                      (pollards-rho (/ n factor)))
              #'<))))

(defun pollards-rho-find-factor (n)
  (handler-case
      (typecase n
        (fixnum
         (pollards-rho-attempt-fixnum n))
        (integer
         (pollards-rho-attempt n)))
    (factor-attempt-failed ()
      (pollards-rho-find-factor n))))

(define-condition factor-attempt-failed () ())

;; @We use the psuedorandom function that really isn't very random, but it seems
;; to work in practice.

(defun pollards-rho-attempt (n)
  (declare (optimize (speed 3))
           (type integer n))
  (let* ((c (1+ (random n)))
         (f (lambda (x)
              (declare (type integer x c))
              (mod (+ (* x x) c) n))))
    (iter
      (declare (type integer x y d n))
      (for x initially (funcall f 2) then (funcall f x))
      (for y initially (funcall f (funcall f 2)) then (funcall f (funcall f y)))
      (for d = (gcd (abs (- x y)) n))
      (while (= d 1))
      (finally (if (= d n)
                   (signal 'factor-attempt-failed)
                   (return d))))))

(defun pollards-rho-attempt-fixnum (n)
  (declare (optimize (speed 3))
           (type (integer 0 #.(- most-positive-fixnum 1)) n))
  (let* ((c (1+ (random n)))
         (f (lambda (x)
              (declare (type fixnum x c))
              (mod (+ (* x x) c) n))))
    (iter
      (declare (type fixnum x y d n))
      (for x initially (funcall f 2) then (funcall f x))
      (for y initially (funcall f (funcall f 2)) then (funcall f (funcall f y)))
      (for d = (gcd (abs (- x y)) n))
      (while (= d 1))
      (finally (if (= d n)
                   (signal 'factor-attempt-failed)
                   (return d))))))

;; @\subsection{Brent's Cycle Method}

;; @Brent's cycle finding method is very similar to the pollard method but is
;; reportedly a bit faster.

(defun brents-cycle (n)
  "Find the prime factorization of N."
  (if (primep n)
      (list n)
      (let ((factor (brents-cycle-find-factor n)))
        (sort (append (brents-cycle factor)
                      (brents-cycle (/ n factor)))
              #'<))))

(defun brents-cycle-find-factor (n)
  (handler-case
      (brents-cycle-attempt n)
    (factor-attempt-failed ()
      (brents-cycle-find-factor n))))

;; @This is a mess basically because the paper is a mess.

(defun brents-cycle-attempt (n)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (let* ((x0 2)
         (y x0)
         (r 1)
         (q 1)
         x ys
         (g 1)
         k
         (m 1)
         (c (1+ (random n))))
    (labels ((f (x) (mod (- (* x x) c) n)))
      (iter
        (setf x y)
        (iter (for i from 1 to r)
          (setf y (f y)))
        (setf k 0)
        (iter
          (setf ys y)
          (iter (for i from 1 to (min m (- r k)))
            (setf y (f y)
                  q (mod (* q (abs (- x y))) n)))
          (setf g (gcd q n)
                k (+ k m))
          (until (or (>= k r) (> g 1))))
        (setf r (* 2 r))
        (until (> g 1)))
      (if (= g n)
          (iter (setf ys (f ys)
                      g (gcd (abs (- x ys)) n))
            (until (> g 1))))
      (if (= g n)
          (signal 'factor-attempt-failed)
          g))))

;; @While the paper that introduces Brent's method of factorization says that
;; the method is faster than the Pollard method, my experiments with this
;; implemetation place them in a dead heat.

;; @\section{General Purpose Methods}

;; These time complexity of these general methods are independent of the size of
;; the prime factors of the numbers.  The complexity is purely a function of the
;; size of the argument that is to be factored.

;; @\subsection{Shank's square forms factorization}

;; O(n^(1/4))

;; @\subsection{Dixon's factorization method}

;; O(e^(2 sqrt(2) sqrt(log n log log n)))

;; @\subsection{Continued fraction factorization}

;; O(e^sqrt(2 log n log log n))

;; @\subsection{Quadratic sieve}

;; O(e^sqrt(log n log log n))

;; Fastest known algorithm for numbers under 100 decimal digits

;; @\subsection{General number field sieve}

;; O(e^((c+o(1))(log n)^(1/3) (log log n)^(2/3))) (heuristically)

;; Assymptotically fastest known algorithm


;; @\section{Generic Interface}

(defun factor (n)
  "Return the prime factorization of N as a sorted \(smallest to largest) list.  Factors that appear more than once are present mulitiple times in the output.

\(reduce '* (factor n)) = n"
  (pollards-rho n))

;; Define some common name interfaces that hide the methods that are being used.

;; If you would like to access a particular algorithm specifically, use the
;; package <<cl-factoring-algorithms>>.

(defpackage :cl-factoring-algorithms
  (:import-from :cl-factoring
                #:trial-division
                #:pollards-rho
                #:brents-rho)
  (:export #:trial-division
           #:pollards-rho
           #:brents-rho))
