
;;;;;;;;;;;;;;;;
;; APARTADO 1 ;;
;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finds the middle point between 2 points.
;;
;; a: lower extremum of the interval in which we want to find the middle
;; b: b>a upper extremum of the interval in which we want to find the middle
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mid (a b)
  (float (/ (+ a b) 2)))

(mid 2 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finds the distance between 2 points.
;;
;; a: lower extremum of the interval which length we want to find out
;; b: b>a upper extremum of the interval which length we want to find out
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dist (a b)
  (abs (- a b)))

(dist 2 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implements the recursive part of bisect, thus preventing redundant error
;; checking.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bisect_rec (f a b tol)
  (cond ((= (funcall f a) 0.0) (return-from bisect_rec a))
        ((= (funcall f b) 0.0) (return-from bisect_rec b))
        ((< (funcall #'dist a b) tol) (return-from bisect_rec (funcall #'mid a b)))
          (t (if (< (* (funcall f a) (funcall f (funcall #'mid a b))) 0.0)
             (return-from bisect_rec (funcall #'bisect_rec f a (funcall #'mid a b) tol))
             (return-from bisect_rec (funcall #'bisect_rec f (funcall #'mid a b) b tol))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finds a root of f between the points a and b using bisection.
;;
;; If f(a)f(b)>0 there is no guarantee that there will be a root in the
;; interval, and the function will return NIL.
;;
;; f: function of a single real parameter with real values whose root
;; we want to find
;; a: lower extremum of the interval in which we search for the root
;; b: b>a upper extremum of the interval in which we search for the root
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;; returns (a+b)/2 as a solution.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bisect (f a b tol)
  (cond ((null a) (return-from bisect NIL))
        ((null b) (return-from bisect NIL))
        ((> a b) (return-from bisect NIL))
        ((> (* (funcall f a) (funcall f b)) 0.0) (return-from bisect NIL))
        ((< tol 0.0) (return-from bisect NIL))
        (t (return-from bisect (funcall #'bisect_rec f a b tol)))))


(bisect #'(lambda(x) (sin (* 6.26 x))) 0.1 0.7 0.001) ;;---> 0.5020995
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 1.5 0.001) ;;---> NIL
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 2.1 0.001) ;;---> NIL


;;;;;;;;;;;;;;;;
;; APARTADO 2 ;;
;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implements the recursive part of allroot, thus preventing redundant error
;; checking.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allroot_rec (f lst tol)
  (cond ((null lst) (return-from allroot_rec NIL))
        ((null (rest lst)) (return-from allroot_rec NIL))
        (t (return-from allroot_rec (cons
                                     (funcall #'bisect f (first lst) (first (rest lst)) tol)
                                     (funcall #'allroot_rec f (rest lst) tol))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finds all the roots that are located between consecutive values of a list
;; of values
;;
;; Parameters:
;;
;; f: function of a single real parameter with real values whose root
;; we want to find
;; lst: ordered list of real values (lst[i] < lst[i+1])
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;; returns (a+b)/2 as a solution.
;;
;; Whenever sgn(f(lst[i])) != sgn(f(lst[i+1])) this function looks for a
;; root in the corresponding interval.
;;
;; Returns:
;; A list o real values containing the roots of the function in the
;; given sub-intervals
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allroot (f lst tol)
  (if (< tol 0.0) (return-from allroot nil)
    (return-from allroot (remove NIL (funcall #'allroot_rec f lst tol)))))


(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001) ;;---> (0.50027466 1.0005188 1.5007629 2.001007)
(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.9 0.75 1.25 1.75 2.25) 0.0001) ;;---> (0.50027466 1.0005188 1.5007629 2.001007)


;;;;;;;;;;;;;;;;
;; APARTADO 3 ;;
;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Finds the middle point between 2 points and returns a sorted list with the
;; input points and the middle point.
;;
;; a: lower extremum of the interval in which we want to find the middle
;; b: b>a upper extremum of the interval in which we want to find the middle
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mid_lst (a b)
  (list a (funcall #'mid a b) b))

(mid_lst 2 3)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Implements the recursive part of allind, thus preventing redundant error
;; checking.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allind_rec (f a b N tol)
  (if (= N 1) (return-from allind_rec (funcall #'mid_lst a b))
    (return-from allind_rec (append
                             (funcall #'allind_rec f a (funcall #'mid a b) (- N 1) tol) 
                             (funcall #'allind_rec f (funcall #'mid a b) b (- N 1) tol)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Divides an interval up to a specified length and find all the roots of
;; the function f in the intervals thus obtained.
;;
;; Parameters:
;;
;; f: function of a single real parameter with real values whose root
;; we want to find
;; a: lower extremum of the interval in which we search for the root
;; b: b>a upper extremum of the interval in which we search for the root
;; N: Exponent of the number of intervals in which [a,b] is to be divided:
;; [a,b] is divided into 2^N intervals
;; tol: tolerance for the stopping criterion: if b-a < tol the function
;; returns (a+b)/2 as a solution.
;;
;; The interval (a,b) is divided in intervals (x[i], x[i+i]) with
;; x[i]= a + i*dlt; a root is sought in each interval, and all the roots
;; thus found are assembled into a list that is returned.
;;
;;
;; Hint:
;; One might find a way to use allroot to implement this function. This is
;; possible, of course, but there is a simple way of doing it recursively
;; without using allroot.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allind (f a b N tol)
  (cond ((null a) (return-from allind NIL))
        ((null b) (return-from allind NIL))
        ((> a b) (return-from allind NIL))
        ((< N 1) (return-from allind NIL))
        ((< tol 0.0) (return-from allind NIL))
        (t (return-from allind
             (funcall #'allroot f (remove-duplicates (funcall #'allind_rec f a b N tol)) tol)))))


(allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 2 0.0001) ;;---> (0.50027084 1.0005027 1.5007347 2.0010324)
