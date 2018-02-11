;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 1.1 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Calcula el producto escalar de dos vectores recursivamente
;;;
;;;INPUT: x: vector, representado por una lista
;;;y: vector, representado como una lista
;;;
;;;OUTPUT: producto escalar de x por y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pesc-rec (x y)
  (if (or (null x) (null y))
      0
    (+ (* (first x) (first y)) (pesc-rec (rest x) (rest y)))))

(pesc-rec '(1 2 3) '(3 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-rec (x y)
;;; Calcula la similitud coseno de un vector de forma recursiva
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-rec (x y)
  (if(or (null x) (null y))
      (return-from sc-rec nil))
  (let ((denom (* (sqrt (pesc-rec x x)) (sqrt (pesc-rec y y)))))
    (if(eql denom 0.0) ;;Si se anula el denominador 
        (return-from sc-rec nil)
      (/ (pesc-rec x y) denom))))


(sc-rec '(0 0 0) '(1 2 3))
(sc-rec '(1 2 3) '(1 2 3))
(sc-rec '(1 2 3) '(0 0 0))
(sc-rec '(1 2 3) '(-3 -2 -1))
(sc-rec '(1 2) '(-2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Calcula el producto escalar de dos vectores mediante mapcar
;;;
;;;INPUT: x: vector, representado por una lista
;;;y: vector, representado como una lista
;;;
;;;OUTPUT: producto escalar de x por y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pesc-mapcar (x y)
  (apply 
   #'+ (mapcar #'(lambda (x y) (* x y)) x y)))

(pesc-mapcar '(1 2 3) '(3 2 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-mapcar (x y)
;;; Calcula la similitud coseno de un vector usando mapcar
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-mapcar (x y)
  (if(or (null x) (null y))
      (return-from sc-mapcar NIL))
  (let ((denom (* (sqrt (pesc-mapcar x x)) (sqrt (pesc-mapcar y y)))))
    (if(eql denom 0.0) ;;Si se anula el denominador
        (return-from sc-mapcar nil)
      (/ (pesc-mapcar x y) denom))))


(sc-rec '(1 0) '(1 0)) ;; --> 1.0
(sc-rec '(1 0) '(0 1)) ;; --> 0.0
(sc-rec '(1 0) '(1 1)) ;; --> 0.70710677
(sc-mapcar '(1 0) '(1 0)) ;; --> 1.0
(sc-mapcar '(1 0) '(0 1)) ;; --> 0.0
(sc-mapcar '(1 0) '(1 1)) ;; --> 0.70710677

(sc-rec '(1 2 3) '(3 2 1)) ;; --> 0.7142857
(sc-mapcar '(1 2 3) '(3 2 1)) ;; --> 0.7142857

(sc-rec '(3 4) '(1 0)) ;; --> 0.6
(sc-rec '(3 4) '(3 0)) ;; --> 0.6
(sc-mapcar '(3 4) '(1 0)) ;; --> 0.6
(sc-mapcar '(3 4) '(3 0)) ;; --> 0.6


;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 1.2 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (x vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud es superior al nivel de confianza, ordenados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-conf (x vs conf)
  (if(or (or (null x) (null vs)) (or (> conf 1) (< conf 0)))
      (return-from sc-conf nil)) 
  (sort
   (copy-list
    (remove-if
     #'(lambda (y) (<= (sc-mapcar x y) conf)) vs)) #'> :key #'(lambda (y) (sc-mapcar x y))))


(sc-conf '(1 0) '((1 0.2) (1 0.4) (1 0.6) (1 0.8) (1 1))  1)
;; --> NIL
(sc-conf '(1 0) '((1 0) (1 0.2) (1 0.4) (1 0.6) (1 0.8) (1 1))  1)
;; --> ((1 0))
(sc-conf '(1 0) '((1 0) (1 0.2) (1 0.4) (1 0.6) (1 0.8) (1 1))  0.95)
;; --> ((1 0) (1 0.2))
(sc-conf '(1 0) '((1 0) (1 0.2) (1 0.4) (1 0.6) (1 0.8) (1 1))  0.9)
;; --> ((1 0) (1 0.2) (1 0.4))
(sc-conf '(1 0) '((1 0) (1 0.2) (1 0.4) (1 0.6) (1 0.8) (1 1))  0.8)
;; --> ((1 0) (1 0.2) (1 0.4) (1 0.6))
(sc-conf '(1 0) '((1 0) (1 0.2) (1 0.4) (1 0.6) (1 0.8) (1 1))  0.75)
;; --> ((1 0) (1 0.2) (1 0.4) (1 0.6) (1 0.8))
(sc-conf '(1 0) '((1 0) (1 0.2) (1 0.4) (1 0.6) (1 0.8) (1 1))  0.7)
;; --> ((1 0) (1 0.2) (1 0.4) (1 0.6) (1 0.8) (1 1))


;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 1.3 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Asocia un vector con la categoria mas parecida
;;;
;;;INPUT: x: vector al que asociar, lista
;;;vs: vector de vectores categoria, lista
;;;func: funcion con la que calcular la similitud coseno
;;;
;;;OUTPUT: ID del vector de categoria mas parecido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ord-conf (x vs func)
  (if(or (or (null x) (null vs)) (null func))
      (return-from ord-conf nil))
  (first
   (sort
    (copy-list 
     (mapcar #'(lambda (x y) (list (first y) x)) 
       (mapcar #'(lambda (y) (funcall func (rest x) (rest y))) vs) 
       vs))
    #'> :key #'second))) ;;Ordenamos de mayor a menor segun la similitud coseno


(ord-conf '(1 2 3) '((1 2 1) (2 2 3) (3 4 5)) #'sc-mapcar)
(ord-conf '(1 2 3) '((1 2 1) (2 2 3) (3 4 5)) #'sc-rec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;; Clasifica a los textos en categorías.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; vs: vector de vectores, representado como una lista de listas
;;; func: referencia a función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sc-classifier (cats texts func)
  (if(or (or (null cats) (null texts)) (null func))
      (return-from sc-classifier nil))
  (mapcar #'(lambda (y) (ord-conf y cats func)) texts))


;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 1.4 ;;;
;;;;;;;;;;;;;;;;;;;;

(setf cats '((1 43 23 12) (2 33 54 24)))
(setf texts '((1 3 22 134) (2 43 26 58)))
(sc-classifier cats texts #'sc-rec) ;; -> ((2 0.48981872) (1 0.81555086))
(sc-classifier cats texts #'sc-mapcar) ;; -> ((2 0.48981872) (1 0.81555086))


(time 
 (sc-classifier 
  '((1 43 23 12) (2 33 54 24) (3 4 3 1) (4 2 1 2) (5 2 1 2) (6 2 1 2) (7 2 1 2)) 
  '((1 3 22 134) (2 43 26 58) (3 23 21 83) (4 12 32 1 4)) 
  #'sc-rec))
(time 
 (sc-classifier 
  '((1 4 3 1 93 1 23 9) (2 2 1 2 2321 21 21 3)) 
  '((1 3 22 134 122 41 48 4) (2 43 26 58 3 4 5 6)) 
  #'sc-rec))
(time 
 (sc-classifier 
  '((1 4 3 1 93 1 23 9 43 34) (2 2 1 2 2321 21 21 3 12 43)) 
  '((1 3 22 134 122 41 48 4 12 34) (2 43 26 58 3 4 5 6 95 23)) 
  #'sc-rec))

(time 
 (sc-classifier 
  '((1 43 23 12) (2 33 54 24) (3 4 3 1) (4 2 1 2) (5 2 1 2) (6 2 1 2) (7 2 1 2)) 
  '((1 3 22 134) (2 43 26 58) (3 23 21 83) (4 12 32 1 4)) 
  #'sc-mapcar))
(time 
 (sc-classifier 
  '((1 4 3 1 93 1 23 9) (2 2 1 2 2321 21 21 3)) 
  '((1 3 22 134 122 41 48 4) (2 43 26 58 3 4 5 6)) 
  #'sc-mapcar))
(time 
 (sc-classifier 
  '((1 4 3 1 93 1 23 9 234 12) (2 2 1 2 2321 21 21 3 21 34)) 
  '((1 3 22 134 122 41 48 44 23 12) (2 43 26 58 3 4 5 6 12 34)) 
  #'sc-mapcar))


;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 2.1 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finds the middle point between 2 points.
;;;
;;; INPUT: a: lower extremum of the interval in which we want to find the middle
;;; b: b>a upper extremum of the interval in which we want to find the middle
;;;
;;; OUTPUT: Middle point of a and b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mid (a b)
  (float (/ (+ a b) 2)))

(mid 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finds the distance between 2 points.
;;;
;;; INPUT: a: lower extremum of the interval which length we want to find out
;;; b: b>a upper extremum of the interval which length we want to find out
;;;
;;; OUTPUT: distance between a and b
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dist (a b)
  (abs (- a b)))

(dist 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implements the recursive part of bisect, thus preventing redundant error
;;; checking.
;;; 
;;; INPUT: f: function
;;; a: lower extremum of the interval
;;; b: upper extremum of the interval
;;; tol: allowed tolerance
;;;
;;; OUTPUT: root in the interval
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bisect_rec (f a b tol)
  (let ((middle (funcall #'mid a b)))
    (cond ((= (funcall f a) 0.0) (return-from bisect_rec a))
          ((= (funcall f b) 0.0) (return-from bisect_rec b))
          ((< (funcall #'dist a b) tol) (return-from bisect_rec middle))
          (t (if (< (* (funcall f a) (funcall f middle)) 0.0)
                 (return-from bisect_rec (funcall #'bisect_rec f a middle tol))
               (return-from bisect_rec (funcall #'bisect_rec f middle b tol)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finds a root of f between the points a and b using bisection.
;;;
;;; If f(a)f(b)>=0 there is no guarantee that there will be a root in the
;;; interval, and the function will return NIL.
;;;
;;; INPUT:
;;; f: function of a single real parameter with real values whose root
;;; we want to find
;;; a: lower extremum of the interval in which we search for the root
;;; b: b>a upper extremum of the interval in which we search for the root
;;; tol: tolerance for the stopping criterion: if b-a < tol the function
;;; returns (a+b)/2 as a solution.
;;;
;;; OUTPUT: Root of the function, or NIL if no root
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bisect (f a b tol)
  (cond ((> a b) (return-from bisect NIL))
        ((>= (* (funcall f a) (funcall f b)) 0.0) (return-from bisect NIL))
        ;; Si la linea de arriba fuera asi, obtendriamos la solucion en el caso de que a o b inicales fueran la solucion
        ;; ((> (* (funcall f a) (funcall f b)) 0.0) (return-from bisect NIL))
        ((< tol 0.0) (return-from bisect NIL))
        (t (return-from bisect (funcall #'bisect_rec f a b tol)))))


(bisect #'(lambda(x) (sin (* 6.26 x))) 0.1 0.7 0.001) ;; --> 0.5020995
(bisect #'(lambda(x) (sin (* 6.26 x))) 0.0 0.7 0.001) ;; --> NIL
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 1.5 0.001) ;; --> NIL
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 2.1 0.001) ;; --> NIL


;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 2.2 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implements the recursive part of allroot, thus preventing redundant error
;;; checking.
;;;
;;; INPUT: f: function
;;; lst: list of values
;;; tol: tolerance
;;;
;;; OUTPUT: roots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allroot_rec (f lst tol)
  (cond ((null lst) (return-from allroot_rec NIL))
        ((null (rest lst)) (return-from allroot_rec NIL))
        (t (let ((bisec (funcall #'bisect f (first lst) (first (rest lst)) tol))
                 (allroot (funcall #'allroot_rec f (rest lst) tol)))
             (if (eql bisec nil)
                 (return-from allroot_rec allroot)
               (return-from allroot_rec (cons bisec allroot)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finds all the roots that are located between consecutive values of a list
;;; of values
;;;
;;; Parameters:
;;;
;;; f: function of a single real parameter with real values whose root
;;; we want to find
;;; lst: ordered list of real values (lst[i] < lst[i+1])
;;; tol: tolerance for the stopping criterion: if b-a < tol the function
;;; returns (a+b)/2 as a solution.
;;;
;;; Whenever sgn(f(lst[i])) != sgn(f(lst[i+1])) this function looks for a
;;; root in the corresponding interval.
;;;
;;; Returns:
;;; A list o real values containing the roots of the function in the
;;; given sub-intervals
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allroot (f lst tol)
  (if (< tol 0.0) (return-from allroot nil)
    (return-from allroot (funcall #'allroot_rec f lst tol))))


(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001)
;; --> (0.50027466 1.0005188 1.5007629 2.001007)
(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.9 0.75 1.25 1.75 2.25) 0.0001)
;; --> (0.50027466 1.0005188 1.5007629 2.001007)
(allroot #'(lambda(x) (cos (* 6.28 x))) '(0.00 0.50 1.00 1.50 2.00) 0.0001)
;; --> (0.2501526 0.7503967 1.2506409 1.750885)
(allroot #'(lambda(x) (cos (* 6.28 x))) '(0.00 0.50 1.00 1.50 2.00 2.50) 0.0001)
;; --> (0.2501526 0.7503967 1.2506409 1.750885 2.2511292)


;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 2.3 ;;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Finds the middle point between 2 points and returns a sorted list with the
;;; first input point and the middle point.
;;;
;;; a: lower extremum of the interval in which we want to find the middle
;;; b: b>a upper extremum of the interval in which we want to find the middle
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mid_lst (a b)
  (list a (funcall #'mid a b)))

(mid_lst 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implements the recursive part of allind, thus preventing redundant error
;;; checking.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allind_rec (f a b N tol)
  (if (= N 1) (return-from allind_rec (funcall #'mid_lst a b))
    (let ((middle (funcall #'mid a b)))
      (return-from allind_rec (append
                             (funcall #'allind_rec f a middle (- N 1) tol) 
                             (funcall #'allind_rec f middle b (- N 1) tol))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Divides an interval up to a specified length and find all the roots of
;;; the function f in the intervals thus obtained.
;;;
;;;  INPUT:
;;; f: function of a single real parameter with real values whose root
;;; we want to find
;;; a: lower extremum of the interval in which we search for the root
;;; b: b>a upper extremum of the interval in which we search for the root
;;; N: Exponent of the number of intervals in which [a,b] is to be divided:
;;; [a,b] is divided into 2^N intervals
;;; tol: tolerance for the stopping criterion: if b-a < tol the function
;;; returns (a+b)/2 as a solution.
;;;
;;; The interval (a,b) is divided in intervals (x[i], x[i+i]) with
;;; x[i]= a + i*dlt; a root is sought in each interval, and all the roots
;;; thus found are assembled into a list that is returned.
;;;
;;; OUTPUT: List with all the found roots.
;;;
;;; Hint:
;;; One might find a way to use allroot to implement this function. This is
;;; possible, of course, but there is a simple way of doing it recursively
;;; without using allroot.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allind (f a b N tol)
  (cond ((null a) (return-from allind NIL))
        ((null b) (return-from allind NIL))
        ((> a b) (return-from allind NIL))
        ((< N 1) (return-from allind NIL))
        ((< tol 0.0) (return-from allind NIL))
        (t (return-from allind
             (funcall #'allroot f (append (funcall #'allind_rec f a b N tol) (list b)) tol)))))


(allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 1 0.0001)
;; --> NIL
(allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 2 0.0001)
;; --> (0.50027084 1.0005027 1.5007347 2.0010324)
(allind #'(lambda(x) (cos (* 6.28 x))) 0.0 2.14 3 0.0001)
;; --> (0.25009555 0.7503519 1.2506084 1.7508645)
(allind #'(lambda(x) (cos (* 6.28 x))) 0.0 2.15 4 0.0001)
;; --> (0.2501488 0.75038075 1.2506126 1.7509103)


;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 3.1 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun combine-elt-lst (elt lst)
  (if(or (null elt) (null lst))
      (return-from combine-elt-lst nil))
  (mapcar #'(lambda (x) (list elt x)) lst))


(combine-elt-lst 'a nil) ;; --> NIL
(combine-elt-lst 'a '(1 2 3)) ;; --> ((A 1) (A 2) (A 3))
(combine-elt-lst '4 '(1 2 3)) ;; --> ((4 1) (4 2) (4 3))
(combine-elt-lst nil '(1 2 3)) ;; --> NIL


;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 3.2 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun combine-lst-lst (lst1 lst2)
  (if(or (null lst1) (null lst2))
      (return-from combine-lst-lst nil))
  (mapcan #'(lambda (x) (combine-elt-lst x lst2)) lst1))


(combine-lst-lst nil nil) ;; --> NIL
(combine-lst-lst '(a b c) nil) ;; --> NIL
(combine-lst-lst NIL '(a b c)) ;; --> NIL
(combine-lst-lst '(a b c) '(1 2)) ;; --> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))

;;;;;;;;;;;;;;;;;;;;
;;; APARTADO 3.3 ;;;
;;;;;;;;;;;;;;;;;;;;

(defun combine-lst-lsts (lst1 lsts)
  (cond
   ((null lst1) nil) ;;Lista vacia al principio
   ((null lsts) lst1) ;;Caso de una sola lista
   ((and (rest lsts) (null (second lsts))) nil) ;;Si alguna lista es nil o vacia, equivalente
   ((not (null (second lsts))) ;;Caso recursivo, vamos construyendo las listas con la siguiente lista
    (combine-lst-lsts
     (mapcan
       #'(lambda (x) 
           (mapcar #'(lambda (y) (if (listp x)
                                     (append x (list y))
                                   (append (list x) (list y)))) 
           (first lsts))) 
       lst1) 
     (rest lsts)))
   (T (mapcan #'(lambda (x) (mapcar #'(lambda (y) (append x (list y))) (first lsts))) lst1)))) ;;Ultimo caso


(defun combine-list-of-lsts (lstolsts)
  (if (null lstolsts) ;;Caso de lista de listas vacia
      (return-from combine-list-of-lsts (list nil)))
  (combine-lst-lsts (mapcar #'(lambda (x) (list x)) (first lstolsts)) (rest lstolsts))) ;;Le pasamos una lista 


(combine-list-of-lsts '(() (+ -) (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) () (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) (1 2 3 4) ())) ;; --> NIL
(combine-list-of-lsts '((1 2 3 4))) ;; --> ((1) (2) (3) (4))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;; --> ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;; (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;; (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))
(combine-list-of-lsts '())
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4) (p o) (i r s)))