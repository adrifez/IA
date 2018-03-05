;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 1 ;;;
;;;;;;;;;;;;;;;;;;;

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
      nil
    (let ((denom (* (sqrt (pesc-rec x x)) (sqrt (pesc-rec y y)))))
      (if(eql denom 0.0) ;;Si se anula el denominador 
          nil
        (/ (pesc-rec x y) denom)))))


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
   #'+ (* double-float-epsilon double-float-epsilon)
   (mapcar #'* x y)))

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
      NIL
    (let ((denom (* (sqrt (pesc-mapcar x x)) (sqrt (pesc-mapcar y y)))))
      (if(eql denom 0.0) ;;Si se anula el denominador
          nil
        (/ (pesc-mapcar x y) denom)))))


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
      nil 
    (sort
     (copy-list
      (remove-if
       #'(lambda (y) (< (sc-mapcar x y) conf)) vs))
     #'> :key #'(lambda (y) (sc-mapcar x y)))))

;;mejor insert sort

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
      nil
    (first
     (sort
      (copy-list 
       (mapcar #'(lambda (x y) (list (first y) x)) 
         (mapcar #'(lambda (y) (funcall func (rest x) (rest y))) vs) 
         vs))
      #'> :key #'second)))) ;;Ordenamos de mayor a menor segun la similitud coseno

;;mejor recursivo!!!!

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
      nil
    (mapcar #'(lambda (y) (ord-conf y cats func)) texts)))



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





;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 2 ;;;
;;;;;;;;;;;;;;;;;;;

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

(defun bisect-rec (f a b tol)
  (let ((middle (mid a b)))
    (cond ((= (funcall f a) 0.0)
           a)
          ((= (funcall f b) 0.0)
           b)
          ((< (dist a b) tol)
           middle)
          (t
           (if (< (* (funcall f a) (funcall f middle)) 0.0)
               (bisect-rec f a middle tol)
             (bisect-rec f middle b tol))))))

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
  (if (or (> a b) (>= (* (funcall f a) (funcall f b)) 0.0) (< tol 0.0))
      NIL
    (bisect-rec f a b tol)))


(bisect #'(lambda(x) (sin (* 6.26 x))) 0.1 0.7 0.001) ;; --> 0.5016602
(bisect #'(lambda(x) (sin (* 6.26 x))) 0.0 0.7 0.001) ;; --> NIL
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 1.5 0.001) ;; --> NIL
(bisect #'(lambda(x) (sin (* 6.28 x))) 1.1 2.1 0.001) ;; --> NIL



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
  (mapcan #'(lambda (x y)
              (let ((root (bisect f x y tol)))
                (when root
                  (list root))))
    lst
    (rest lst)))


(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.75 1.25 1.75 2.25) 0.0001)
;; --> (0.50027466 1.0005188 1.5007629 2.001007)
(allroot #'(lambda(x) (sin (* 6.28 x))) '(0.25 0.9 0.75 1.25 1.75 2.25) 0.0001)
;; --> (0.50027466 1.0005188 1.5007629 2.001007)
(allroot #'(lambda(x) (cos (* 6.28 x))) '(0.00 0.50 1.00 1.50 2.00) 0.0001)
;; --> (0.2501526 0.7503967 1.2506409 1.750885)
(allroot #'(lambda(x) (cos (* 6.28 x))) '(0.00 0.50 1.00 1.50 2.00 2.50) 0.0001)
;; --> (0.2501526 0.7503967 1.2506409 1.750885 2.2511292)



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
  (list a (mid a b)))

(mid_lst 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Implements the recursive part of allind, thus preventing redundant error
;;; checking.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allind-rec (f a b N tol)
  (if (= N 1)
      (mid_lst a b)
    (let ((middle (mid a b)))
      (append
       (allind-rec f a middle (- N 1) tol) 
       (allind-rec f middle b (- N 1) tol)))))

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
  (cond ((or (null a) (null b) (> a b) (< N 1) (< tol 0.0))
         NIL)
        (t
         (allroot f (append (allind-rec f a b N tol) (list b)) tol))))


(allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 1 0.0001)
;; --> NIL
(allind #'(lambda(x) (sin (* 6.28 x))) 0.1 2.25 2 0.0001)
;; --> (0.50027084 1.0005027 1.5007347 2.0010324)
(allind #'(lambda(x) (cos (* 6.28 x))) 0.0 2.14 3 0.0001)
;; --> (0.25009555 0.7503519 1.2506084 1.7508645)
(allind #'(lambda(x) (cos (* 6.28 x))) 0.0 2.15 4 0.0001)
;; --> (0.2501488 0.75038075 1.2506126 1.7509103)





;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 3 ;;;
;;;;;;;;;;;;;;;;;;;

(defun combine-elt-lst (elt lst)
  (if(or (null elt) (null lst))
      nil
    (mapcar #'(lambda (x) (list elt x)) lst)))


(combine-elt-lst 'a nil) ;; --> NIL
(combine-elt-lst 'a '(1 2 3)) ;; --> ((A 1) (A 2) (A 3))
(combine-elt-lst '4 '(1 2 3)) ;; --> ((4 1) (4 2) (4 3))
(combine-elt-lst nil '(1 2 3)) ;; --> NIL



(defun combine-lst-lst (lst1 lst2)
  (if(or (null lst1) (null lst2))
      nil
    (mapcan #'(lambda (x) (combine-elt-lst x lst2)) lst1)))

;; alternativa recursiva


(combine-lst-lst nil nil) ;; --> NIL
(combine-lst-lst '(a b c) nil) ;; --> NIL
(combine-lst-lst NIL '(a b c)) ;; --> NIL
(combine-lst-lst '(a b c) '(1 2)) ;; --> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))



(defun combine-elt-lst-c (elt lst)
  (if(or (null elt) (null lst))
      nil
  (mapcar #'(lambda (x) (cons elt x)) lst)))


(defun combine-list-of-lsts (lstolsts)
  (if (null lstolsts)
      (list NIL)  
    (mapcan 
        #'(lambda (x) (combine-elt-lst-c
                       x
                       (combine-list-of-lsts (rest lstolsts))))
      (first lstolsts))))

(combine-list-of-lsts '()) ;; --> (NIL)
(combine-list-of-lsts '(() (+ -) (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) () (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) (1 2 3 4) ())) ;; --> NIL
(combine-list-of-lsts '((1 2 3 4))) ;; --> ((1) (2) (3) (4))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;; --> ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;; (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;; (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4) (p o) (i r s)))





;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 4 ;;;
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definicion de simbolos que representan valores de verdad,
;; conectores y predicados para evaluar si una expresion LISP
;; es un valor de verdad o un conector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +bicond+ '<=>)
(defconstant +cond+   '=>)
(defconstant +and+    '^)
(defconstant +or+     'v)
(defconstant +not+    '~)

(defun truth-value-p (x) 
  (or (eql x T) (eql x NIL)))

(defun unary-connector-p (x) 
  (eql x +not+))

(defun binary-connector-p (x) 
  (or (eql x +bicond+) 
      (eql x +cond+)))

(defun n-ary-connector-p (x) 
  (or (eql x +and+) 
      (eql x +or+)))

(defun connector-p (x) 
  (or (unary-connector-p  x)
      (binary-connector-p x)
      (n-ary-connector-p  x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.1
;; Predicado para determinar si una expresion en LISP
;; es un literal positivo 
;;
;; RECIBE   : expresion 
;; EVALUA A : T si la expresion es un literal positivo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun positive-literal-p (x)
  (and (not (truth-value-p x))
       (not (connector-p x))
       (not (listp x))))


;; EJEMPLOS:
(positive-literal-p 'p)
;; evalua a T
(positive-literal-p T)
(positive-literal-p NIL)
(positive-literal-p '~)
(positive-literal-p '=>)
(positive-literal-p '(p))
(positive-literal-p '(~ p))
(positive-literal-p '(~ (v p q)))
;; evaluan a NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.2
;; Predicado para determinar si una expresion
;; es un literal negativo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si la expresion es un literal negativo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun negative-literal-p (x)
  (when (listp x) (and
                   (unary-connector-p (first x))
                   (positive-literal-p (second x))
                   (null (third x)))))

;; EJEMPLOS:
(negative-literal-p '(~ p))        ; T
(negative-literal-p NIL)           ; NIL
(negative-literal-p '~)            ; NIL
(negative-literal-p '=>)           ; NIL
(negative-literal-p '(p))          ; NIL
(negative-literal-p '((~ p)))      ; NIL
(negative-literal-p '(~ T))        ; NIL
(negative-literal-p '(~ NIL))      ; NIL
(negative-literal-p '(~ =>))       ; NIL
(negative-literal-p 'p)            ; NIL
(negative-literal-p '((~ p)))      ; NIL
(negative-literal-p '(~ (v p q)))  ; NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.3
;; Predicado para determinar si una expresion es un literal  
;;
;; RECIBE   : expresion x  
;; EVALUA A : T si la expresion es un literal, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun literal-p (x) 
  (or (positive-literal-p x)
      (negative-literal-p x)))

;; EJEMPLOS:
(literal-p 'p)             
(literal-p '(~ p))      
;;; evaluan a T
(literal-p '(p))
(literal-p '(~ (v p q)))
;;; evaluan a  NIL


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicado para determinar si una expresion esta en formato prefijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato prefijo, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-prefix-p (x)
  (unless (null x)             ;; NIL no es FBF en formato prefijo (por convencion)
    (or (literal-p x)          ;; Un literal es FBF en formato prefijo
        (and (listp x)         ;; En caso de que no sea un literal debe ser una lista
             (let ((connector (first x))
                   (rest_1    (rest  x)))
               (cond
                ((unary-connector-p connector)  ;; Si el primer elemento es un connector unario
                 (and (null (rest rest_1))      ;; deberia tener la estructura (<conector> FBF)
                      (wff-prefix-p (first rest_1)))) 
                ((binary-connector-p connector) ;; Si el primer elemento es un conector binario
                 (let ((rest_2 (rest rest_1)))  ;; deberia tener la estructura 
                   (and (null (rest rest_2))    ;; (<conector> FBF1 FBF2)
                        (wff-prefix-p (first rest_1))
                        (wff-prefix-p (first rest_2)))))               
                ((n-ary-connector-p connector)  ;; Si el primer elemento es un conector enario
                 (or (null rest_1)              ;; conjuncion o disyuncion vacias
                     (and (wff-prefix-p (first rest_1)) ;; tienen que ser FBF los operandos 
                          (let ((rest_2 (rest rest_1)))
                            (or (null rest_2)           ;; conjuncion o disyuncion con un elemento
                                (wff-prefix-p (cons connector rest_2)))))))	
                (t NIL)))))))                   ;; No es FBF en formato prefijo 
;;
;; EJEMPLOS:
(wff-prefix-p '(v))
(wff-prefix-p '(^))
(wff-prefix-p '(v A))
(wff-prefix-p '(^ (~ B)))
(wff-prefix-p '(v A (~ B)))
(wff-prefix-p '(v (~ B) A ))
(wff-prefix-p '(^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E))
;;; evaluan a T
(wff-prefix-p 'NIL)
(wff-prefix-p '(~))
(wff-prefix-p '(=>))
(wff-prefix-p '(<=>))
(wff-prefix-p '(^ (V P (=> A ( B ^ (~ C) ^ D))) (^ (<=> P (~ Q)) P) E))
;;; evaluan a NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.4
;; Predicado para determinar si una expresion esta en formato infijo 
;;
;; RECIBE   : expresion x 
;; EVALUA A : T si x esta en formato infijo, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-p (x)
  (unless (null x)             ;; NIL no es FBF en formato infijo (por convencion)
    (or (literal-p x)          ;; Un literal es FBF en formato infijo
        (and (listp x)         ;; En caso de que no sea un literal debe ser una lista
             (let ((first (first x))
                   (second (first (rest x)))
                   (rest_1 (rest (rest x))))
               (cond
                ((unary-connector-p first)  ;; Si el primer elemento es un connector unario
                 (and (null rest_1)         ;; deberia tener la estructura (<conector> FBF)
                      (wff-infix-p second))) 
                ((binary-connector-p second) ;; Si el segundo elemento es un conector binario
                 (let ((rest_2 (rest rest_1)))  ;; deberia tener la estructura 
                   (and (null rest_2)
                        (wff-infix-p first)
                        (wff-infix-p (first rest_1)))))              
                ((n-ary-connector-p second)  ;; Si el segundo elemento es un conector enario
                 (let ((rest_2 (rest rest_1)))
                   (or (and (null rest_2)  ;; Si esta al final de la lista
                            (wff-infix-p first)
                            (wff-infix-p (first rest_1)))
                       (and (wff-infix-p first) ;; Si esta a mitad de lista
                            (wff-infix-p rest_1)
                            (eql second (first rest_2))))))
                ((n-ary-connector-p first) (null (second x)))  ;; Si solo hay un conector n-ario
                (t NIL)))))))                   ;; No es FBF en formato infijo  

;;
;; EJEMPLOS:
;;
(wff-infix-p 'a) 						; T
(wff-infix-p '(^)) 					; T  ;; por convencion
(wff-infix-p '(v)) 					; T  ;; por convencion
(wff-infix-p '(A ^ (v))) 			      ; T  
(wff-infix-p '( a ^ b ^ (p v q) ^ (~ r) ^ s))  	; T 
(wff-infix-p '(A => B)) 				; T
(wff-infix-p '(A => (B <=> C))) 			; T
(wff-infix-p '( B => (A ^ C ^ D))) 			; T   
(wff-infix-p '( B => (A ^ C))) 			; T 
(wff-infix-p '( B ^ (A ^ C))) 			; T 
(wff-infix-p '((p v (a => (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p ) ^ e))  ; T 
(wff-infix-p nil) 					; NIL
(wff-infix-p '(a ^)) 					; NIL
(wff-infix-p '(^ a)) 					; NIL
(wff-infix-p '(a)) 					; NIL
(wff-infix-p '((a))) 				      ; NIL
(wff-infix-p '((a) b))   			      ; NIL
(wff-infix-p '(^ a b q (~ r) s))  		      ; NIL 
(wff-infix-p '( B => A C)) 			      ; NIL   
(wff-infix-p '( => A)) 				      ; NIL   
(wff-infix-p '(A =>)) 				      ; NIL   
(wff-infix-p '(A => B <=> C)) 		      ; NIL
(wff-infix-p '( B => (A ^ C v D))) 		      ; NIL   
(wff-infix-p '( B ^ C v D )) 			      ; NIL 
(wff-infix-p '((p v (a => e (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p ) ^ e)); NIL 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convierte FBF en formato prefijo a FBF en formato infijo
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF en formato infijo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun prefix-to-infix (wff)
  (when (wff-prefix-p wff)
    (if (literal-p wff)
        wff
      (let ((connector      (first wff))
            (elements-wff (rest wff)))
        (cond
         ((unary-connector-p connector) 
          (list connector (prefix-to-infix (second wff))))
         ((binary-connector-p connector) 
          (list (prefix-to-infix (second wff))
                connector
                (prefix-to-infix (third wff))))
         ((n-ary-connector-p connector) 
          (cond 
           ((null elements-wff)        ;;; conjuncion o disyuncion vacias. 
            wff)                       ;;; por convencion, se acepta como fbf en formato infijo
           ((null (cdr elements-wff))  ;;; conjuncion o disyuncion con un unico elemento
            (prefix-to-infix (car elements-wff)))  
           (t (cons (prefix-to-infix (first elements-wff)) 
                    (mapcan #'(lambda(x) (list connector (prefix-to-infix x))) 
                      (rest elements-wff))))))
         (t NIL)))))) ;; no deberia llegar a este paso nunca

;;
;;  EJEMPLOS:
;;
(prefix-to-infix '(v))          ; (V)
(prefix-to-infix '(^))          ; (^)
(prefix-to-infix '(v a))        ; A
(prefix-to-infix '(^ a))        ; A
(prefix-to-infix '(^ (~ a)))    ; (~ a)
(prefix-to-infix '(v a b))      ; (A v B)
(prefix-to-infix '(v a b c))    ; (A V B V C)
(prefix-to-infix '(^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E))
;;; ((P V (A => (B ^ (~ C) ^ D))) ^ ((P <=> (~ Q)) ^ P) ^ E)
(prefix-to-infix '(^ (v p (=> a (^ b (~ c) d))))) ; (P V (A => (B ^ (~ C) ^ D)))
(prefix-to-infix '(^ (^ (<=> p (~ q)) p ) e))     ; (((P <=> (~ Q)) ^ P) ^ E)  
(prefix-to-infix '( v (~ p) q (~ r) (~ s)))       ; ((~ P) V Q V (~ R) V (~ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.5
;;
;; Convierte FBF en formato infijo a FBF en formato prefijo
;;  
;; RECIBE   : FBF en formato infijo 
;; EVALUA A : FBF en formato prefijo 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun infix-to-prefix (wff)
  (when (wff-infix-p wff)
    (if (literal-p wff)
        wff
      (let ((first (first wff))
            (second (second wff))
            (third (third wff)))
        (cond
         ((unary-connector-p first) 
          (list first
                (infix-to-prefix second)))
         ((binary-connector-p second) 
          (list second
                (infix-to-prefix first)
                (infix-to-prefix third)))
         ((and (n-ary-connector-p first) (null second)) wff)  ;; Si solo hay un conector n-ario
         ((n-ary-connector-p second)
          (cond
            ((null (fourth wff))
             (list second
                   (infix-to-prefix first)
                   (infix-to-prefix third)))
            (t
             (let ((rest (infix-to-prefix (rest (rest wff)))))
               (append (list second)
                       (list (infix-to-prefix first))
                       (rest rest))))))
          (t NIL)))))) ;; no deberia llegar a este paso nunca

;;
;; EJEMPLOS
;;
(infix-to-prefix nil)      ;; NIL
(infix-to-prefix 'a)       ;; a
(infix-to-prefix '((a)))   ;; NIL
(infix-to-prefix '(a))     ;; NIL
(infix-to-prefix '(((a)))) ;; NIL
(infix-to-prefix '(a v b v c v d)) ;; (V A B C D)

(prefix-to-infix (infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p) ^ e)) ) 
;;-> ((P V (A => (B ^ (~ C) ^ D))) ^ ((P <=> (~ Q)) ^ P) ^ E)


(infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^  ((p <=> (~ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E)

(infix-to-prefix '(~ ((~ p) v q v (~ r) v (~ s))))
;; (~ (V (~ P) Q (~ R) (~ S)))


(infix-to-prefix
 (prefix-to-infix
  '(V (~ P) Q (~ R) (~ S))))
;;-> (V (~ P) Q (~ R) (~ S))

(infix-to-prefix
 (prefix-to-infix
  '(~ (V (~ P) Q (~ R) (~ S)))))
;;-> (~ (V (~ P) Q (~ R) (~ S)))


(infix-to-prefix 'a)  ; A
(infix-to-prefix '((p v (a => (b ^ (~ c) ^ d))) ^  ((p <=> (~ q)) ^ p) ^ e))  
;; (^ (V P (=> A (^ B (~ C) D))) (^ (<=> P (~ Q)) P) E)

(infix-to-prefix '(~ ((~ p) v q v (~ r) v (~ s))))
;; (~ (V (~ P) Q (~ R) (~ S)))

(infix-to-prefix  (prefix-to-infix '(^ (v p (=> a (^ b (~ c) d)))))) ; '(v p (=> a (^ b (~ c) d)))
(infix-to-prefix  (prefix-to-infix '(^ (^ (<=> p (~ q)) p ) e))) ; '(^ (^ (<=> p (~ q)) p ) e))  
(infix-to-prefix (prefix-to-infix '( v (~ p) q (~ r) (~ s))))  ; '( v (~ p) q (~ r) (~ s)))
;;;

(infix-to-prefix '(p v (a => (b ^ (~ c) ^ d)))) ; (V P (=> A (^ B (~ C) D)))
(infix-to-prefix '(((P <=> (~ Q)) ^ P) ^ E))  ; (^ (^ (<=> P (~ Q)) P) E)
(infix-to-prefix '((~ P) V Q V (~ R) V (~ S))); (V (~ P) Q (~ R) (~ S))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.1.6
;; Predicado para determinar si una FBF es una clausula  
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : T si FBF es una clausula, NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-aux (lst)
  (if (null lst)
      T
    (and
     (literal-p (first lst))
     (clause-aux (rest lst)))))

(clause-aux '(a b (~ c)))


(defun clause-p (wff)
  (when (wff-prefix-p wff)
    (when (not (literal-p wff))
      (and
       (eq (first wff) +or+)
       (clause-aux (rest wff))))))

;;
;; EJEMPLOS:
;;
(clause-p '(v))             ; T
(clause-p '(v p))           ; T
(clause-p '(v (~ r)))       ; T
(clause-p '(v p q (~ r) s)) ; T
(clause-p NIL)                    ; NIL
(clause-p 'p)                     ; NIL
(clause-p '(~ p))                 ; NIL
(clause-p NIL)                    ; NIL
(clause-p '(p))                   ; NIL
(clause-p '((~ p)))               ; NIL
(clause-p '(^ a b q (~ r) s))     ; NIL
(clause-p '(v (^ a b) q (~ r) s)) ; NIL
(clause-p '(~ (v p q)))           ; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 1.7
;; Predicado para determinar si una FBF esta en FNC  
;;
;; RECIBE   : FFB en formato prefijo 
;; EVALUA A : T si FBF esta en FNC con conectores, 
;;            NIL en caso contrario. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnf-aux (lst)
  (if (null lst)
      T
    (and
     (clause-p (first lst))
     (cnf-aux (rest lst)))))


(defun cnf-p (wff)
  (when (wff-prefix-p wff)
    (when (not (literal-p wff))
        (and
         (eq (first wff) +and+)
         (cnf-aux (rest wff))))))

;;
;; EJEMPLOS:
;;
(cnf-p '(^ (v a  b c) (v q r) (v (~ r) s) (v a b))) ; T
(cnf-p '(^ (v a  b (~ c)) ))                        ; T
(cnf-p '(^ ))                                       ; T
(cnf-p '(^(v )))                                    ; T
(cnf-p '(~ p))                                      ; NIL
(cnf-p '(^ a b q (~ r) s))                          ; NIL
(cnf-p '(^ (v a b) q (v (~ r) s) a b))              ; NIL
(cnf-p '(v p q (~ r) s))                            ; NIL
(cnf-p '(^ (v a b) q (v (~ r) s) a b))              ; NIL
(cnf-p '(^ p))                                      ; NIL
(cnf-p '(v ))                                       ; NIL
(cnf-p NIL)                                         ; NIL
(cnf-p '((~ p)))                                    ; NIL
(cnf-p '(p))                                        ; NIL
(cnf-p '(^ (p)))                                    ; NIL
(cnf-p '((p)))                                      ; NIL
(cnf-p '(^ a b q (r) s))                            ; NIL
(cnf-p '(^ (v a  (v b c)) (v q r) (v (~ r) s) a b)) ; NIL
(cnf-p '(^ (v a (^ b c)) (^ q r) (v (~ r) s) a b))  ; NIL
(cnf-p '(~ (v p q)))                                ; NIL
(cnf-p '(v p q (r) s))                              ; NIL 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.1: Incluya comentarios en el codigo adjunto
;;
;; Dada una FBF, evalua a una FBF equivalente 
;; que no contiene el connector <=>
;;
;; RECIBE   : FBF en formato prefijo 
;; EVALUA A : FBF equivalente en formato prefijo 
;;            sin connector <=>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eliminate-biconditional (wff)
  (if (or (null wff) (literal-p wff)) ;; Caso base
      wff
    (let ((connector (first wff)))
      (if (eq connector +bicond+) ;; Si encuentra el conector <=>
          (let ((wff1 (eliminate-biconditional (second wff)))
                (wff2 (eliminate-biconditional (third  wff))))
            (list +and+ ;; Descompone <=> en dos => conjuntas
                  (list +cond+ wff1 wff2)
                  (list +cond+ wff2 wff1)))
        (cons connector ;; Si no hay <=>
              (mapcar #'eliminate-biconditional (rest wff))))))) ;; Continua buscando <=>

;;
;; EJEMPLOS:
;;
(eliminate-biconditional '(<=> p  (v q s p) ))
;;   (^ (=> P (v Q S P)) (=> (v Q S P) P))
(eliminate-biconditional '(<=>  (<=> p  q) (^ s (~ q))))
;;   (^ (=> (^ (=> P Q) (=> Q P)) (^ S (~ Q)))
;;      (=> (^ S (~ Q)) (^ (=> P Q) (=> Q P))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.2
;; Dada una FBF, que contiene conectores => evalua a
;; una FBF equivalente que no contiene el connector =>
;;
;; RECIBE   : wff en formato prefijo sin el connector <=> 
;; EVALUA A : wff equivalente en formato prefijo 
;;            sin el connector =>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-conditional (wff)  
  (if (or (null wff) (literal-p wff)) ;; Caso base
      wff
    (let ((connector (first wff)))
      (if (eq connector +cond+) ;; Si encuentra el conector =>
          (let ((wff1 (eliminate-conditional (second wff)))
                (wff2 (eliminate-conditional (third  wff))))
            (list +or+ ;; Descompone (A => B) en (~A v B)
                  (list +not+ wff1)
                  wff2))
        (cons connector ;; Si no hay =>
              (mapcar #'eliminate-conditional (rest wff))))))) ;; Continua buscando =>       

;;
;; EJEMPLOS:
;;
(eliminate-conditional '(=> p q))                      ;;; (V (~ P) Q)
(eliminate-conditional '(=> p (v q s p)))              ;;; (V (~ P) (V Q S P))
(eliminate-conditional '(=> (=> (~ p) q) (^ s (~ q)))) ;;; (V (~ (V (~ (~ P)) Q)) (^ S (~ Q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.3
;; Dada una FBF, que no contiene los conectores <=>, => 
;; evalua a una FNF equivalente en la que la negacion  
;; aparece unicamente en literales negativos
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, => 
;; EVALUA A : FBF equivalente en formato prefijo en la que 
;;            la negacion  aparece unicamente en literales 
;;            negativos.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun exchange-and-or (connector)
  (cond
   ((eq connector +and+) +or+)    
   ((eq connector +or+) +and+)
   (t connector)))

(defun invert (lst)
  (cond
   ((null lst) ;; Condicion de parada de conectores n-arios
    NIL)
   ((positive-literal-p lst) ;; Caso base con literal positivo
    (list +not+ lst))
   ((negative-literal-p lst) ;; Caso base con literal negativo
    (second lst))
   ((n-ary-connector-p lst) ;; Caso con conector n-ario
    (exchange-and-or lst))
   ((unary-connector-p (first lst)) ;; Expresion negada
    (second lst))
   (t
    (cons (invert (first lst)) (invert (rest lst))))))

(invert '(~ (^ (v (~ a) b c) d)))


(defun reduce-scope-of-negation-aux (lst)
  (cond ((null lst)
         NIL)
        ((literal-p lst)
         lst)
        ((unary-connector-p (first lst))
         (invert (second lst)))
        (t
         (cons (first lst) (mapcar
                               #'reduce-scope-of-negation-aux
                             (rest lst))))))

(defun reduce-scope-of-negation (wff)
  (when (wff-prefix-p wff)
    (reduce-scope-of-negation-aux wff)))


;;
;;  EJEMPLOS:
;;
(reduce-scope-of-negation '(~ (v p (~ q) r))) 
;;; (^ (~ P) Q (~ R))
(reduce-scope-of-negation '(~ (^ p (~ q) (v  r s (~ a))))) 
;;;  (V (~ P) Q (^ (~ R) (~ S) A))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.4: Comente el codigo adjunto 
;;
;; Dada una FBF, que no contiene los conectores <=>, => en la 
;; que la negacion aparece unicamente en literales negativos
;; evalua a una FNC equivalente en FNC con conectores ^, v  
;;
;; RECIBE   : FBF en formato prefijo sin conector <=>, =>, 
;;            en la que la negacion aparece unicamente 
;;            en literales negativos
;; EVALUA A : FBF equivalente en formato prefijo FNC 
;;            con conectores ^, v
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-elt-lst (elt lst) ;; Funcion para combinar un elemento con una lista
  (if (null lst)
      (list (list elt))
    (mapcar #'(lambda (x) (cons elt x)) lst)))

(defun exchange-NF (nf) ;; Funcion para cambiar de forma normal
  (if (or (null nf) (literal-p nf)) ;; Caso base
      nf
    (let ((connector (first nf)))
      (cons (exchange-and-or connector) ;; Cambiamos el conector
            (mapcar #'(lambda (x) ;; Concatenamos el conector con el resultado del resto
                          (cons connector x))
                (exchange-NF-aux (rest nf)))))))

(defun exchange-NF-aux (nf) ;; Funcion auxiliar de NF
  (if (null nf)  ;; Caso base
      NIL
    (let ((lst (first nf)))
      (mapcan #'(lambda (x)
                  (combine-elt-lst ;; Combinamos x con el resto de la expresion 
                   x 
                   (exchange-NF-aux (rest nf)))) 
        (if (literal-p lst) (list lst) (rest lst)))))) ;; Evaluamos un literal o una expresion

(defun simplify (connector lst-wffs) ;; Funcion para simplificar respecto a un conector
  (if (literal-p lst-wffs) ;; Caso base
      lst-wffs                    
    (mapcan #'(lambda (x) ;; Para cada elemento de la lista  
                (cond 
                 ((literal-p x) (list x)) ;; Si es un literal, devolvemos una lista que lo contiene
                 ((equal connector (first x)) ;; Si el primer elemento igual a connector
                  (mapcan ;; Simplificas cada elemento del resto de la lista
                      #'(lambda (y) (simplify connector (list y))) 
                    (rest x))) 
                 (t (list x)))) ;; En el resto de casos devuelve una lista que lo contiene              
      lst-wffs)))

(defun cnf (wff) ;; Funcion para pasar una FBF a FNC
  (cond
   ((cnf-p wff) wff) ;; Si ya esta en FNC
   ((literal-p wff) ;; Si es un literal
    (list +and+ (list +or+ wff)))
   ((let ((connector (first wff))) 
      (cond
       ((equal +and+ connector) ;; Si el conector es un and
        (cons +and+ (simplify +and+ (mapcar #'cnf (rest wff)))))
       ((equal +or+ connector) ;; Si el conector es un or
        (cnf (exchange-NF (cons +or+ (simplify +or+ (rest wff)))))))))))


(cnf 'a)

(cnf '(v (~ a) b c))
(print (cnf '(^ (v (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(v (^ (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))
(print (cnf '(^ (v p  (~ q)) a (v k  r  (^ m  n)))))
(print (cnf '(v p  q  (^ r  m)  (^ n  a)  s )))
(exchange-NF '(v p  q  (^ r  m)  (^ n  a)  s ))
(cnf '(^ (v a b (^ y r s) (v k l)) c (~ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (v a b (^ y r s)) c (~ d) (^ e f (v h i) (^ o p))))
(cnf '(^ (^ y r s (^ p q (v c d))) (v a b)))
(print (cnf '(^ (v (~ a) b c) (~ e) r s 
                (v e f (~ g) h) k (v m n) d)))
;;
(cnf '(^ (v p (~ q)) (v k r (^ m  n))))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (~ b) c) (^ d s))))
(print (cnf '(^ (^ (~ y) (v r (^ s (~ x)) (^ (~ p) m (v c d))) (v (~ a) (~ b))) g)))
;;
;; EJEMPLOS:
;;
(cnf NIL)              ; NIL
(cnf 'a)               ; (^ (V A))
(cnf '(~ a))           ; (^ (V (~ A)))
(cnf '(V (~ P) (~ P))) ; (^ (V (~ P) (~ P)))
(cnf '(V A))           ; (^ (V A))
(cnf '(^ (v p (~ q)) (v k r (^ m  n))))
;;;   (^ (V P (~ Q)) (V K R M) (V K R N))
(print  (cnf '(v (v p q) e f (^ r  m) n (^ a (~ b) c) (^ d s))))
;;; (^ (V P Q E F R N A D)      (V P Q E F R N A S)
;;;    (V P Q E F R N (~ B) D)  (V P Q E F R N (~ B) S)
;;;    (V P Q E F R N C D)      (V P Q E F R N C S) 
;;;    (V P Q E F M N A D)      (V P Q E F M N A S) 
;;;    (V P Q E F M N (~ B) D)  (V P Q E F M N (~ B) S) 
;;;    (V P Q E F M N C D)      (V P Q E F M N C S))
;;;
(print 
 (cnf '(^ (^ (~ y) (v r (^ s (~ x)) 
                      (^ (~ p) m (v c d)))(v (~ a) (~ b))) g)))
;;;(^ (V (~ Y)) (V R S (~ P)) (V R S M) 
;;;   (V R S C D) (V R (~ X) (~ P)) 
;;;   (V R (~ X) M) (V R (~ X) C D)
;;;   (V (~ A) (~ B)) (V G))  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.5:
;;
;; Dada una FBF en  FNC
;; evalua a lista de listas sin conectores
;; que representa una conjuncion de disyunciones de literales
;;
;; RECIBE   : FBF en FNC con conectores ^, v
;; EVALUA A : FBF en FNC (con conectores ^, v eliminaos)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-connectors-aux (lst)
  (cond
   ((null lst)
    NIL)
   ((literal-p lst)
    lst)
   ((n-ary-connector-p (first lst))
    (eliminate-connectors-aux (rest lst)))
   (t
    (cons (eliminate-connectors-aux (first lst))
          (eliminate-connectors-aux (rest lst))))))


(defun eliminate-connectors (cnf)
  (when (wff-prefix-p cnf)
    (eliminate-connectors-aux cnf)))


(eliminate-connectors 'nil)
(eliminate-connectors (cnf '(^ (v p  (~ q))  (v k  r  (^ m  n)))))
(eliminate-connectors
 (cnf '(^ (v (~ a) b c) (~ e) (^ e f (~ g) h) (v m n) (^ r s q) (v u q) (^ x y))))

(eliminate-connectors (cnf '(v p  q  (^ r  m)  (^ n  q)  s )))
(eliminate-connectors (print (cnf '(^ (v p  (~ q)) (~ a) (v k  r  (^ m  n))))))

(eliminate-connectors '(^))
(eliminate-connectors '(^ (v p (~ q)) (v) (v k r)))
(eliminate-connectors '(^ (v a b)))

;;   EJEMPLOS:
;;

(eliminate-connectors '(^ (v p (~ q)) (v k r)))
;; ((P (~ Q)) (K R))
(eliminate-connectors '(^ (v p (~ q)) (v q (~ a)) (v s e f) (v b)))
;; ((P (~ Q)) (Q (~ A)) (S E F) (B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.2.6
;; Dada una FBF en formato infijo
;; evalua a lista de listas sin conectores
;; que representa la FNC equivalente
;;
;; RECIBE   : FBF 
;; EVALUA A : FBF en FNC (con conectores ^, v eliminados)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wff-infix-to-cnf (wff)
  (when (wff-infix-p wff)
    (eliminate-connectors
     (cnf
      (reduce-scope-of-negation
       (eliminate-conditional
        (eliminate-biconditional (infix-to-prefix wff))))))))


;;
;; EJEMPLOS:
;; 
(wff-infix-to-cnf 'a)
(wff-infix-to-cnf '(~ a))
(wff-infix-to-cnf  '( (~ p) v q v (~ r) v (~ s)))
(wff-infix-to-cnf  '((p v (a => (b ^ (~ c) ^ d))) ^ ((p <=> (~ q)) ^ p) ^ e))
;; ((P (~ A) B) (P (~ A) (~ C)) (P (~ A) D) ((~ P) (~ Q)) (Q P) (P) (E))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.1
;; eliminacion de literales repetidos una clausula 
;; 
;; RECIBE   : K - clausula (lista de literales, disyuncion implicita)
;; EVALUA A : clausula equivalente sin literales repetidos 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun clause-lst-p (lst)
  (if (null lst)
      T
    (when (list lst)
      (when (literal-p (first lst))
        (if (null (rest lst))
            T
          (clause-aux (rest lst)))))))
  

(defun eliminate-repeated-literals-aux (lst)
  (let ((first (first lst))
          (rest (rest lst)))
      (when (literal-p first)
        (if (member first rest :test #'equal)
            (eliminate-repeated-literals-aux rest)
          (cons first
                (eliminate-repeated-literals-aux rest))))))

(defun eliminate-repeated-literals (k)
  (when (clause-lst-p k)
    (eliminate-repeated-literals-aux k)))

;;
;; EJEMPLO:
;;
(eliminate-repeated-literals '(a b (~ c) (~ a) a c (~ c) c a))
;;;   (B (~ A) (~ C) C A)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.2
;; eliminacion de clausulas repetidas en una FNC 
;; 
;; RECIBE   : cnf - FBF en FNC (lista de clausulas, conjuncion implicita)
;; EVALUA A : FNC equivalente sin clausulas repetidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cnf-lst-p (lst)
  (when (list lst)
    (when (clause-lst-p (first lst))
      (if (null (rest lst))
          T
        (cnf-lst-p (rest lst))))))

(cnf-lst-p '((a) (a b) ((~ c))))

(defun contain-clause-aux (c1 c2)
  (if (null c1)
      T
    (let ((elmnt (find (first c1) c2 :test #'equal)))
      (if elmnt
          (contain-clause-aux (rest c1) c2)
        NIL))))


(defun contain-clause (c1 c2)
  (if (null c1)
      (list NIL)
    (when (and (clause-lst-p c1) (clause-lst-p c2))
      (if (contain-clause-aux c1 c2)
          T
        NIL))))


(defun equal-clause (c1 c2)
  (when (and (clause-lst-p c1) (clause-lst-p c2))
    (let ((c1-aux (eliminate-repeated-literals c1))
          (c2-aux (eliminate-repeated-literals c2)))
      (and (contain-clause c1-aux c2-aux)
           (contain-clause c2-aux c1-aux)))))


(defun eliminate-repeated-clauses-aux (lst)
  (if (null lst)
      NIL
    (when (clause-lst-p (first lst))
      (let ((first (eliminate-repeated-literals (first lst)))
            (rest (rest lst)))
        (if (member first rest :test #'equal-clause)
            (eliminate-repeated-clauses-aux rest)
          (cons first
                (eliminate-repeated-clauses-aux rest)))))))

(defun eliminate-repeated-clauses (cnf) 
  (when (cnf-lst-p cnf)
    (eliminate-repeated-clauses-aux cnf)))

;;
;; EJEMPLO:
;;
(eliminate-repeated-clauses '(((~ a) c) (c (~ a)) ((~ a) (~ a) b c b) (a a b) (c (~ a) b  b) (a b)))
;;; ((C (~ A)) (C (~ A) B) (A B))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.3
;; Predicado que determina si una clausula subsume otra
;;
;; RECIBE   : K1, K2 clausulas
;; EVALUA a : K1 si K1 subsume a K2
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subsume-aux (K1 K2)
  (if (null K1)
      T
    (let ((elmnt (find (first K1) K2 :test #'equal)))
      (if elmnt
          (subsume-aux (rest K1) K2)
        NIL))))

(defun subsume (K1 K2)
  (when (and (clause-lst-p K1) (clause-lst-p K2))
    (if (subsume-aux K1 K2)
        (list K1)
      NIL)))
  
;;
;;  EJEMPLOS:
;;
(subsume '(a) '(a b (~ c)))
;; ((a))
(subsume NIL '(a b (~ c)))
;; (NIL)
(subsume '(a b (~ c)) '(a) )
;; NIL
(subsume '( b (~ c)) '(a b (~ c)) )
;; (( b (~ c)))
(subsume '(a b (~ c)) '( b (~ c)))
;; NIL
(subsume '(a b (~ c)) '(d  b (~ c)))
;; nil
(subsume '(a b (~ c)) '((~ a) b (~ c) a))
;; ((A B (~ C)))
(subsume '((~ a) b (~ c) a) '(a b (~ c)) )
;; nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.4
;; eliminacion de clausulas subsumidas en una FNC 
;; 
;; RECIBE   : K (clausula), cnf (FBF en FNC)
;; EVALUA A : FBF en FNC equivalente a cnf sin clausulas subsumidas 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun subsume-lst (lst clause)
  (when lst
    (if (equal (first lst) 'NIL)
        T
      (or (and (not (eq (first lst) clause))
             (not (null (subsume (first lst) clause))))
        (subsume-lst (rest lst) clause)))))
    

(subsume-lst '((a b c) (b c) (a (~ c) b)  ((~ a) b) (a b (~ a))) '(c b))

(defun eliminate-subsumed-clauses-aux (aux cnf)
  (if (null aux)
      NIL
    (if (subsume-lst cnf (first aux))
        (eliminate-subsumed-clauses-aux (rest aux) cnf)
      (cons (first aux)
            (eliminate-subsumed-clauses-aux (rest aux) cnf)))))

(defun eliminate-subsumed-clauses (cnf) 
  (when (cnf-lst-p cnf)
    (eliminate-subsumed-clauses-aux cnf cnf)))

;;
;;  EJEMPLOS:
;;
(eliminate-subsumed-clauses 
 '((a b c) (b c) (a (~ c) b)  ((~ a) b) (a b (~ a)) (c b a)))
;;; ((A (~ C) B) ((~ A) B) (B C)) ;; el orden no es importante
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (~ c) b) (b)  ((~ a) b) (a b (~ a)) (c b a)))
;;; ((B))
(eliminate-subsumed-clauses
 '((a b c) (b c) (a (~ c) b) ((~ a))  ((~ a) b) (a b (~ a)) (c b a)))
;;; ((A (~ C) B) ((~ A)) (B C))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.5
;; Predicado que determina si una clausula es tautologia
;;
;; RECIBE   : K (clausula)
;; EVALUA a : T si K es tautologia
;;            NIL en caso contrario
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun conjugado (lit1 lit2) ;; T si son dos literales conjugados
  (when (and (literal-p lit1) (literal-p lit2))
    (or (and (positive-literal-p lit1)
             (negative-literal-p lit2)
             (equal lit1 (second lit2)))
        (and (negative-literal-p lit1)
             (positive-literal-p lit2)
             (equal (second lit1) lit2)))))
  

(defun tautology-p-aux (lst)
  (unless (null lst)
    (or (member (first lst) (rest lst) :test #'conjugado)
        (not (null (tautology-p-aux (rest lst)))))))

(defun tautology-p (K) 
  (when (clause-lst-p K)
    (tautology-p-aux K)))

;;
;;  EJEMPLOS:
;;
(tautology-p '((~ B) A C (~ A) D)) ;;; T 
(tautology-p '((~ B) A C D))       ;;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.6
;; eliminacion de clausulas en una FBF en FNC que son tautologia
;;
;; RECIBE   : cnf - FBF en FNC
;; EVALUA A : FBF en FNC equivalente a cnf sin tautologias 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eliminate-tautologies-aux (lst)
  (if (null lst)
      NIL
    (if (tautology-p (first lst))
        (eliminate-tautologies-aux (rest lst))
      (cons (first lst)
            (eliminate-tautologies-aux (rest lst))))))

(defun eliminate-tautologies (cnf) 
  (when (cnf-lst-p cnf)
    (eliminate-tautologies-aux cnf)))

;;
;;  EJEMPLOS:
;;
(eliminate-tautologies 
 '(((~ b) a) (a (~ a) b c) ( a (~ b)) (s d (~ s) (~ s)) (a)))
;; (((~ B) A) (A (~ B)) (A))

(eliminate-tautologies '((a (~ a) b c)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.3.7
;; simplifica FBF en FNC 
;;        * elimina literales repetidos en cada una de las clausulas 
;;        * elimina clausulas repetidas
;;        * elimina tautologias
;;        * elimina clausulass subsumidas
;;  
;; RECIBE   : cnf  FBF en FNC
;; EVALUA A : FNC equivalente sin clausulas repetidas, 
;;            sin literales repetidos en las clausulas
;;            y sin clausulas subsumidas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun simplify-cnf-aux (lst)
  (eliminate-subsumed-clauses
   (eliminate-tautologies
    (eliminate-repeated-clauses
     (mapcar #'eliminate-repeated-literals lst)))))

(defun simplify-cnf (cnf) 
  (when (cnf-lst-p cnf)
    (simplify-cnf-aux cnf)))

;;
;;  EJEMPLOS:
;;
(simplify-cnf '((a a) (b) (a) ((~ b)) ((~ b)) (a b c a)  (s s d) (b b c a b)))
;; ((B) ((~ B)) (S D) (A)) ;; en cualquier orden


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.1
;; Construye el conjunto de clausulas lambda-neutras para una FNC 
;;
;; RECIBE   : cnf    - FBF en FBF simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(0) subconjunto de clausulas de cnf  
;;            que no contienen el literal lambda ni ~lambda   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun equal-literal (lit1 lit2)
  (or (equal lit1 lit2) (conjugado lit1 lit2)))

(defun extract-neutral-clauses-aux (lit lst)
  (if (null lst)
      NIL
    (when (clause-lst-p (first lst))
      (if (member lit (first lst) :test #'equal-literal)
          (extract-neutral-clauses-aux lit (rest lst))
        (cons (first lst)
              (extract-neutral-clauses-aux lit (rest lst)))))))

(defun extract-neutral-clauses (lambda cnf)
  (when (and (positive-literal-p lambda) (cnf-lst-p cnf))
    (extract-neutral-clauses-aux lambda cnf)))

;;
;;  EJEMPLOS:
;;
(extract-neutral-clauses 'p
                           '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((R (~ S) Q) ((~ R) S))

(extract-neutral-clauses 'r NIL)
;; NIL

(extract-neutral-clauses 'r '(NIL))
;; (NIL)

(extract-neutral-clauses 'r
                           '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((P Q) (A B P) (A (~ P) C))

(extract-neutral-clauses 'p
                           '((p (~ q) r) (p q) (r (~ s) p q) (a b p) (a (~ p) c) ((~ r) p s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.2
;; Construye el conjunto de clausulas lambda-positivas para una FNC
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo
;; EVALUA A : cnf_lambda^(+) subconjunto de clausulas de cnf 
;;            que contienen el literal lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-lit-clauses-aux (lit lst)
  (if (null lst)
      NIL
    (when (clause-lst-p (first lst))
      (if (member lit (first lst) :test #'equal)
          (cons (first lst) (extract-lit-clauses-aux lit (rest lst)))
      (extract-lit-clauses-aux lit (rest lst))))))

(defun extract-positive-clauses (lambda cnf) 
  (when (and (positive-literal-p lambda) (cnf-lst-p cnf))
    (extract-lit-clauses-aux lambda cnf)))

;;
;;  EJEMPLOS:
;;
(extract-positive-clauses 'p
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))

;; ((P (~ Q) R) (P Q) (A B P))


(extract-positive-clauses 'r NIL)
;; NIL
(extract-positive-clauses 'r '(NIL))
;; NIL
(extract-positive-clauses 'r
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((P (~ Q) R) (R (~ S) Q))
(extract-positive-clauses 'p
                             '(((~ p) (~ q) r) ((~ p) q) (r (~ s) (~ p) q) (a b (~ p)) ((~ r) (~ p) s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.3
;; Construye el conjunto de clausulas lambda-negativas para una FNC 
;;
;; RECIBE   : cnf    - FBF en FNC simplificada
;;            lambda - literal positivo 
;; EVALUA A : cnf_lambda^(-) subconjunto de clausulas de cnf  
;;            que contienen el literal ~lambda  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun extract-negative-clauses (lambda cnf) 
  (when (and (positive-literal-p lambda) (cnf-lst-p cnf))
    (extract-lit-clauses-aux (list +not+ lambda) cnf)))

;;
;;  EJEMPLOS:
;;
(extract-negative-clauses 'p
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; ((A (~ P) C))

(extract-negative-clauses 'r NIL)
;; NIL
(extract-negative-clauses 'r '(NIL))
;; NIL
(extract-negative-clauses 'r
                             '((p (~ q) r) (p q) (r (~ s) q) (a b p) (a (~ p) c) ((~ r) s)))
;; (((~ R) S))
(extract-negative-clauses 'p
                             '(( p (~ q) r) ( p q) (r (~ s) p q) (a b p) ((~ r) p s)))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.4
;; resolvente de dos clausulas
;;
;; RECIBE   : lambda      - literal positivo
;;            K1, K2      - clausulas simplificadas
;; EVALUA A : res_lambda(K1,K2) 
;;                        - lista que contiene la 
;;                          clausula que resulta de aplicar resolucion 
;;                          sobre K1 y K2, con los literales repetidos 
;;                          eliminados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun union-aux (LL1 LL2 lambda flag)
  (let ((L1 (copy-list (first LL1)))
        (L2 (copy-list (first LL2)))
        (lambda-conj (invert lambda)))
  (if (= flag 1)
      (list (union (remove lambda L1) (remove lambda-conj L2 :test #'equal)))
    (list (union (remove lambda-conj L1 :test #'equal) (remove lambda L2))))))

(defun resolve-on (lambda K1 K2) 
  (if (or (null K1) (null K2))
      '()
    (let ((pos1 (extract-positive-clauses lambda (list K1)))
          (neg1 (extract-negative-clauses lambda (list K2)))
          (pos2 (extract-positive-clauses lambda (list K2)))
          (neg2 (extract-negative-clauses lambda (list K1))))
      (cond
       ((and pos1 neg1)
        (union-aux pos1 neg1 lambda 1)) ;res on lambda:lambda-conj
       ((and pos2 neg2)
        (union-aux neg2 pos2 lambda 2)) ;res on lambda-conj:lambda
       (T NIL))))) ;Solo llega aqui si no es posible la resolucion
;;
;;  EJEMPLOS:
;;
(resolve-on 'p '(a b (~ c) p) '((~ p) b a q r s))
;; (((~ C) B A Q R S))

(resolve-on 'p '(a b (~ c) (~ p)) '(p b a q r s))
;; (((~ C) B A Q R S))

(resolve-on 'p '(p) '((~ p)))
;; (NIL)

(resolve-on 'p NIL '(p b a q r s))
;; NIL

(resolve-on 'p NIL NIL)
;; NIL

(resolve-on 'p '(a b (~ c) (~ p)) '(p b a q r s))
;; (((~ C) B A Q R S))

(resolve-on 'p '(a b (~ c)) '(p b a q r s))
;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.4.5
;; Construye el conjunto de clausulas RES para una FNC 
;;
;; RECIBE   : lambda - literal positivo
;;            cnf    - FBF en FNC simplificada
;;            
;; EVALUA A : RES_lambda(cnf) con las clauses repetidas eliminadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-RES (lambda cnf)
  (if (null cnf)
      NIL
    (let ((pos (extract-positive-clauses lambda cnf))
          (neg (extract-negative-clauses lambda cnf))
          (neu (extract-neutral-clauses lambda cnf)))
      (eliminate-repeated-clauses 
       (append 
        neu 
        (mapcan #'(lambda(x) 
                    (mapcan #'(lambda (y) (resolve-on lambda x y)) neg))
          pos))))))
;;
;;  EJEMPLOS:
;;
(build-RES 'p NIL)
;; NIL
(build-RES 'P '((A  (~ P) B) (A P) (A B)));; ((A B))
(build-RES 'P '((B  (~ P) A) (A P) (A B)));; ((B A))

(build-RES 'p '(NIL))
;; (NIL)

(build-RES 'p '((p) ((~ p))))
;; (NIL)

(build-RES 'q '((p q) ((~ p) q) (a b q) (p (~ q)) ((~ p) (~ q))))
;; ((P) ((~ P) P) ((~ P)) (B A P) (B A (~ P)))

(build-RES 'p '((p q) (c q) (a b q) (p (~ q)) (p (~ q))))
;; ((A B Q) (C Q))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.5
;; Comprueba si una FNC es SAT calculando RES para todos los
;; atomos en la FNC 
;;
;; RECIBE   : cnf - FBF en FNC simplificada
;; EVALUA A :	T  si cnf es SAT
;;                NIL  si cnf es UNSAT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-same-cl (K1 K2) ;Comprueba si dos clausulas son equivalentes
  (if (and (null K1) (null K2))
      T
    (if (member (first K1) K2 :test #'equal)
        (check-same-cl (rest K1) 
                       (remove 
                        (first K1) 
                        (copy-list K2) 
                        :count 1 :test #'equal))
      NIL)))

(defun check-same-list (l1 l2) ;Comprueba si dos listas tienen las mismas
  (if (null l2)                ;cláusulas, o si la segunda está contenida en la
      T                        ;primera
    (if (member (first l2) l1 :test #'check-same-cl)
        (check-same-list (remove 
                          (first l2) 
                          (copy-list l1) 
                          :count 1 :test #'check-same-cl)
                         (rest l2))
      NIL)))

(defun extract-atoms-aux (list)
  (if (null list) 
      NIL
    (if (positive-literal-p (first list))
        (cons (first list) (extract-atoms-aux (rest list)))
      (extract-atoms-aux (rest list)))))

(defun extract-atoms (lol)
  (if (null lol) 
      NIL
    (eliminate-repeated-literals (mapcan #'extract-atoms-aux lol))))

(defun  RES-SAT-p (cnf) 
  (if (null cnf)
      T
    (if (member nil cnf)
        NIL
      (let ((lista (mapcar #'(lambda (x) 
                               (build-RES x cnf))
                     (extract-atoms cnf))))
        (if (member '(nil) lista :test #'equal) ;Si hemos derivado la cláusula vacía
            NIL
          (let ((l-new-knowlg (simplify-cnf (apply #'append lista))))
            (if (null l-new-knowlg) ;Cláusula vacía
                NIL
              (if (check-same-list cnf l-new-knowlg) ;Si no hemos adquirido nuevo
                  T                                  ;conocimiento
                (RES-SAT-p (union ;Si hemos adquirido nuevo conocimiento seguimos
                            l-new-knowlg 
                            cnf 
                            :test #'check-same-cl))))))))))
;;
;;  EJEMPLOS:
;;
;;
;; SAT Examples
;;
(RES-SAT-p nil)  ;;; T
(RES-SAT-p '((p) ((~ q)))) ;;; T 
(RES-SAT-p
 '((a b d) ((~ p) q) ((~ c) a b) ((~ b) (~ p) d) (c d (~ a)))) ;;; T 
(RES-SAT-p
 '(((~ p) (~ q) (~ r)) (q r) ((~ q) p) ((~ q)) ((~ p) (~ q) r))) ;;;T
;;
;; UNSAT Examples
;;
(RES-SAT-p '(nil))         ;;; NIL
(RES-SAT-p '((S) nil))     ;;; NIL 
(RES-SAT-p '((p) ((~ p)))) ;;; NIL
(RES-SAT-p
 '(((~ p) (~ q) (~ r)) (q r) ((~ q) p) (p) (q) ((~ r)) ((~ p) (~ q) r))) ;;; NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EJERCICIO 4.6:
;; Resolucion basada en RES-SAT-p
;;
;; RECIBE   : wff - FBF en formato infijo 
;;            w   - FBF en formato infijo 
;;                               
;; EVALUA A : T   si w es consecuencia logica de wff
;;            NIL en caso de que no sea consecuencia logica.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun logical-consequence-RES-SAT-p (wff w)
  (if (or (null wff) (eq w nil))
      NIL
    (let ((fnc (append           ;Incluimos la negación de la meta en la
                (simplify-cnf    ;base de conocimiento
                 (wff-infix-to-cnf 
                  (prefix-to-infix 
                   (reduce-scope-of-negation
                    (list +not+ (eliminate-conditional 
                                 (eliminate-biconditional 
                                  (infix-to-prefix w)))))))) 
                (simplify-cnf 
                 (wff-infix-to-cnf wff)))))
      (if (eq (RES-SAT-p fnc) nil) ;Si derivamos la cláusula vacía con
          T                        ;w negado, entonces es consecuencia lógica
        NIL))))                    ;si no, no
;;
;;  EJEMPLOS:
;;
(logical-consequence-RES-SAT-p NIL 'a) ;;; NIL
(logical-consequence-RES-SAT-p NIL NIL) ;;; NIL
(logical-consequence-RES-SAT-p '(q ^ (~ q)) 'a) ;;; T 
(logical-consequence-RES-SAT-p '(q ^ (~ q)) '(~ a)) ;;; T 

(logical-consequence-RES-SAT-p '((p => (~ p)) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => (~ p)) ^ p) '(~ q))
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) 'q)
;; T

(logical-consequence-RES-SAT-p '((p => q) ^ p) '(~ q))
;; NIL

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => (a v (~ b))) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 '(~ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => (a v (~ b))) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 'a)
;; T

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 'a)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 '(~ a))
;; T

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 'q)
;; NIL

(logical-consequence-RES-SAT-p 
 '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
 '(~ q))
;; NIL

(or 
 (logical-consequence-RES-SAT-p '((p => q) ^ p) '(~q))      ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((~ p) => q) ^ (p => ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
  'a) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
  'q) ;; NIL
 (logical-consequence-RES-SAT-p 
  '(((~ p) => q) ^ (p <=> ((~ a) ^ b)) ^ ( (~ p) => (r  ^ (~ q)))) 
  '(~ q))) ;;NIL





;;;;;;;;;;;;;;;;;;;
;;; EJERCICIO 5 ;;;
;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net) ;queue es una lista de listas con la raiz como elemento de la lista contenida
  (if (null queue) '() ;si la cola se queda vacia, la meta no es accesible desde el nodo origen
    (let* ((path (first queue)) ;en vez de guardar solo un nodo en la cola, guarda una lista con el nodo a explorar en la primera posicion y el camino para llegar a el en las siguientes
           (node (first path))) ;node es el nodo que sale de la cola y cuyos vecinos se van explorar
      (if (eql node end) ;si el nodo es la meta devolvemos el camino hasta la misma
          (reverse path) ;teniendo en cuenta que el camino que teniamos estaba al reves ya que el primer nodo era el que nos tocaba explorar y el ultimo el de partida
        (bfs end
             (append (rest queue)
                     (new-paths path node net)) ;encolamos todos los adyacentes al nodo en el que nos encontramos
             net)))))

(defun new-paths (path node net) ;devuelve una lista con todos los caminos a explorar desde un nodo
  (mapcar #'(lambda(n)           ;es decir, todos los caminos a los que dan lugar sus nodos adyacentes
              (cons n path))
    (rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bfs 'd '((c)) '((a d) (b d f) (c e) (d f) (e b f) (f)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;;Lo hace porque BFS es un algoritmo que nos asegura encontrar el camino más corto entre dos nodos
;;(al menos si suponemos que todos los enlaces tienen el mismo coste), ya que los nodos se van explorando
;;según cuan "cerca" se encuentran del nodo de partida (es decir, el algoritmo funciona encolando
;;los nodos adyacentes a los nodos extraídos de la cola)

(shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))
;;(bfs 'f '((a)) '((a d) (b d f) (c e) (d f) (e b f) (f)))
;;   (new-paths '(a) 'a '((a d) (b d f) (c e) (d f) (e b f) (f))) -> ((d a))
;;(bfs 'f '((d a)) '((a d) (b d f) (c e) (d f) (e b f) (f)))
;;   (new-paths '(d a) 'd '((a d) (b d f) (c e) (d f) (e b f) (f))) -> ((f d a))
;;(bfs 'f '((f d a)) '((a d) (b d f) (c e) (d f) (e b f) (f)))
;;   (eql node end) -> T, devuelve (reverse path)=(a d f)

(shortest-path 'f 'c 
               '((a b c d e) (b a d e f) (c a g) (d a b g h) (e a b g h) (f b h) (g c d e h) (h d e f g)))
;;Output: (F B A C)

;;Falla porque no etiqueta los nodos como visitados, por ejemplo
;;en ((a d) (b d f) (c e) (d f) (e b f) (f a)), la llamada
;;(shortest-path 'a 'b '((a d) (b d f) (c e) (d f) (e b f) (f a)))
;;Error: Stack overflow (signal 1000)
;;[condition type: SYNCHRONOUS-OPERATING-SYSTEM-SIGNAL]

(defun bfs-improved (end queue net)
  (if (null queue) '()
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
        (if (member node (rest path)) ;De esta manera evitamos los bucles
            '()
          (bfs-improved end
                        (append (rest queue)
                                (new-paths path node net))
                        net))))))

(defun shortest-path-improved (start end net)
  (bfs-improved end (list (list start)) net))

(shortest-path-improved 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))
(shortest-path-improved 'a 'b '((a d) (b d f) (c e) (d f) (e b f) (f a)))