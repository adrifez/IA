;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;1.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Calcula el producto escalar de dos vectores recursivamente
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pesc-rec (x y)
  (if (or (null x) (null y))
      0
    (+ (* (first x) (first y)) (pesc-rec (rest x) (rest y)))))

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
    (if(eql denom 0.0)
        (return-from sc-rec nil)
      (/ (pesc-rec x y) denom))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;1.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Calcula el producto escalar de dos vectores con mapcar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pesc-mapcar (x y)
  (apply 
   #'+ (mapcar #'(lambda (x y) (* x y)) x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-mapcar (x y)
;;; Calcula la similitud coseno de un vector usando mapcar
;;;
;;; INPUT: x: vector, representado como una lista
;;; y: vector, representado como una lista
;;;
;;; OUTPUT: similitud coseno entre x e y
;;;
(defun sc-mapcar (x y)
  (if(or (null x) (null y))
      (return-from sc-mapcar NIL))
  (let ((denom (* (sqrt (pesc-mapcar x x)) (sqrt (pesc-mapcar y y)))))
    (if(eql denom 0.0)
        (return-from sc-mapcar nil)
      (/ (pesc-mapcar x y) denom))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-conf (x vs conf)
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT: x: vector, representado como una lista
;;; vs: vector de vectores, representado como una lista de listas
;;; conf: Nivel de confianza
;;; OUTPUT: Vectores cuya similitud es superior al nivel de confianza, ordenados
;;;
(defun sc-conf (x vs conf)
  (if(or (or (null x) (null vs)) (or (> conf 1) (< conf 0)))
      (return-from sc-conf nil)) 
  (sort
   (copy-list
    (remove-if
     #'(lambda (y) (< (sc-mapcar x y) conf)) vs)) #'> :key #'(lambda (y) (sc-mapcar x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;1.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Asocia cada ID a la similitud coseno con el vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ord-conf (x vs func)
  (if(or (or (null x) (null vs)) (null func))
      (return-from ord-conf nil))
  (first
   (sort
   (copy-list 
    (mapcar #'(lambda (x y) (cons x (first y))) 
      (mapcar #'(lambda (y) (funcall func (rest x) (rest y))) vs) 
      vs))
    #'> :key #'first)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sc-classifier (cats texts func)
;; Clasifica a los textos en categorías.
;;;
;;; INPUT: cats: vector de vectores, representado como una lista de listas
;;; vs: vector de vectores, representado como una lista de listas
;;; func: referencia a función para evaluar la similitud coseno
;;; OUTPUT: Pares identificador de categoría con resultado de similitud coseno
;;;
(defun sc-classifier (cats texts func)
  (if(or (or (null cats) (null texts)) (null func))
      (return-from sc-classifier nil))
  (mapcar #'(lambda (y) (ord-conf y cats func)) texts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;1.4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(time (sc-classifier '((1 43 23 12) (2 33 54 24) (3 4 3 1) (4 2 1 2) (5 2 1 2) (6 2 1 2) (7 2 1 2)) '((1 3 22 134) (2 43 26 58) (3 23 21 83) (4 12 32 1 4)) #'sc-rec))
(time (sc-classifier '((1 43 23 12) (2 33 54 24) (3 4 3 1) (4 2 1 2) (5 2 1 2) (6 2 1 2) (7 2 1 2)) '((1 3 22 134) (2 43 26 58) (3 23 21 83) (4 12 32 1 4)) #'sc-mapcar))