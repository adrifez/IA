(defpackage :grupo02pareja073tq1204 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo

(in-package grupo02pareja073tq1204)

(defun heuristica (estado)
  (apply 
   #'+ 
   (mapcar #'(lambda (pos) 
               (let ((fichas-op (get-fichas (estado-tablero estado) 
                                            (lado-contrario (estado-lado-sgte-jugador estado)) 
                                            (op-pos pos)))
                     (fichas (get-fichas (estado-tablero estado) 
                                         (estado-lado-sgte-jugador estado) 
                                         pos)))
               (if (= fichas 0)
                 (if (= fichas-op 0)
                     0
                   1)
                 (if (= (+ fichas pos) 6)
                     (if (= (+ fichas-op (op-pos pos)) 6)
                         0
                       1)
                   0))))
   '(0 1 2 3 4))))

(defvar *alias* '|ELPKDOISCOMING|) ; alias que aparecerá en el ranking

(defun op-pos (pos)
  (case pos
    (0 5)
    (1 4)
    (2 3)
    (3 2)
    (4 1)
    (5 0)))