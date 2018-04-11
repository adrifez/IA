(defpackage :grupo02pareja071tq1104 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo

(in-package grupo02pareja071tq1104)

(defun heuristica (estado)
  (apply 
   #'+ 
   (mapcar #'(lambda (pos) 
               (let ((fichas-op (get-fichas (estado-tablero estado) 
                                            (lado-contrario (estado-lado-sgte-jugador estado)) 
                                            (op-pos pos)))
                     (fichas (get-fichas (estado-tablero estado) 
                                         (estado-lado-sgte-jugador estado) 
                                         pos))
                     (fichas-sim (get-fichas (estado-tablero estado) 
                                         (estado-lado-sgte-jugador estado) 
                                         (sim-pos pos))))
                  (if (= fichas 0)
                      fichas-op
                    (if (= fichas-op 0)
                        (- 0 fichas-sim)
                      (if (= (+ fichas pos) 6)
                          1
                        0)))))
                  '(0 1 2 3 4 5))))

(defvar *alias* '|MANCALASGOD|) ; alias que aparecerá en el ranking

(defun sim-pos (pos)
  (case pos
    (0 4)
    (1 3)
    (2 2)
    (3 1)
    (4 0)
    (5 5)))

(defun op-pos (pos)
  (case pos
    (0 5)
    (1 4)
    (2 3)
    (3 2)
    (4 1)
    (5 0)))