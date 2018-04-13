(defpackage :grupo02pareja072tq1204 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la funci�n de evaluaci�n
  (:export :heuristica :*alias*))   ; heur�stica y un alias para el torneo

(in-package grupo02pareja072tq1204)

(defun heuristica (estado)
  (- (cuenta-fichas (estado-tablero estado) 
                    (estado-lado-sgte-jugador estado) 
                    1)
     (cuenta-fichas (estado-tablero estado) 
                    (lado-contrario (estado-lado-sgte-jugador estado)) 
                    1)))

(defvar *alias* '|MANCALASGOD_ROADTOCHALLENGER|) ; alias que aparecer� en el ranking