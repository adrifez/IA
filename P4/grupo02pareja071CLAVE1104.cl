(defpackage :grupo14pareja023Xf0604 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala) ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*)) ; heurística y un alias para el torneo
(in-package grupo14pareja023Xf0604)
(defun heuristica (estado) …) ; función de evaluación heurística a implementar
(defvar *alias* '|MANCALASGOD|) ; alias que aparecerá en el ranking