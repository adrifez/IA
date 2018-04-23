(defpackage :grupo02pareja072tq2304 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo

(in-package grupo02pareja072tq2304)

(defun heuristica (estado)
  (let ((f1 (get-fichas (estado-tablero estado) 
                        (estado-lado-sgte-jugador estado) 
                        0))
        (f2 (get-fichas (estado-tablero estado) 
                        (estado-lado-sgte-jugador estado) 
                        1))
        (f3 (get-fichas (estado-tablero estado) 
                        (estado-lado-sgte-jugador estado) 
                        2))
        (f4 (get-fichas (estado-tablero estado) 
                        (estado-lado-sgte-jugador estado) 
                        3))
        (f5 (get-fichas (estado-tablero estado) 
                        (estado-lado-sgte-jugador estado) 
                        4))
        (f6 (get-fichas (estado-tablero estado) 
                        (estado-lado-sgte-jugador estado) 
                        5))
        (f1-op (get-fichas (estado-tablero estado) 
                           (lado-contrario (estado-lado-sgte-jugador estado))
                           0))
        (f2-op (get-fichas (estado-tablero estado) 
                           (lado-contrario (estado-lado-sgte-jugador estado)) 
                           1))
        (f3-op (get-fichas (estado-tablero estado) 
                           (lado-contrario (estado-lado-sgte-jugador estado)) 
                           2))
        (f4-op (get-fichas (estado-tablero estado) 
                           (lado-contrario (estado-lado-sgte-jugador estado)) 
                           3))
        (f5-op (get-fichas (estado-tablero estado) 
                           (lado-contrario (estado-lado-sgte-jugador estado)) 
                           4))
        (f6-op (get-fichas (estado-tablero estado) 
                           (lado-contrario (estado-lado-sgte-jugador estado))
                           5)))
    (cond
     ((and (juego-terminado-p estado) 
           (> (+ (get-tot (estado-lado-sgte-jugador estado))
                 f1 f2 f3 f4 f5 f6)
              (+ (get-tot (lado-contrario (estado-lado-sgte-jugador estado)))
                 f1-op f2-op f3-op f4-op f5-op f6-op)))
      999999999)
     ((and (juego-terminado-p estado) 
           (< (+ (get-tot (estado-lado-sgte-jugador estado))
                 f1 f2 f3 f4 f5 f6)
              (+ (get-tot (lado-contrario (estado-lado-sgte-jugador estado)))
                 f1-op f2-op f3-op f4-op f5-op f6-op)))
      (- 999999999))
     (T (apply #'+ (mapcar #'* 
                     '(-401 387 497 340 -315 -291 -236 -431 -47 414 -169 350 427 362)
                     (list
                      f1
                      f2
                      f3
                      f4
                      f5
                      f6
                      f1-op
                      f2-op
                      f3-op
                      f4-op
                      f5-op
                      f6-op
                      (cuenta-ceros estado 0)
                      (cuenta-ceros estado 1))))))))

(defvar *alias* '|MANCALASGOD_ISBACK|) ; alias que aparecerá en el ranking

(defun cuenta-ceros(estado jugador)
  (if (= jugador 0)
      (apply #'+ (mapcar #'(lambda (pos) 
                             (let 
                                 ((fichas (get-fichas (estado-tablero estado) 
                                                      (estado-lado-sgte-jugador estado) 
                                                      pos)))
                               (if (= fichas 0)
                                   1
                                 0)))
                   '(0 1 2 3 4 5)))
    (apply #'+ (mapcar #'(lambda (pos) 
                           (let 
                               ((fichas (get-fichas (estado-tablero estado) 
                                                    (lado-contrario (estado-lado-sgte-jugador estado)) 
                                                    pos)))
                             (if (= fichas 0)
                                 1
                               0)))
                 '(0 1 2 3 4 5)))))