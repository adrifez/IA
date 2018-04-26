(defpackage :grupo02pareja072tq2504 ; se declara un paquete lisp que usa common-lisp
  (:use :common-lisp :mancala)      ; y mancala, y exporta la función de evaluación
  (:export :heuristica :*alias*))   ; heurística y un alias para el torneo

(in-package grupo02pareja072tq2504)

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
                             5))
          (kl (get-tot (estado-lado-sgte-jugador estado)))
          (kl-op (get-tot (lado-contrario (estado-lado-sgte-jugador estado)))))
      (cond
       ((and (juego-terminado-p estado) 
             (> (+ kl f1 f2 f3 f4 f5 f6)
                (+ kl-op f1-op f2-op f3-op f4-op f5-op f6-op)))
        95534)
       ((and (juego-terminado-p estado) 
             (< (+ kl f1 f2 f3 f4 f5 f6)
                (+ kl-op f1-op f2-op f3-op f4-op f5-op f6-op)))
        (- 95534))
       (T (apply #'+ (mapcar #'* 
                       '(-352 -3 358 -165 -300 -496 -215 396 303 143 357 375 113 -409 -277 -157)                       (list
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
                        kl
                        kl-op
                        (cuenta-ceros estado 0)
                        (cuenta-ceros estado 1))))))))

(defvar *alias* '|MR_POOPYBUTTHOLE|) ; alias que aparecerá en el ranking

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