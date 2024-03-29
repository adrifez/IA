(use-package 'mancala)

;(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;;; ------------------------------------------------------------------------------------------
;;; FUNCIONES DE DEFINICION DE JUGADORES Y PARTIDAS DE PRUEBA
;;; ------------------------------------------------------------------------------------------

;;; f-juego que utiliza busqueda negamax (con o sin poda)
;;; ------------------------------------------------------------------------------------------
(defun f-j-nmx (estado profundidad-max f-eval)
;;;(negamax-a-b estado profundidad-max f-eval))
  (negamax estado profundidad-max f-eval))

;;; f-juego controlado por un humano
;;; ------------------------------------------------------------------------------------------
;(defun f-j-humano (estado &optional profundidad-max f-eval)
;  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
;  (setq *verjugada* t)
;  (let ((accion (pide-accion (acciones-posibles estado))))
;    (unless (null accion) (ejecuta-accion estado accion))))

(defvar *jdr-humano* (make-jugador
                        :nombre   '|Humano|
                        :f-juego  #'f-j-humano
                        :f-eval   nil))

(defvar *jdr-human2* (make-jugador
                        :nombre   '|Human2|
                        :f-juego  #'f-j-humano
                        :f-eval   nil))

;;; f-juego para un jugador que realiza movimientos aleatorios
;;; ------------------------------------------------------------------------------------------
(defun f-j-aleatorio (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (let ((lst-acciones (acciones-posibles estado)))
    (ejecuta-accion estado (nth (random (length lst-acciones)) lst-acciones))))

;;; Jugador que no evalua y juega aleatoriamente
;;; ------------------------------------------------------------------------------------------
(defvar *jdr-aleatorio* (make-jugador
                        :nombre   '|Ju-Aleatorio|
                        :f-juego  #'f-j-aleatorio
                        :f-eval   nil))

;;; f-juego para un jugador que siempre juega la primera opcion
;;; ------------------------------------------------------------------------------------------
(defun f-j-1st-opt (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (ejecuta-accion estado (first (acciones-posibles estado))))

;;; Jugador que no evalua y juega la primera opcion disponible
;;; ------------------------------------------------------------------------------------------
(defvar *jdr-1st-opt* (make-jugador
                        :nombre   '|Ju-1st-Opt|
                        :f-juego  #'f-j-1st-opt
                        :f-eval   nil))

;;; f-juego para un jugador que siempre juega la ultima opcion
;;; ----------------------------------------------------------
(defun f-j-last-opt (estado &optional profundidad-max f-eval)
  (and profundidad-max f-eval)      ; dummy to avoid compiler warnings
  (ejecuta-accion estado (car (last (acciones-posibles estado)))))

;;; Jugador que no evalua y juega la segunda opcion disponible (si puede)
;;; ------------------------------------------------------------------------------------------
(defvar *jdr-last-opt* (make-jugador
                        :nombre   '|Ju-Last-Opt|
                        :f-juego  #'f-j-last-opt
                        :f-eval   nil))

;;; Funcion de evaluacion que da al estado recibido una puntuacion aleatoria
;;; ------------------------------------------------------------------------------------------
(defun f-eval-aleatoria (estado)
  (when estado t)                   ; dummy para evitar warnings de compilador
  (random 100) )

;;; f-juego que utiliza busqueda negamax pero evalua aleatoriamente
;;; ------------------------------------------------------------------------------------------
(defvar *jdr-nmx-eval-aleatoria* (make-jugador
                        :nombre   '|Ju-Nmx-Eval-Aleatoria|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-aleatoria))

;;; f-juego que aborta
;;; ------------------------------------------------------------------------------------------
(defun f-eval-erronea (estado)
  (when estado t)                   ; dummy para evitar warnings de compilador
  (/ 1 0))

(defvar *jdr-erroneo* (make-jugador
                        :nombre   '|Ju-Erroneo|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-erronea))

;;; Jugador Bueno (debido a que juega con un nivel mas de evaluacion)
;;; ------------------------------------------------------------------------------------------
(defun f-eval-Bueno (estado)
  (if (juego-terminado-p estado)
      -50                              ;; Condicion especial de juego terminado
    ;; Devuelve el maximo del numero de fichas del lado enemigo menos el numero de propias
    (max-list (mapcar #'(lambda(x)
                          (- (suma-fila (estado-tablero x) (lado-contrario (estado-lado-sgte-jugador x)))
                             (suma-fila (estado-tablero x) (estado-lado-sgte-jugador x))))
                (generar-sucesores estado)))))

(defvar *jdr-nmx-Bueno* (make-jugador
                        :nombre   '|Ju-Nmx-Bueno|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-Bueno))

;;; Devuelve el top segun test de una lista de nos. y su posicion
(defun max-list (l &key (test #'max))
  (let ((m (reduce test l)))
    (values m (position m l))))

;;; (max-list '(2 5 3 6 2 0))  -> 6, 3
;;; (max-list '(2 5 3 6 2 0) :test #'Min)  -> 0, 5

;;; Jugador Regular
;;; ------------------------------------------------------------------------------------------
(defun f-eval-Regular (estado)
  (- (suma-fila (estado-tablero estado) (estado-lado-sgte-jugador estado))
     (suma-fila (estado-tablero estado) (lado-contrario (estado-lado-sgte-jugador estado)))))

(defvar *jdr-nmx-Regular* (make-jugador
                        :nombre   '|Ju-Nmx-Regular|
                        :f-juego  #'f-j-nmx
                        :f-eval   #'f-eval-Regular))

;;; Mi jugador
;;; ------------------------------------------------------------------------------------------
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

(defun mi-f-ev (estado) 
  (cond
   ((and (juego-terminado-p estado) 
         (> (get-tot (estado-lado-sgte-jugador estado)) 
            (get-tot (lado-contrario (estado-lado-sgte-jugador estado)))))
    95534)
   ((and (juego-terminado-p estado) 
         (< (get-tot (estado-lado-sgte-jugador estado)) 
            (get-tot (lado-contrario (estado-lado-sgte-jugador estado)))))
    (- 95534))
   (T (apply #'+ (mapcar #'* 
                   '(14 15 -71 -60 33 -78 68 29 -20 89 55 -62 95 25)
                   (list
                    (get-fichas (estado-tablero estado) 
                                (estado-lado-sgte-jugador estado) 
                                0)
                    (get-fichas (estado-tablero estado) 
                                (estado-lado-sgte-jugador estado) 
                                1)
                    (get-fichas (estado-tablero estado) 
                                (estado-lado-sgte-jugador estado) 
                                2)
                    (get-fichas (estado-tablero estado) 
                                (estado-lado-sgte-jugador estado) 
                                3)
                    (get-fichas (estado-tablero estado) 
                                (estado-lado-sgte-jugador estado) 
                                4)
                    (get-fichas (estado-tablero estado) 
                                (estado-lado-sgte-jugador estado) 
                                5)
                    (get-fichas (estado-tablero estado) 
                                (lado-contrario (estado-lado-sgte-jugador estado))
                                0)
                    (get-fichas (estado-tablero estado) 
                                (lado-contrario (estado-lado-sgte-jugador estado))
                                1)
                    (get-fichas (estado-tablero estado) 
                                (lado-contrario (estado-lado-sgte-jugador estado))
                                2)
                    (get-fichas (estado-tablero estado) 
                                (lado-contrario (estado-lado-sgte-jugador estado))
                                3)
                    (get-fichas (estado-tablero estado) 
                                (lado-contrario (estado-lado-sgte-jugador estado))       
                                4)
                    (get-fichas (estado-tablero estado) 
                                (lado-contrario (estado-lado-sgte-jugador estado))   
                                5)
                    (cuenta-ceros estado 0)
                    (cuenta-ceros estado 1)))))))
  
(setf *mi-jugador* (make-jugador
                    :nombre 'MancalasGOD
                    :f-juego #'negamax
                    :f-eval #'mi-f-ev))

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
                       '(-269 207 155 -303 -494 -433 -338 46 -381 -211 -333 140 134 320 -198 295)                       (list
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

(setf *mi-jugador2* (make-jugador
                    :nombre 'MancalasSEMIGOD
                    :f-juego #'negamax
                    :f-eval #'heuristica))

;;; Juego automatico sin presentacion del tablero pero con listado de contador
(setq *verjugada* nil)   ; valor por defecto
(setq *vermarcador* nil)   ; valor por defecto
(partida 0 2 (list *mi-jugador2* *jdr-nmx-regular*))
(partida 0 2 (list *mi-jugador2* *jdr-nmx-bueno*))
(partida 0 2 (list *mi-jugador2* *jdr-1st-opt*))
(partida 0 2 (list *mi-jugador2* *jdr-last-opt*))
(partida 1 2 (list *mi-jugador2* *jdr-nmx-regular*))
(partida 1 2 (list *mi-jugador2* *jdr-nmx-bueno*))
(partida 1 2 (list *mi-jugador2* *jdr-1st-opt*))
(partida 1 2 (list *mi-jugador2* *jdr-last-opt*))
;;; ------------------------------------------------------------------------------------------
;;; EJEMPLOS DE PARTIDAS DE PRUEBA
;;; ------------------------------------------------------------------------------------------
;;; Juego manual contra jugador automatico, saca el humano
;(partida 0 2 (list *jdr-humano*      *jdr-nmx-Bueno* ))

;;; Juego manual contra jugador automatico, saca el automatico
;(partida 1 2 (list *jdr-humano*      *jdr-nmx-Bueno* ))

;;; Juego automatico sin presentacion del tablero pero con listado de contador
;(setq *verjugada* nil)   ; valor por defecto
;(setq *vermarcador* t)   ; valor por defecto
;(partida 0 1 (list *jdr-aleatorio*   *jdr-nmx-Bueno*))

;;; Juego automatico con salida mimima por eficiencia, profundidad=2
;;;(setq *verb* nil *debug-level* 0 *verjugada* nil *vermarcador* nil)
;;; Ajustes para facilitar el seguimiento paso a paso (pag. 11). Reduzcase el nivel de
;;; detalle cuando se vaya adquiriendo práctica.
;;; *debug-nmx* activa *verb* tambien para jugadores automaticos (normalmente desactivado).
(setq *debug-level* 2)         ; Ajusta a 2 el nivel de detalle
(setq *verb*        nil)         ; Activa comentarios para seguir la evolucion de la partida
(setq *verjugada*   t)         ; Activa la visualizacion de jugadas
(setq *vermarcador* t)         ; Activa la visualizacion del marcador
(setq *debug-nmx*   t)         ; Desactiva debuging de negamax

(partida 1 2 (list *jdr-nmx-Regular* *jdr-nmx-Regular*))
(partida 1 2 (list *jdr-aleatorio* *jdr-aleatorio*))

;;; Ajustes para facilitar el seguimiento paso a paso (pag. 11). Reduzcase el nivel de
;;; detalle cuando se vaya adquiriendo práctica.
;;; *debug-nmx* activa *verb* tambien para jugadores automaticos (normalmente desactivado).
(setq *debug-level* 2)         ; Ajusta a 2 el nivel de detalle
(setq *verb* t)                ; Activa comentarios para seguir la evolucion de la partida
(setq *verjugada*   t)         ; Activa la visualizacion de jugadas
(setq *vermarcador* t)         ; Activa la visualizacion del marcador
(setq *debug-nmx* nil)         ; Desactiva debuging de negamax
;(partida 0 2 (list *jdr-humano*  *jdr-nmx-Regular*))

;;; Partida entre dos humanos a partir de una posicion determinada para analisis detallado
;;; Se fuerza la posicion de inicio para jugar a partir de ella (ejemplo Pag.5 del enunciado)
;;;(setq mi-posicion (list '(1 0 1 3 2 4 0) (reverse '(12 0 3 5 2 1 2))))
;;;(setq mi-posicion (list '(1 0 1 3 2 4 7) (reverse '(5 0 3 5 2 1 2))))
;;;(setq mi-posicion (list '(0 3 1 1 1 8 8) (reverse '(4 0 8 2 0 0 0))))
;;;(setq estado (crea-estado-inicial 0 mi-posicion))
;;;(partida 0 2 (list *jdr-humano*      *jdr-human2*) mi-posicion)

;;; Fuerza posicion: fin juego inevitable a la siguiente jugada
;(setq mi-posicion (list '(2 1 3 0 2 7 29) (reverse '(2 5 0 0 0 0 0))))

;;; Ejemplo de experimentacion a varias profundidades
;;;(setq *debug-level* 2)
;;;(setq *verjugada*   nil)
;;;(setq *vermarcador* nil)
;;;(dolist (n '(1 2 3 4 5)) (print (partida 2 n (list *jdr-nmx-regular* *jdr-nmx-bueno*))))

;;; Timeout jugada: a nivel 8 el aleatorio pierde por tiempo
;;;(partida 0 1 (list *jdr-humano*      *jdr-nmx-eval-aleatoria*))
;;;(partida 0 8 (list *jdr-humano*      *jdr-nmx-eval-aleatoria*))

;;; Ejemplos de partidas para pruebas
;;;(partida 0 2 (list *jdr-nmx-Regular* *jdr-erroneo*))
;;;(partida 0 2 (list *jdr-nmx-Regular* *jdr-nmx-bueno*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-nmx-Regular*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-nmx-Bueno*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-1st-opt*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-last-opt*))
;;;(partida 0 2 (list *jdr-humano*      *jdr-human2*))

;;; ------------------------------------------------------------------------------------------
;;; TORNEO
;;; ------------------------------------------------------------------------------------------
(defun rnd()
  (let ((randv (random-vector)))
    (progn (setq *players* 
                 (append *players* 
                         (list randv))))))

(defun play(p1 p2 r)
  (setq *verjugada* nil)   ; valor por defecto
  (setq *vermarcador* nil)   ; valor por defecto
  (partida r 2 (list p1 p2)))


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

(defun random-vector()
  (let* ((vec (mapcar #'(lambda (x)
                         (- (random 1000) 500))
               '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
         (jug (crear-jugador vec)))
    (if (and 
         (= (partida 0 2 (list *jdr-nmx-Regular* jug)) 2) 
         (= (partida 1 2 (list *jdr-nmx-Regular* jug)) 2)
         (= (partida 0 2 (list *jdr-nmx-Bueno* jug)) 2)
         (= (partida 1 2 (list *jdr-nmx-Bueno* jug)) 2))
        vec
      (random-vector))))

(defun crear-jugador (vec)
  (make-jugador
   :nombre vec
   :f-juego #'negamax
   :f-eval #'(lambda (estado)
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
                                vec
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
                                 kl
                                 kl-op
                                 (cuenta-ceros estado 0)
                                 (cuenta-ceros estado 1))))))))))

(defun split (l)
  (setq l1 '())
  (setq l2 '())
  (let* ((long (length l))
        (half (/ long 2)))
    (loop for x from 0 to (- long 1) do
          (if (< x half)
              (setq l1 (append l1 (list (nth x l))))
            (setq l2 (append l2 (list (nth x l))))))
    (list l1 l2)))

(defun tourn (lst)
  (if (null (rest lst))
      (first lst)
    (let* ((l (split lst))
           (l1 (first l))
           (l2 (second l))
           (res (mapcar #'(lambda (x y)
                            (if (<= (+ (partida 0 2 (list x y))
                                       (partida 1 2 (list x y)))
                                    3)
                                x
                              y))
                  l1 l2)))
      (tourn res))))

;;; JUGADORES ALEATORIOS
(setq *winners* '())
(setq *players* '())
(length *players*)
(setq *verjugada* nil)   ; valor por defecto
(setq *vermarcador* nil)   ; valor por defecto
(loop for x from 1 to 8192 do (rnd))
(print *players*)
(setq *players* (mapcar #'crear-jugador *players*))
(setq *verjugada* nil)   ; valor por defecto
(setq *vermarcador* nil)   ; valor por defecto
(setq *winners* 
       (append *winners* (list (jugador-nombre (tourn *players*)))))

(print *winners*)


;;; CAMPEONES
(setq *champions* '())

(setq *players* '())
(setq *players* (mapcar #'crear-jugador *winners*))
(setq *champions* 
       (append *champions* (list (jugador-nombre (tourn *players*)))))

(print *champions*)


;;; ------------------------------------------------------------------------------------------
;;; PRUEBA JUGADORES
;;; ------------------------------------------------------------------------------------------
;;; ELBUENO (14 15 -71 -60 33 -78 68 29 -20 89 55 -62 95 25)
;;; ELAMO(-401 387 497 340 -315 -291 -236 -431 -47 414 -169 350 427 362)
;;; ELSUBNOR(429 -283 264 -135 177 -310 209 -202 194 284 274 20 -368 -258)
;;; ELGOD(491 50 201 264 -276 -350 443 194 337 -7 413 -62 -167 32)
;;; THEREALGOD(-364 373 -491 -213 -330 -369 285 31 403 -123 130 92 373 -480 316 179)
;;; SANTISGOD(-352 -3 358 -165 -300 -496 -215 396 303 143 357 375 113 -409 -277 -157)
;;; ADRISGOD(-266 95 -115 -468 -124 -222 -450 -62 434 307 451 -328 -157 -371 320 434)
;;; FINALIST1(-172 194 -159 -265 -342 -321 -240 427 458 187 -310 -471 215 254 -430 141)
;;; FINALIST2(-199 341 -52 -3 -114 337 255 275 -352 -121 -39 -163 120 -242 -172 -154)
;;; JERRYBEATER(-226 382 301 34 255 -374 229 478 -205 148 291 56 355 -215 -214 -478)
;;; ANOTHER1(-235 131 148 -434 -362 -277 316 -82 399 -94 217 -386 158 -72 -276 452)
;;; 2NDPLACE(-259 -285 51 -78 -332 -424 6 -278 -387 -128 -493 118 387 -177 -369 -65)
;;; SOMERANDOMWINNER(106 488 -365 60 -215 -491 -238 -199 54 317 -154 236 298 -416 303 -13)
;;; THEJERRYBEATER(-294 -254 277 -182 -404 -364 -325 5 -213 54 -258 -306 62 -282 -248 -378)
;;; 40961ST(-269 207 155 -303 -494 -433 -338 46 -381 -211 -333 140 134 320 -198 295)
;;; 40962ND(401 405 487 462 -54 -319 -210 224 436 473 302 -317 -442 18 -134 -285)
;;; 81921ST(-288 73 37 -119 -411 -320 -294 280 -389 48 120 303 -102 -154 -453 150)
;;; 81922ND(58 395 -456 -410 -454 -295 -38 -367 -94 484 -88 354 267 344 -147 -247)


(setf vec-champion1 '(-288 73 37 -119 -411 -320 -294 280 -389 48 120 303 -102 -154 -453 150))
(setf campeon1 (crear-jugador vec-champion1))

(setf vec-champion2 '(401 405 487 462 -54 -319 -210 224 436 473 302 -317 -442 18 -134 -285))
(setf campeon2 (crear-jugador vec-champion2))

(setf vec-champion3 '(-172 194 -159 -265 -342 -321 -240 427 458 187 -310 -471 215 254 -430 141))
(setf campeon3 (crear-jugador vec-champion3))

(setf vec-champion4 '(58 395 -456 -410 -454 -295 -38 -367 -94 484 -88 354 267 344 -147 -247))
(setf campeon4 (crear-jugador vec-champion4))

(setf vec-champion5 '(-269 207 155 -303 -494 -433 -338 46 -381 -211 -333 140 134 320 -198 295))
(setf campeon5 (crear-jugador vec-champion5))

(setq *verjugada* nil)   ; valor por defecto
(setq *vermarcador* nil)   ; valor por defecto

;;;TEST CHAMPIONS

(partida 0 2 (list campeon1 *jdr-nmx-regular*))
(partida 1 2 (list campeon1 *jdr-nmx-regular*))
(partida 0 2 (list campeon1 *jdr-nmx-bueno*))
(partida 1 2 (list campeon1 *jdr-nmx-bueno*))
(partida 0 2 (list campeon1 *jdr-1st-opt*))
(partida 1 2 (list campeon1 *jdr-1st-opt*))
(partida 0 2 (list campeon1 *jdr-last-opt*))
(partida 1 2 (list campeon1 *jdr-last-opt*))

(partida 0 2 (list campeon2 *jdr-nmx-regular*))
(partida 1 2 (list campeon2 *jdr-nmx-regular*))
(partida 0 2 (list campeon2 *jdr-nmx-bueno*))
(partida 1 2 (list campeon2 *jdr-nmx-bueno*))
(partida 0 2 (list campeon2 *jdr-1st-opt*))
(partida 1 2 (list campeon2 *jdr-1st-opt*))
(partida 0 2 (list campeon2 *jdr-last-opt*))
(partida 1 2 (list campeon2 *jdr-last-opt*))

(partida 0 2 (list campeon3 *jdr-nmx-regular*))
(partida 1 2 (list campeon3 *jdr-nmx-regular*))
(partida 0 2 (list campeon3 *jdr-nmx-bueno*))
(partida 1 2 (list campeon3 *jdr-nmx-bueno*))
(partida 0 2 (list campeon3 *jdr-1st-opt*))
(partida 1 2 (list campeon3 *jdr-1st-opt*))
(partida 0 2 (list campeon3 *jdr-last-opt*))
(partida 1 2 (list campeon3 *jdr-last-opt*))

(partida 0 2 (list campeon4 *jdr-nmx-regular*))
(partida 1 2 (list campeon4 *jdr-nmx-regular*))
(partida 0 2 (list campeon4 *jdr-nmx-bueno*))
(partida 1 2 (list campeon4 *jdr-nmx-bueno*))
(partida 0 2 (list campeon4 *jdr-1st-opt*))
(partida 1 2 (list campeon4 *jdr-1st-opt*))
(partida 0 2 (list campeon4 *jdr-last-opt*))
(partida 1 2 (list campeon4 *jdr-last-opt*))

(partida 0 2 (list campeon5 *jdr-nmx-regular*))
(partida 1 2 (list campeon5 *jdr-nmx-regular*))
(partida 0 2 (list campeon5 *jdr-nmx-bueno*))
(partida 1 2 (list campeon5 *jdr-nmx-bueno*))
(partida 0 2 (list campeon5 *jdr-1st-opt*))
(partida 1 2 (list campeon5 *jdr-1st-opt*))
(partida 0 2 (list campeon5 *jdr-last-opt*))
(partida 1 2 (list campeon5 *jdr-last-opt*))

(partida 0 2 (list campeon1 *mi-jugador*))
(partida 1 2 (list campeon1 *mi-jugador*))
(partida 0 2 (list campeon1 *mi-jugador2*))
(partida 1 2 (list campeon1 *mi-jugador2*))
(partida 0 2 (list campeon2 *mi-jugador*))
(partida 1 2 (list campeon2 *mi-jugador*))
(partida 0 2 (list campeon2 *mi-jugador2*))
(partida 1 2 (list campeon2 *mi-jugador2*))
(partida 0 2 (list campeon3 *mi-jugador*))
(partida 1 2 (list campeon3 *mi-jugador*))
(partida 0 2 (list campeon3 *mi-jugador2*))
(partida 1 2 (list campeon3 *mi-jugador2*))
(partida 0 2 (list campeon4 *mi-jugador*))
(partida 1 2 (list campeon4 *mi-jugador*))
(partida 0 2 (list campeon4 *mi-jugador2*))
(partida 1 2 (list campeon4 *mi-jugador2*))
(partida 0 2 (list campeon5 *mi-jugador*))
(partida 1 2 (list campeon5 *mi-jugador*))
(partida 0 2 (list campeon5 *mi-jugador2*))
(partida 1 2 (list campeon5 *mi-jugador2*))

(partida 0 2 (list campeon1 campeon2))
(partida 1 2 (list campeon1 campeon2))
(partida 0 2 (list campeon1 campeon3))
(partida 1 2 (list campeon1 campeon3))
(partida 0 2 (list campeon1 campeon4))
(partida 1 2 (list campeon1 campeon4))
(partida 0 2 (list campeon1 campeon5))
(partida 1 2 (list campeon1 campeon5))
(partida 0 2 (list campeon3 campeon2))
(partida 1 2 (list campeon3 campeon2))
(partida 0 2 (list campeon4 campeon2))
(partida 1 2 (list campeon4 campeon2))
(partida 0 2 (list campeon5 campeon2))
(partida 1 2 (list campeon5 campeon2))
(partida 0 2 (list campeon3 campeon4))
(partida 1 2 (list campeon3 campeon4))
(partida 0 2 (list campeon5 campeon4))
(partida 1 2 (list campeon5 campeon4))
