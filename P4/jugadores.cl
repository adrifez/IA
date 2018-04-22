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
  
(setf *mi-jugador2* (make-jugador
                    :nombre 'MancalasSEMIGOD
                    :f-juego #'negamax
                    :f-eval #'heuristica))

;;; Juego automatico sin presentacion del tablero pero con listado de contador
(setq *verjugada* nil)   ; valor por defecto
(setq *vermarcador* nil)   ; valor por defecto
(partida 0 2 (list *mi-jugador* *jdr-nmx-regular*))
(partida 0 2 (list *mi-jugador* *jdr-nmx-bueno*))
(partida 0 2 (list *mi-jugador* *jdr-1st-opt*))
(partida 0 2 (list *mi-jugador* *jdr-last-opt*))
(partida 1 2 (list *mi-jugador* *jdr-nmx-regular*))
(partida 1 2 (list *mi-jugador* *jdr-nmx-bueno*))
(partida 1 2 (list *mi-jugador* *jdr-1st-opt*))
(partida 1 2 (list *mi-jugador* *jdr-last-opt*))
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

(defun play(p1 p2 r)
  (setq *verjugada* nil)   ; valor por defecto
  (setq *vermarcador* nil)   ; valor por defecto
  (partida r 1 (list p1 p2)))


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
  (mapcar #'(lambda (x)
              (- (random 1000) 500))
    '(1 2 3 4 5 6 7 8 9 10 11 12 13 14)))


(defun crear-jugador (vec)
  (make-jugador
   :nombre vec
   :f-juego #'negamax
   :f-eval #'(lambda (estado)
               (if (juego-terminado-p estado)
                   -999999999
                 (apply #'+ (mapcar #'* 
                            vec
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
                             (cuenta-ceros estado 1))))))))


(defun tourn (lst)
  (if (null (rest lst))
      (first lst)
    (let* ((l (split lst))
           (l1 (first l))
           (l2 (second l))
           (res (mapcar #'(lambda (x y)
                            (if (<= (+ (partida 0 1 (list x y))
                                       (partida 1 1 (list x y))
                                       (partida 0 2 (list x y))
                                       (partida 1 2 (list x y)))
                                    6)
                                x
                              y))
                  l1 l2)))
      (tourn res))))


(defun split (l)
  (if (null (rest (rest l)))
      (list (list (first l))
            (list (second l)))
    (let ((l1 (first (split (rest (rest l)))))
          (l2 (second (split (rest (rest l))))))
      (list (cons (first l) l1)
            (cons (second l) l2)))))


;;; JUGADORES ALEATORIOS
(setq *winners* '())

(setq *players* '())
(loop for x from 1 to 32 do (rnd))
(setq *players* (mapcar #'crear-jugador *players*))
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

;;; (19 38 -17 48 -73 -4 -94 -61 72 59 6 -12 41 98)
;;; ELBUENO (14 15 -71 -60 33 -78 68 29 -20 89 55 -62 95 25)
;;; CHAMPION1 (-26 129 -281 -215 36 -480 158 -190 -452 -252 -181 108 -254 39)
;;; CHAMPION2 (475 -166 457 287 -303 418 241 -125 67 240 -55 492 -368 -243)
;;; CHAMPION3 (215 -43 -183 173 -179 -201 -24 377 -270 -215 121 76 -115 3)
;;; CHAMPION4 (331 -93 -479 47 -200 -492 126 -43 421 425 342 196 84 -262)


(setf vec-champion1 '(-26 129 -281 -215 36 -480 158 -190 -452 -252 -181 108 -254 39))
(setf campeon1 (crear-jugador vec-champion1))

(setf vec-champion2 '(475 -166 457 287 -303 418 241 -125 67 240 -55 492 -368 -243))
(setf campeon2 (crear-jugador vec-champion2))

(setf vec-champion3 '(215 -43 -183 173 -179 -201 -24 377 -270 -215 121 76 -115 3))
(setf campeon3 (crear-jugador vec-champion3))

(setf vec-champion4 '(331 -93 -479 47 -200 -492 126 -43 421 425 342 196 84 -262))
(setf campeon4 (crear-jugador vec-champion4))


(partida 0 1 (list campeon4 *jdr-nmx-bueno*))
(partida 1 1 (list campeon4 *jdr-nmx-bueno*))
(partida 0 2 (list campeon4 *jdr-nmx-bueno*))
(partida 1 2 (list campeon4 *jdr-nmx-bueno*))

(partida 0 1 (list campeon3 campeon4))
(partida 1 1 (list campeon3 campeon4))
(partida 0 2 (list campeon3 campeon4))
(partida 1 2 (list campeon3 campeon4))
>>>>>>> 209447221f0e3ebefc6750dd419896cdb05b8ae2
