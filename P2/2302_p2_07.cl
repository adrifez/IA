;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; DEFINICIONES
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Planets
;;
(defparameter *planets* '(Avalon Davion Katril Kentares Mallory Proserpina Sirtis))
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Holes
;;
(defparameter *white-holes*
  '((Avalon Mallory 6.4) (Avalon Proserpina 8.6)
    (Davion Proserpina 5) (Davion Sirtis 6)
    (Katril Davion 9) (Katril Mallory 10)
    (Kentares Avalon 3) (Kentares Katril 10) (Kentares Proserpina 7)
    (Mallory Katril 10) (Mallory Proserpina 15)
    (Proserpina Avalon 8.6) (Proserpina Davion 5) (Proserpina Mallory 15) (Proserpina Sirtis 12)
    (Sirtis Davion 6) (Sirtis Proserpina 12)))

(defparameter *worm-holes*
  '((Avalon Kentares 4) (Avalon Mallory 9)
    (Davion Katril 5) (Davion Sirtis 8)
    (Katril Davion 5) (Katril Mallory 5) (Katril Sirtis 10)
    (Kentares Avalon 4) (Kentares Proserpina 12)
    (Mallory Avalon 9) (Mallory Katril 5) (Mallory Proserpina 11)
    (Proserpina Kentares 12) (Proserpina Mallory 11) (Proserpina Sirtis 9)
    (Sirtis Davion 8) (Sirtis Katril 10) (Sirtis Proserpina 9)))
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sensors
;;
(defparameter *sensors*
  '((Avalon 15) (Davion 5) (Katril 9) (Kentares 14)
    (Mallory 12) (Proserpina 7) (Sirtis 0)))
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Origin planet
;;
(defparameter *planet-origin* 'Mallory)
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Destination planets
;;
(defparameter *planets-destination* '(Sirtis))
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Forbidden planets
;;
(defparameter *planets-forbidden* '(Avalon))
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mandatory planets
;;
(defparameter *planets-mandatory* '(Katril Proserpina))
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Problem definition
;;
(defstruct problem
  states            ; List of states
  initial-state     ; Initial state
  f-goal-test       ; reference to a function that determines whether
                    ; a state fulfills the goal
  f-h               ; reference to a function that evaluates to the
                    ; value of the heuristic of a state
  operators)        ; list of operators (references to functions)
                    ; to generate actions, which, in their turn, are
                    ; used to generate succesors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Node in the search algorithm
;;
(defstruct node
  state        ; state label
  parent       ; parent node
  action       ; action that generated the current node from its parent
  (depth 0)    ; depth in the search tree
  (g 0)        ; cost of the path from the initial state to this node
  (h 0)        ; value of the heuristic
  (f 0))       ; g + h
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Actions
;;
(defstruct action
  name         ; Name of the operator that generated the action
  origin       ; State on which the action is applied
  final        ; State that results from the application of the action
  cost)        ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Search strategies
;;
(defstruct strategy
  name               ; Name of the search strategy
  node-compare-p)    ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EJERCICIOS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 1. Codifica una función que calcula el valor de la heurística
;;    en el estado actual.
;;
(defun f-h-galaxy (state sensors)
  (unless (null sensors)
    (let* ((sensor (first sensors))
           (planet (first sensor))
           (cost (second sensor)))
      (if (equal state planet)
          cost
        (f-h-galaxy state (rest sensors))))))
    

(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth *sensors*) ;-> NIL

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; 2. Codifica los operadores de agujeros blancos y de gusano, que
;;    indican a qué planetas se pueden llegar con cada tipo de agujero.
;;
(defun navigate-white-hole (state white-holes)
  )


(defun navigate-worm-hole (state worm-holes planets-forbidden)
  )


(navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))
(navigate-worm-hole 'Mallory *worm-holes* NIL) ;->
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL AVALON :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))
(navigate-white-hole 'Kentares *white-holes*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL PROSERPINA :COST 7))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
