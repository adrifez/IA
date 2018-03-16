;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Lab assignment 2: Search
;;    LAB GROUP: 2302
;;    Couple: 07
;;    Author 1: Adrián Fernández Amador
;;    Author 2: Santiago González-Carvajal Centenera
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
  states              ; List of states
  initial-state       ; Initial state
  f-goal-test         ; reference to a function that determines whether 
                      ; a state fulfills the goal 
  f-h                 ; reference to a function that evaluates to the 
                      ; value of the heuristic of a state
  operators)          ; list of operators (references to functions) to generate succesors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in search tree
;;
(defstruct node 
  state           ; state label
  parent          ; parent node
  action          ; action that generated the current node from its parent
  (depth 0)       ; depth in the search tree
  (g 0)           ; cost of the path from the initial state to this node
  (h 0)           ; value of the heurstic
  (f 0))          ; g + h 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
  name              ; Name of the operator that generated the action
  origin            ; State on which the action is applied
  final             ; State that results from the application of the action
  cost)             ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
  name              ; name of the search strategy
  node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN: Define galaxy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *planets* '(Avalon Davion Katril Kentares Mallory Proserpina Sirtis))

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
 
(defparameter *sensors*
  '((Avalon 15) (Davion 5) (Katril 9) (Kentares 14)
    (Mallory 12) (Proserpina 7) (Sirtis 0)))

(defparameter *planet-origin* 'Mallory)
(defparameter *planets-destination* '(Sirtis))
(defparameter *planets-forbidden*   '(Avalon))
(defparameter *planets-mandatory*   '(Katril Proserpina))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; BEGIN: Exercise 1 -- Evaluation of the heuristic
;;
;; Returns the value of the heuristics for a given state
;;
;;  Input:
;;    state: the current state (vis. the planet we are on)
;;    sensors: a sensor list, that is a list of pairs
;;                (state cost)
;;             where the first element is the name of a state and the second
;;             a number estimating the cost to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;
(defun f-h-galaxy (state sensors)
  (unless (null sensors)
    (let* ((sensor (first sensors))
           (planet (first sensor))
           (cost (second sensor)))
      (if (equal state planet)                     ; Si encuentra el planeta
          cost                                     ; devuelve el coste
        (f-h-galaxy state (rest sensors))))))      ; Si no continua evaluando
    

(f-h-galaxy 'Sirtis *sensors*) ;-> 0
(f-h-galaxy 'Avalon *sensors*) ;-> 15
(f-h-galaxy 'Earth *sensors*) ;-> NIL


;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;
(defun possible_links (state links forbidden)
  (unless (null links)
    (let ((link (first links))
          (found-links (possible_links state (rest links) forbidden)))
      (if (and (eql state (first link))                                 ; Si el origen es el estado actual
               (not (member (second link) forbidden :test #'eql)))      ; y el destino no esta prohibido
          (cons (first links)                                           ; Incluimos el link como permitido
                found-links)
        found-links))))                                                 ; Si no, seguimos evaluando

(possible_links 'Mallory *white-holes* '())


(defun navigate (state holes forbidden action-name)
  (let ((actions (possible_links state holes forbidden)))
    (mapcar #'(lambda (act) (make-action :name action-name        ; Para cada link posible, creamos una accion
                                         :origin (first act)
                                         :final (second act)
                                         :cost (third act)))
      actions)))


(defun navigate-white-hole (state white-holes)
  (navigate state white-holes '() 'navigate-white-hole))


(defun navigate-worm-hole (state worm-holes planets-forbidden)
  (navigate state worm-holes planets-forbidden 'navigate-worm-hole))


(navigate-worm-hole 'Mallory *worm-holes* *planets-forbidden*)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))

(navigate-worm-hole 'Mallory *worm-holes* NIL)  ;-> 
;;;(#S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL AVALON :COST 9)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL KATRIL :COST 5)
;;; #S(ACTION :NAME NAVIGATE-WORM-HOLE :ORIGIN MALLORY :FINAL PROSERPINA :COST 11))


(navigate-white-hole 'Kentares *white-holes*) ;->
;;;(#S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL AVALON :COST 3)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL KATRIL :COST 10)
;;; #S(ACTION :NAME NAVIGATE-WHITE-HOLE :ORIGIN KENTARES :FINAL PROSERPINA :COST 7))


(navigate-white-hole 'Uranus *white-holes*)  ;-> NIL
(navigate-worm-hole 'Uranus *worm-holes* *planets-forbidden*)  ;-> NIL


;;
;; END: Exercise 2 -- Navigation operators
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3 -- Goal test
;;
(defun check-mandatory (node mandatory)
  (unless (null node)                                    ; Si node es NIL no se han alcanzado mandatory
    (let* ((parent (node-parent node))                   ; Padre del nodo
           (state (find (node-state node)                ; Etiqueta del nodo si es mandatory,
                        mandatory                        ; NIL en caso contrario
                        :test #'eql))
           (new-mandatory (remove state mandatory)))     ; Mandatory sin state
      (if (null mandatory)                                           ; Caso base
          T                                                          ; Caso recurrente
        (or (check-mandatory (node-parent node) new-mandatory)
            (null new-mandatory))))))


(defun f-goal-test-galaxy (node planets-destination planets-mandatory) 
  (and (check-mandatory node planets-mandatory)         ; Debe haber alcanzado los mandatorios
       (not (null (member (node-state node)             ; Y ser destino
                          planets-destination
                          :test #'eql)))))


(defparameter node-01
  (make-node :state 'Avalon))
(defparameter node-02
  (make-node :state 'Kentares :parent node-01))
(defparameter node-03
  (make-node :state 'Katril :parent node-02))
(defparameter node-04
  (make-node :state 'Kentares :parent node-03))

(check-mandatory node-01 '(Avalon Katril)); -> NIL
(check-mandatory node-02 '(Avalon Katril)); -> NIL
(check-mandatory node-03 '(Avalon Katril)); -> T
(check-mandatory node-04 '(Avalon Katril)); -> T

(f-goal-test-galaxy node-01 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-02 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-03 '(kentares urano) '(Avalon Katril)); -> NIL
(f-goal-test-galaxy node-04 '(kentares urano) '(Avalon Katril)); -> T


;;
;; END: Exercise 3 -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 4 -- Define the galaxy structure
;;
;;
(defparameter *galaxy-M35* 
  (make-problem 
   :states            *planets*          
   :initial-state     *planet-origin*
   :f-goal-test       #'(lambda (node) 
                          (f-goal-test-galaxy node *planets-destination*
                                                   *planets-mandatory*))
   :f-h               #'(lambda (node)
                          (f-h-galaxy node *sensors*))
   :operators         (list #'(lambda (node) (navigate-white-hole
                                              node
                                              *white-holes*))
                            #'(lambda (node) (navigate-worm-hole
                                              node
                                              *worm-holes*
                                              *planets-forbidden*))))) 

;;
;;  END: Exercise 4 -- Define the galaxy structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 5: Expand node
;;
(defun expand-node (node problem)
  (mapcan #'(lambda (oper)                             ; Para cada operador del problema
              (mapcar #'(lambda (act)                  ; Para cada accion del del nodo con el operador
                          (let ((gfun (+ (node-g node)                  ; g es la suma del g del nodo
                                         (action-cost act)))            ; y el coste de la accion
                                (hfun (funcall (problem-f-h problem)    ; h es la funcion f del problema
                                               (action-final act))))    ; evaluada en el final de la accion
                            (make-node :state (action-final act)        ; state es el final de la accion
                                       :parent node                     ; parent es el nodo original
                                       :action act                      ; action es la accion evaluada
                                       :depth (+ (node-depth node) 1)   ; profundidad del nodo mas 1
                                       :g gfun
                                       :h hfun
                                       :f (+ gfun hfun))))              ; f es la suma de g h
                (funcall oper (node-state node))))
    (problem-operators problem)))


(expand-node (make-node :state 'Kentares :depth 0 :g 0 :f 0) *galaxy-M35*)
;;;(#S(NODE :STATE AVALON
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL AVALON
;;;                           :COST 3)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE KATRIL
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL KATRIL
;;;                           :COST 10)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WHITE-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL PROSERPINA
;;;                           :COST 7)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT #S(NODE :STATE KENTARES
;;;                         :PARENT NIL
;;;                         :ACTION NIL
;;;                         :DEPTH 0
;;;                         :G ...)
;;;         :ACTION #S(ACTION :NAME NAVIGATE-WORM-HOLE
;;;                           :ORIGIN KENTARES
;;;                           :FINAL PROSERPINA
;;;                           :COST 12)
;;;         :DEPTH 1
;;;         :G ...))



;;
;; END Exercise 5: Expand node
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 6 -- Node list management
;;;  
(defun insert-node-strategy (node lst-nodes strategy)
  (if (null lst-nodes)                                        ; Si la lista de nodos es vacia devolvemos el nodo
      (list node)
    (let* ((sorted-node (first lst-nodes))                    ; Primer nodo de la listad de ordenados
           (res (funcall (strategy-node-compare-p strategy)   ; Resultado de aplicar la estrategia
                         node                                 ; al nodo evaluado
                         sorted-node)))                       ; con el primer nodo de la lista ordenada
      (cond (res
             (cons node                                       ; Caso base
                   lst-nodes))
            (T
             (cons sorted-node                                ; Caso recursivo
                   (insert-node-strategy node
                                         (rest lst-nodes)
                                         strategy)))))))


(defun insert-nodes-strategy (nodes lst-nodes strategy)
  (unless (null nodes)                                        ; Si la lista de nodos es vacia devolvemos NIL
    (let* ((node (first nodes))                               ; Primer nodo a evaluar
           (new-lst-nodes (insert-node-strategy node          ; Nueva lista con el nodo insertado correctamente
                                                lst-nodes
                                                strategy)))
      (if (null (second nodes))                               ; Caso base  
          new-lst-nodes
        (insert-nodes-strategy (rest nodes)                   ; Caso recursivo
                               new-lst-nodes
                               strategy)))))


(insert-nodes-strategy '(4 8 6 2)
                       '(1 3 5 7) 
                       (make-strategy :name 'simple
                                      :node-compare-p #'<))


(defparameter *uniform-cost*
  (make-strategy :name 'uniform-cost
                 :node-compare-p #'(lambda (n1 n2)
                                     (< (node-g n1) (node-g n2)))))


(insert-nodes-strategy (list (make-node :state 'Proserpina :depth 0 :g 5 :f 0)) 
                       (expand-node (make-node :state 'Kentares :depth 0 :g 0 :f 0)
                                    *galaxy-M35*) 
                       *uniform-cost*)
;;; (#S(NODE :STATE AVALON
;;;         :PARENT #S(NODE
;;;                    :STATE
;;;                    KENTARES
;;;                    :PARENT
;;;                    NIL
;;;                    :ACTION
;;;                    NIL
;;;                    :DEPTH
;;;                    0
;;;                    :G
;;;                    ...)
;;;         :ACTION #S(ACTION
;;;                    :NAME
;;;                    NAVIGATE-WHITE-HOLE
;;;                    :ORIGIN
;;;                    KENTARES
;;;                    :FINAL
;;;                    AVALON
;;;                    :COST
;;;                    3)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT NIL
;;;         :ACTION NIL
;;;         :DEPTH 0
;;;         :G ...)
;;; #S(NODE :STATE KATRIL
;;;         :PARENT #S(NODE
;;;                    :STATE
;;;                    KENTARES
;;;                    :PARENT
;;;                    NIL
;;;                    :ACTION
;;;                    NIL
;;;                    :DEPTH
;;;                    0
;;;                    :G
;;;                    ...)
;;;         :ACTION #S(ACTION
;;;                    :NAME
;;;                    NAVIGATE-WHITE-HOLE
;;;                    :ORIGIN
;;;                    KENTARES
;;;                    :FINAL
;;;                    KATRIL
;;;                    :COST
;;;                    10)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT #S(NODE
;;;                    :STATE
;;;                    KENTARES
;;;                    :PARENT
;;;                    NIL
;;;                    :ACTION
;;;                    NIL
;;;                    :DEPTH
;;;                    0
;;;                    :G
;;;                    ...)
;;;         :ACTION #S(ACTION
;;;                    :NAME
;;;                    NAVIGATE-WHITE-HOLE
;;;                    :ORIGIN
;;;                    KENTARES
;;;                    :FINAL
;;;                    PROSERPINA
;;;                    :COST
;;;                    7)
;;;         :DEPTH 1
;;;         :G ...)
;;; #S(NODE :STATE PROSERPINA
;;;         :PARENT #S(NODE
;;;                    :STATE
;;;                    KENTARES
;;;                    :PARENT
;;;                    NIL
;;;                    :ACTION
;;;                    NIL
;;;                    :DEPTH
;;;                    0
;;;                    :G
;;;                    ...)
;;;         :ACTION #S(ACTION
;;;                 :NAME
;;;                 NAVIGATE-WORM-HOLE
;;;                 :ORIGIN
;;;                 KENTARES
;;;                 :FINAL
;;;                 PROSERPINA
;;;                 :COST
;;;                 12)
;;;         :DEPTH 1
;;;         :G ...))
 


;;
;;    END: Exercize 6 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 7 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell 
;; us which nodes should be analyzed first. In the A* strategy, the first 
;; node to be analyzed is the one with the smallest value of g+h
;;
(defun node-f-p (node-1 node-2)
  (< (node-f node-1)
     (node-f node-2)))

(defparameter *A-star*
  (make-strategy 
   :name 'A-star
   :node-compare-p #'node-f-p))

;;
;; END: Exercise 7 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 8: Search algorithm
;;;
(defun graph-search-rec (open-nodes problem strategy)
  (unless (null open-nodes)
    (let* ((node (first open-nodes))
           (new-lst (rest open-nodes))
           (test (problem-f-goal-test node)))
      (cond (test
             node)
            (T
             (graph-search-rec (insert-nodes-strategy (expand-node node problem)
                                                      new-lst
                                                      strategy)
                               problem
                               strategy))))))


(defun graph-search (problem strategy)
  (let* ((name (problem-initial-state problem))
         (hfun (funcall (problem-f-h problem) name))
         (node-ini (make-node :state name
                              :parent NIL
                              :action NIL
                              :h hfun
                              :f hfun))
         (open-nodes (list node-ini))
         )
    (graph-search-rec open-nodes problem strategy)))


;
;  Solve a problem using the A* strategy
;
(defun a-star-search (problem)
  (graph-search problem 
                *A-star*))


(graph-search *galaxy-M35* *A-star*);->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


(print (a-star-search *galaxy-M35*));->
;;;#S(NODE :STATE ...
;;;        :PARENT #S(NODE :STATE ...
;;;                        :PARENT #S(NODE :STATE ...)) 


;;; 
;;;    END Exercise 8: Search algorithm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 9: Solution path / action sequence
;;;
(defun solution-path (node)
  ...)

(solution-path nil) ;;; -> NIL 
(solution-path (a-star-search *galaxy-M35*))  ;;;-> (MALLORY ...)

(defun action-sequence-aux (node)
  ...)

(action-sequence (a-star-search *galaxy-M35*))
;;; ->
;;;(#S(ACTION :NAME ...)) 

;;; 
;;;    END Exercise 9: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 10: depth-first / breadth-first
;;;

(defparameter *depth-first*
  (make-strategy
   :name 'depth-first
   :node-compare-p #'depth-first-node-compare-p))

(defun depth-first-node-compare-p (node-1 node-2)
  ...)

(solution-path (graph-search *galaxy-M35* *depth-first*))
;;; -> (MALLORY ... )

(defparameter *breadth-first*
  (make-strategy
   :name 'breadth-first
   :node-compare-p #'breadth-first-node-compare-p))

(defun breadth-first-node-compare-p (node-1 node-2)
  ...)

(solution-path (graph-search *galaxy-M35* *breadth-first*))
;; -> (MALLORY ... )

;;; 
;;;    END Exercise 10: depth-first / breadth-first
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
