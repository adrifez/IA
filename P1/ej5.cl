;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net) ;queue es una lista de listas con la raiz como elemento de la lista contenida
  (if (null queue) '() ;si la cola se queda vacia, la meta no es accesible desde el nodo origen
    (let* ((path (first queue)) ;en vez de guardar solo un nodo en la cola, guarda una lista con el nodo a explorar en la primera posicion y el camino para llegar a el en las siguientes
           (node (first path))) ;node es el nodo que sale de la cola y cuyos vecinos se van explorar
      (if (eql node end) ;si el nodo es la meta devolvemos el camino hasta la misma
          (reverse path) ;teniendo en cuenta que el camino que teniamos estaba al reves ya que el primer nodo era el que nos tocaba explorar y el ultimo el de partida
        (bfs end
             (append (rest queue)
                     (new-paths path node net)) ;encolamos todos los adyacentes al nodo en el que nos encontramos
             net)))))

(defun new-paths (path node net) ;devuelve una lista con todos los caminos a explorar desde un nodo
  (mapcar #'(lambda(n)           ;es decir, todos los caminos a los que dan lugar sus nodos adyacentes
              (cons n path))
    (rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bfs 'd '((c)) '((a d) (b d f) (c e) (d f) (e b f) (f)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

;;Lo hace porque BFS es un algoritmo que nos asegura encontrar el camino más corto entre dos nodos
;;(al menos si suponemos que todos los enlaces tienen el mismo coste), ya que los nodos se van explorando
;;según cuan "cerca" se encuentran del nodo de partida (es decir, el algoritmo funciona encolando
;;los nodos adyacentes a los nodos extraídos de la cola)

(shortest-path 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))
;;(bfs 'f '((a)) '((a d) (b d f) (c e) (d f) (e b f) (f)))
;;   (new-paths '(a) 'a '((a d) (b d f) (c e) (d f) (e b f) (f))) -> ((d a))
;;(bfs 'f '((d a)) '((a d) (b d f) (c e) (d f) (e b f) (f)))
;;   (new-paths '(d a) 'd '((a d) (b d f) (c e) (d f) (e b f) (f))) -> ((f d a))
;;(bfs 'f '((f d a)) '((a d) (b d f) (c e) (d f) (e b f) (f)))
;;   (eql node end) -> T, devuelve (reverse path)=(a d f)

(shortest-path 'f 'c 
               '((a b c d e) (b a d e f) (c a g) (d a b g h) (e a b g h) (f b h) (g c d e h) (h d e f g)))
;;Output: (F B A C)

;;Falla porque no etiqueta los nodos como visitados, por ejemplo
;;en ((a d) (b d f) (c e) (d f) (e b f) (f a)), la llamada
;;(shortest-path 'a 'b '((a d) (b d f) (c e) (d f) (e b f) (f a)))
;;Error: Stack overflow (signal 1000)
;;[condition type: SYNCHRONOUS-OPERATING-SYSTEM-SIGNAL]

(defun bfs-improved (end queue net)
  (if (null queue) '()
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
        (if (member node (rest path)) ;De esta manera evitamos los bucles
            '()
          (bfs-improved end
                        (append (rest queue)
                                (new-paths path node net))
                        net))))))

(defun shortest-path-improved (start end net)
  (bfs-improved end (list (list start)) net))

(shortest-path-improved 'a 'f '((a d) (b d f) (c e) (d f) (e b f) (f)))
(shortest-path-improved 'a 'b '((a d) (b d f) (c e) (d f) (e b f) (f a)))