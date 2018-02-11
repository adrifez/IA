
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-lol-rec (lol)
  (if (and (first lol) (not (first (rest lol))))
      (return-from combine-lol-rec (mapcar #'(lambda (x) (list x)) (first lol))))
  (return-from combine-lol-rec
    (mapcan #'(lambda (x) (mapcar #'(lambda (y) (append (list x) y))
                            (combine-lol-rec (rest lol))))
      (first lol))))


(defun combine-list-of-lsts (lstolsts)
  (cond ((null lstolsts) (return-from combine-list-of-lsts NIL))
        ((some #'null lstolsts) (return-from combine-list-of-lsts NIL))
        (t (return-from combine-list-of-lsts (combine-lol-rec lstolsts)))))


(combine-list-of-lsts '(() (+ -) (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) () (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) (1 2 3 4) ())) ;; --> NIL
(combine-list-of-lsts '((1 2 3 4))) ;; --> ((1) (2) (3) (4))
(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;; --> ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;; (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;; (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))