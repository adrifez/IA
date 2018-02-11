
(defun combine-elt-lst (elt lst)
  (if(or (null elt) (null lst))
      (return-from combine-elt-lst nil))
  (mapcar #'(lambda (x) (cons elt x)) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-list-of-lsts (lstolsts)
  (cond ((null lstolsts)
         (return-from combine-list-of-lsts (list NIL)))  
        (t (return-from combine-list-of-lsts
             (mapcan #'(lambda (x) (combine-elt-lst x (combine-list-of-lsts (rest lstolsts))))
               (first lstolsts))))))




(list NIL)
(equal '(NIL) (rest '((a b c) ())))


(combine-elt-lst '1 '(NIL))
(null '(()))
(rest '((a b c) ()))
(first '(()))

(combine-list-of-lsts '(())) ;; --> (NIL)
(combine-list-of-lsts '(() (+ -) (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) () (1 2 3 4))) ;; --> NIL
(combine-list-of-lsts '((a b c) (1 2 3 4) ())) ;; --> NIL

(combine-list-of-lsts '((1 2 3 4))) ;; --> ((1) (2) (3) (4))

(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;; --> ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;; (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;; (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))