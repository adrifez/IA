;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;3.1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-elt-lst (elt lst)
  (if(or (null elt) (null lst))
      (return-from combine-elt-lst nil))
  (mapcar #'(lambda (x) (list elt x)) lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;3.2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-lst-lst (lst1 lst2)
  (if(or (null lst1) (null lst2))
      (return-from combine-lst-lst nil))
  (mapcan #'(lambda (x) (combine-elt-lst x lst2)) lst1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-lst-lsts (lst1 lsts)
  (if (null (second lsts))
      (return-from combine-lst-lsts
        (mapcar #'(lambda (x) (append lst1 (list x))) (first lsts))))
  (combine-lst-lsts (append lst1 (list (caar lsts))) (rest lsts)))

(combine-lst-lsts '(1) '((1 2 3 4) (+ -) (a b c d)))
 
(defun combine-list-of-lsts (lstolsts)
  (if (eql (first lstolsts) nil) 
      (return-from combine-list-of-lsts nil))
  (mapcan #'(lambda (x) (combine-lst-lsts (list x) (rest lstolsts))) (first lstolsts)))

(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))