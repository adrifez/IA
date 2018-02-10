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
  (mapcan #'(lambda (x) (combine-elt-lst x lst2)) lst1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;3.3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-lst-lsts (lst1 lsts)
  (cond
   ((null lst1) nil)
   ((null lsts) lst1)
   ((and (rest lsts) (null (second lsts))) nil)
   ((not (null (second lsts)))
    (combine-lst-lsts
     (mapcan
       #'(lambda (x) 
           (mapcar #'(lambda (y) (if (listp x)
                                     (append x (list y))
                                   (append (list x) (list y)))) 
           (first lsts))) 
       lst1) 
     (rest lsts)))
   (T (mapcan #'(lambda (x) (mapcar #'(lambda (y) (append x (list y))) (first lsts))) lst1))))
 
(defun combine-list-of-lsts (lstolsts)
  (if (null lstolsts)
      (return-from combine-list-of-lsts (list nil)))
  (combine-lst-lsts (mapcar #'(lambda (x) (list x)) (first lstolsts)) (rest lstolsts)))

(combine-list-of-lsts '((a b c)))