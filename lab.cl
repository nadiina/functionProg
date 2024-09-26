#|
ttt:
- Author: nadii
- Date: 2024-09-22
|#

(defvar custom-list nil)
(setq custom-list
    (list 1 'a (list 'b 'c) ()))

(format t "~%1. Printing custom-list:")
(print custom-list)

(format t "~%2. Printing the first element (head) of custom-list:")
(print (car custom-list))

(format t "~%3. Printing the tail of custom-list (everything except the head):")
(print (cdr custom-list))

(format t "~%4. Printing the third element of custom-list:")
(print (nth 2 custom-list))

(format t "~%5. Printing the last element of custom-list:")
(print (first (last custom-list)))

(format t "~%6.1 (atom) Checking if the first, the 3th, the last elements of custom-list are an atom:")
(print (atom (car custom-list)))
(print (atom (nth 2 custom-list)))
(print (atom (car (last custom-list))))

(format t "~%6.2 Checking if the first, the 3th, the last elements of custom-list are a list:")
(print (listp (car custom-list)))
(print (listp (nth 2 custom-list)))
(print (listp (car (last custom-list))))

(format t "~%7.1 Checking EQL for the first element(1) is 1")
(print (eql (car custom-list) 1))

(format t "~%7.2 Checking if the 4th(nil) element of custom-list is null:")
(print (null (nth 3 custom-list)))

(format t "~%7.3 Checking EQUALP for the second(A) element is number")
(print (numberp (second custom-list)))

(format t "~%8. Appending custom-list with the 3th element ")
(print (append custom-list (nth 2 custom-list)))

(format t "~%Task for variant 7")
(defvar list-task nil)
(defvar list-of-task nil)
(setq list-of-task '(d e f) list-task (list list-of-task (rest list-of-task) (list 'f) 4))
(print list-task)

; ((D E F) (E F) (F) 4)