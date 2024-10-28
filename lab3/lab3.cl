(defun shell-sort-recursive (lst)
  "Функціональне сортування Шелла без використання циклів і псевдофункцій."
  (let ((gaps (generate-gaps-r (length lst))))
    (labels ((shell-sort-recursive-helper (lst gaps)
               (if (null gaps)
                   lst
                 (let ((gap (car gaps)))
                   (shell-sort-recursive-helper
                    (shell-sort-helper lst gap) (cdr gaps)))))

             ;; Допоміжна функція для сортування елементів з певним інтервалом
             (shell-sort-helper (lst gap)
               "Функція допомагає сортувати елементи з кроком gap"
               (labels ((sort-gap-rec (lst i gap)
                          (if (>= i (length lst))
                              lst
                            (sort-gap-rec (insertion-sort-gap lst i gap) (+ i 1) gap))))
                 (sort-gap-rec lst gap gap)))

             ;; Реалізація вставкового сорту для елементів на відстані gap
             (insertion-sort-gap (lst start gap)
               "Функція виконує вставкове сортування для елементів з кроком gap."
               (let ((elem (nth start lst)))
                 (labels ((sort-rec (lst j)
                            (if (or (< j gap) (<= (nth (- j gap) lst) elem))
                                (replace-element lst j elem)
                              (sort-rec (replace-element lst j (nth (- j gap) lst)) (- j gap)))))
                   (sort-rec lst start))))

             ;; Заміна елемента за індексом на нове значення
             (replace-element (lst index value)
               "Замінює елемент у списку на заданому індексі."
               (append (subseq lst 0 index) (list value) (nthcdr (+ 1 index) lst))))

      (shell-sort-recursive-helper lst gaps))))

(defun generate-gaps-r (n)
  (labels ((generate-gaps-rec (h gaps)
             (if (>= h n)
                 (reverse gaps)
               (generate-gaps-rec (+ (* h 3) 1) (cons h gaps)))))
    (generate-gaps-rec 1 nil)))

(defun shell-sort-imperative (lst)
  (let* ((lst-copy (copy-list lst))
         (n (length lst-copy))
         (gaps (generate-gaps n)))
    (dolist (gap gaps)
      (dotimes (i (- n gap))
        (let ((elem (nth (+ i gap) lst-copy))
              (j i))
          (loop while (and (>= j 0) (> (nth j lst-copy) elem))
                do (setf (nth (+ j gap) lst-copy) (nth j lst-copy))
                   (decf j gap))
          (setf (nth (+ j gap) lst-copy) elem))))
    lst-copy))

;; Допоміжна функція для генерації інтервалів (gaps)
(defun generate-gaps (n)
  (let ((gaps nil) (h 1))
    (loop while (< h n)
       do (push h gaps)
       (setf h (+ (* h 3) 1)))
    (reverse gaps)))

(defun check-shell-sort (name input expected sort-fn)
  "Execute `sort-fn' on `input', compare result with `expected' and print comparison status"
  (format t "~:[FAILED~;DONE~]... ~a~%"
          (equal (funcall sort-fn input) expected) name))

(defun test-shell-sort ()
  (format t " ~%")
 (format t "Running Shell Sort Tests~%")

 (check-shell-sort "Functional Shell Sort Test 1: Sorting (4 3 2 1) to (1 2 3 4)"
                    '(4 3 2 1)  '(1 2 3 4)
                   #'shell-sort-recursive)

 (check-shell-sort "Imperative Shell Sort Test 2: Sorting (4 3 2 1) to (1 2 3 4)"
                   '(4 3 2 1)  '(1 2 3 4)
                   #'shell-sort-imperative)

 (check-shell-sort "Functional Shell Sort Test 3: Sorting (7 3 9 2 5) to (2 3 5 7 9)"
                        '(7 3 9 2 5)  '(2 3 5 7 9)
                   #'shell-sort-recursive)

 (check-shell-sort "Imperative Shell Sort Test 4: Sorting (7 3 9 2 5) to (2 3 5 7 9)"
                   '(7 3 9 2 5)  '(2 3 5 7 9)
                   #'shell-sort-imperative)

 (check-shell-sort "Functional Shell Sort Test 5: Sorting (1 2 2) to (1 2 2)"
                    '(1 2 2)  '(1 2 2)
                   #'shell-sort-recursive)

 (check-shell-sort "Imperative Shell Sort Test 6: Sorting (1 2 2) to (1 2 2)"
                   '(1 2 2)  '(1 2 2)
                   #'shell-sort-imperative)

 (check-shell-sort "Functional Shell Sort Test 7: Sorting (8) to (8)"
                   '(8)  '(8)
                   #'shell-sort-recursive)

 (check-shell-sort "Imperative Shell Sort Test 8: Sorting (8) to (8)"
                   '(8)  '(8)
                   #'shell-sort-imperative)

 (format t "Tests completed.~%")
)

;(format t "Result: ~A~%" (shell-sort-recursive '(4  2 1)))
;(format t "Result: ~A~%" (shell-sort-imperative '(4 3 2 1)))

(test-shell-sort)
