(defun merge-spinning-tuples-fn (&key (shift-step 1))
  "Створює замикання для циклічного зсуву кортежів.
   Параметр :shift-step задає крок зсуву, за замовчуванням дорівнює 1."
  (let ((current-shift 0))
    (lambda (&rest lists)
      (let ((tuple (mapcar #'identity lists)))
        (setf tuple (loop for i from 0 below (length tuple)
                          collect (elt tuple (mod (+ i current-shift) (length tuple)))))
        (incf current-shift shift-step)
        tuple))))

;(format t "1: ~a~%"
;        (mapcar (merge-spinning-tuples-fn) '(1 2 3) '(a b c)))
;(format t "2 з shift-step 2: ~a~%"
;        (mapcar (merge-spinning-tuples-fn :shift-step 2) '(a b c) '(1 2 3) '(d e f)))

(defun shell-sort (sequence &key (key #'identity) (test #'<))
  "Сортує SEQUENCE за допомогою алгоритму Шелла. Підтримує ключові параметри KEY і TEST."
  (let ((n (length sequence))
        (stages nil))
    (let ((t-val (if (< n 4)
                     1
                     (floor (log n 2) 1))))
      (setf stages (reverse (loop for i from 0 to (1- t-val)
                                  collect (1- (ash 1 i))))))
    (dolist (gap stages)
      (loop for i from gap below n do
            (let ((elem (elt sequence i))
                  (j i))
              (loop while (and (>= j gap)
                               (funcall test
                                        (funcall key elem)
                                        (funcall key (elt sequence (- j gap)))))
                    do (setf (elt sequence j) (elt sequence (- j gap)))
                       (setf j (- j gap)))
              (setf (elt sequence j) elem)))))
  sequence)

;(format t "shell-sort: ~a~%"
;        (shell-sort '(3 1 4 1 5 9 2 6 5) :key #'identity :test #'<))

(defun run-merge-spinning-tuples-test (lists shift-step expected-result test-description)
  (let* ((closure (merge-spinning-tuples-fn :shift-step shift-step))
         (result (apply #'mapcar closure lists)))
    (if (equal result expected-result)
        (format t "~A: DONE~%" test-description)
        (format t "~A: FAIL~%Expected: ~A~%Got: ~A~%~%"
                test-description expected-result result))))

(defun test-merge-spinning-tuples ()
  (format t "Testing of merge-spinning-tuples-fn~%")
  (run-merge-spinning-tuples-test
   '((1 2 3) (a b c))
   1
   '((1 A) (B 2) (3 C))
   "1) Basic test with default shift-step")

  (run-merge-spinning-tuples-test
   '((a b c) (1 2 3) (d e f))
   2
   '((A 1 D) (E B 2) (3 F C))
   "2) Test with :shift-step 2")

  (run-merge-spinning-tuples-test
   '((x y z))
   1
   '((X) (Y) (Z))
   "3) Single list test")

  (run-merge-spinning-tuples-test
   '(())
   1
   '()
   "4) Empty list of lists"))

(defun run-shell-sort-test (input key test expected-result test-description)
  (let ((result (shell-sort input :key key :test test)))
    (if (equal result expected-result)
        (format t "~A: DONE~%" test-description)
        (format t "~A: FAIL~%Expected: ~A~%Got: ~A~%~%"
                test-description expected-result result))))

(defun test-shell-sort ()
  (format t "Testing of shell-sort~%")
  (run-shell-sort-test
   '(5 3 8 6 2)
   #'identity #'<
   '(2 3 5 6 8)
   "1) Sort integers in ascending order")

  (run-shell-sort-test
   '(5 3 8 6 2)
   #'identity #'>
   '(8 6 5 3 2)
   "2) Sort integers in descending order")

  (run-shell-sort-test
   '()
   #'identity #'<
   '()
   "4) Sort empty list")

  (run-shell-sort-test
   '(42)
   #'identity #'<
   '(42)
   "5) Sort single-element list")
  )

(test-shell-sort)
(test-merge-spinning-tuples)
