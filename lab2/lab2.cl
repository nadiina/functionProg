(defun reverse-and-nest-tail (lst)
  (labels ((helper (lst)
             (if (null (cdr lst))
                 (list (car lst))
                 (list (car lst) (helper (cdr lst)))))
           (reverse-helper (remaining acc)
             "Допоміжна функція для реверсування списку."
             (if (null remaining)
                 acc  
                 (reverse-helper (cdr remaining) (cons (car remaining) acc)))))
    (if (null lst)
        nil
        (helper (reverse-helper lst nil)))))

(defun compress-list (lst)
  "Рекурсивна функція, що заміщає послідовні однакові елементи двоелементними списками (кількість-повторень елемент)."
  (if (null lst)
      nil
      (compress-helper (cdr lst) (car lst) 1)))

(defun compress-helper (lst current count)
  (cond
   ((null lst) (list (cons count current)))
   ((eql current (car lst))
    (compress-helper (cdr lst) current (+ count 1)))
   (t (cons (cons count current) (compress-helper (cdr lst) (car lst) 1)))))

(defun run-reverse-nest-test (input expected-result test-description)
  (let ((result (reverse-and-nest-tail input)))
    (if (equal result expected-result)
        (format t "~A: DONE~%" test-description)
        (format t "~A: FAIL ~%Expected: ~A~%Got: ~A~%"
                test-description expected-result result))))

(defun test-reverse-and-nest-tail ()
  (format t "Testing of reverse-and-nest-tail~%")
  (run-reverse-nest-test '(a b c) '(c (b (a))) "1) Basic reverse and nest")
  (run-reverse-nest-test '(1 2 3 4 5) '(5 (4 (3 (2 (1))))) "2) Reverse and nest numbers")
  (run-reverse-nest-test '() nil "3) Empty list")
  (run-reverse-nest-test '(x) '(x) "4) Single element")
  (format t " ~%")
  )

(defun run-compress-list-test (input expected-result test-description)
  (let ((result (compress-list input)))
    (if (equal result expected-result)
        (format t "~A: DONE~%" test-description)
        (format t "~A: FAIL ~%Expected: ~A~%Got: ~A~%"
                test-description expected-result result))))

(defun test-compress-list ()
  (format t "Testing of compress-list~%")
  (run-compress-list-test '(1 a a 3 3 3 b) '((1 . 1) (2 . A) (3 . 3) (1 . B)) "1) Basic compression")
  (run-compress-list-test '(1 1 1 1 1) '((5 . 1)) "2) All identical elements")
  (run-compress-list-test '(a a b b b c c c c c) '((2 . A) (3 . B) (5 . C)) "3) Multiple repeating groups")
  (run-compress-list-test '() nil "4) Empty list")
  (run-compress-list-test '(x) '((1 . X)) "5) Single element")
  )

;(format t "Result: ~A~%" (reverse-and-nest-tail '(1 2 3)))
;(format t "Result: ~A~%" (compress-list  '(1 2 2 3 3 3 a)))

(test-reverse-and-nest-tail)
(test-compress-list)
