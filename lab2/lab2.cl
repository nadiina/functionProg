(defun last-element (lst)
  "Отримує останній елемент списку."
  (if (null (cdr lst))
      (car lst)
      (last-element (cdr lst))))

(defun all-but-last (lst)
  "Повертає всі елементи, крім останнього."
  (if (null (cdr lst))
      nil
      (cons (car lst) (all-but-last (cdr lst)))))


(defun reverse-and-nest-tail (lst)
  "Рекурсивна функція, що обертає список і створює вкладену структуру з його елементів."
  (cond
    ((null lst) nil)  ;; Якщо список порожній
    ((null (cdr lst)) (list (car lst)))  ;; Якщо це останній елемент, обгортаємо його в список
    (t (list (last-element lst) (reverse-and-nest-tail (all-but-last lst))))))  ;; Вкладаємо останній елемент і рекурсивно обробляємо решту


(defun compress-helper (lst current count)
  "Допоміжна функція для compress-list."
  (cond
   ;; Якщо список пустий, повертаємо результат
   ((not lst) (if (> count 0) (cons (cons count current) nil) nil))

   ;; Якщо елемент — це число, використовуємо =, інакше порівнюємо інші типи
   ((and (numberp current) (numberp (car lst)) (= current (car lst)))
    (compress-helper (cdr lst) current (+ count 1)))

   ;; Якщо елемент не число, перевіряємо рівність через символи або рядки
   ((and (symbolp current) (symbolp (car lst)) (string= (symbol-name current) (symbol-name (car lst))))
    (compress-helper (cdr lst) current (+ count 1)))

   ;; Якщо елементи не рівні, додаємо їх у новий список
   (t (cons (cons count current) (compress-helper (cdr lst) (car lst) 1)))))

(defun compress-list (lst)
  "Рекурсивна функція, що заміщає послідовні однакові елементи двоелементними списками (кількість-повторень елемент)."
  (if (not lst)
      nil
      (compress-helper (cdr lst) (car lst) 1)))


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
