<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Щербина Надія Іванівна</p>
<p align="right"><b>Рік</b>: 2024</p>
## Загальне завдання
Реалізуйте дві рекурсивні функції, що виконують деякі дії з вхідним(и) списком(-ами), за
можливості/необхідності використовуючи різні види рекурсії. Функції, які необхідно
реалізувати, задаються варіантом. Вимоги до функцій:
1. Зміна списку згідно із завданням має відбуватись за рахунок конструювання нового
списку, а не зміни наявного (вхідного).
2. Не допускається використання функцій вищого порядку чи стандартних функцій
для роботи зі списками, що не наведені в четвертому розділі навчального
посібника.
3. Реалізована функція не має бути функцією вищого порядку, тобто приймати функції
в якості аргументів.
4. Не допускається використання псевдофункцій (деструктивного підходу).
5. Не допускається використання циклів.
Кожна реалізована функція має бути протестована для різних тестових наборів. Тести
мають бути оформленні у вигляді модульних тестів

## Варіант **8**
<!-- Зазначається завдання за варіантом -->
## Лістинг функції <reverse-and-nest-tail>
```lisp
(defun last-element (lst)
  (if (null (cdr lst))
      (car lst)
      (last-element (cdr lst))))

(defun all-but-last (lst)
  (if (null (cdr lst))
      nil
      (cons (car lst) (all-but-last (cdr lst)))))


(defun reverse-and-nest-tail (lst)
  (cond
    ((null lst) nil)  
    ((null (cdr lst)) (list (car lst)))  
    (t (list (last-element lst) (reverse-and-nest-tail (all-but-last lst))))))  
```
### Тестові набори
```lisp
(defun test-reverse-and-nest-tail ()
  (format t "Testing of reverse-and-nest-tail~%")
  (run-reverse-nest-test '(a b c) '(c (b (a))) "1) Basic reverse and nest")
  (run-reverse-nest-test '(1 2 3 4 5) '(5 (4 (3 (2 (1))))) "2) Reverse and nest numbers")
  (run-reverse-nest-test '() nil "3) Empty list")
  (run-reverse-nest-test '(x) '(x) "4) Single element")
  (format t " ~%")
  )
```
### Тестування
```lisp
(defun run-compress-list-test (input expected-result test-description)
  (let ((result (compress-list input)))
    (if (equal result expected-result)
        (format t "~A: DONE~%" test-description)
        (format t "~A: FAIL ~%Expected: ~A~%Got: ~A~%"
                test-description expected-result result))))

(test-reverse-and-nest-tail)


Testing of reverse-and-nest-tail
1) Basic reverse and nest: DONE
2) Reverse and nest numbers: DONE
3) Empty list: DONE
4) Single element: DONE
```
## Лістинг функції <назва другої функції>
```lisp
(defun compress-helper (lst current count)
  (cond
   ((not lst) (if (> count 0) (cons (cons count current) nil) nil))

   ((and (numberp current) (numberp (car lst)) (= current (car lst)))
    (compress-helper (cdr lst) current (+ count 1)))

   ((and (symbolp current) (symbolp (car lst)) (string= (symbol-name current) (symbol-name (car lst))))
    (compress-helper (cdr lst) current (+ count 1)))

   (t (cons (cons count current) (compress-helper (cdr lst) (car lst) 1)))))

(defun compress-list (lst)
  (if (not lst)
      nil
      (compress-helper (cdr lst) (car lst) 1)))
```
### Тестові набори
```lisp
(defun test-compress-list ()
  (format t "Testing of compress-list~%")
  (run-compress-list-test '(1 a a 3 3 3 b) '((1 . 1) (2 . A) (3 . 3) (1 . B)) "1) Basic compression")
  (run-compress-list-test '(1 1 1 1 1) '((5 . 1)) "2) All identical elements")
  (run-compress-list-test '(a a b b b c c c c c) '((2 . A) (3 . B) (5 . C)) "3) Multiple repeating groups")
  (run-compress-list-test '() nil "4) Empty list")
  (run-compress-list-test '(x) '((1 . X)) "5) Single element")
  )
```
### Тестування
```lisp
(defun run-compress-list-test (input expected-result test-description)
  (let ((result (compress-list input)))
    (if (equal result expected-result)
        (format t "~A: DONE~%" test-description)
        (format t "~A: FAIL ~%Expected: ~A~%Got: ~A~%"
                test-description expected-result result))))

(test-compress-list)

Testing of compress-list
1) Basic compression: DONE
2) All identical elements: DONE
3) Multiple repeating groups: DONE
4) Empty list: DONE
5) Single element: DONE
```
