<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Щербина Надія Іванівна</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
   конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
   списку. Не допускається використання: деструктивних операцій, циклів, функцій
   вищого порядку або функцій для роботи зі списками/послідовностями, що
   використовуються як функції вищого порядку. Також реалізована функція не має
   бути функціоналом (тобто приймати на вхід функції в якості аргументів).

2. Імперативний варіант реалізації має базуватись на використанні циклів і
   деструктивних функцій (псевдофункцій). Не допускається використання функцій
   вищого порядку або функцій для роботи зі списками/послідовностями, що
   використовуються як функції вищого порядку. Тим не менш, оригінальний список
   цей варіант реалізації також не має змінювати, тому перед виконанням
   деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
   Також реалізована функція не має бути функціоналом (тобто приймати на вхід
   функції в якості аргументів).

## Варіант **8** 
Алгоритм сортування Шелла за незменшенням.

## Лістинг функції з використанням конструктивного підходу
```lisp
(defun shell-sort-recursive (lst &key (key #'identity) (test #'<))
  (let* ((length-lst (length lst))
         (gaps (generate-gaps-r length-lst)))
    (labels ((shell-sort-recursive-helper (lst gaps)
               (if (null gaps)
                   lst
                   (shell-sort-recursive-helper (shell-sort-helper lst (car gaps) length-lst) (cdr gaps))))

             (shell-sort-helper (lst gap length-lst)
               (if (>= gap length-lst)
                   lst
                   (sort-gap-rec lst 0 gap length-lst)))

             (sort-gap-rec (lst i gap length-lst)
               (if (>= i length-lst)
                   lst
                   (sort-gap-rec (insertion-sort-gap lst i gap) (+ i 1) gap length-lst)))

             (insertion-sort-gap (lst start gap)
               (let ((elem (funcall key (nth start lst))))
                 (sort-rec lst start gap elem)))

             (sort-rec (lst j gap elem)
               (if (or (< j gap) (funcall test (funcall key (nth (- j gap) lst)) elem))
                   (replace-nth lst j elem)
                   (sort-rec (replace-nth lst j (nth (- j gap) lst)) (- j gap) gap elem))))

      (shell-sort-recursive-helper lst gaps))))

(defun replace-nth (lst index value)
  (if (zerop index)
      (cons value (cdr lst))
      (cons (car lst) (replace-nth (cdr lst) (1- index) value))))

(defun generate-gaps-r (n)
  (let ((gaps nil) (h 1))
    (loop while (< h n)
       do (push h gaps)
       (setf h (+ (* h 3) 1)))
    (reverse gaps)))
```
### Тестові набори
```lisp
 (check-shell-sort "Functional Shell Sort Test 1: Sorting (4 3 2 1) to (1 2 3 4)"
                    '(4 3 2 1)  '(1 2 3 4)
                   #'shell-sort-recursive)

 (check-shell-sort "Functional Shell Sort Test 3: Sorting (7 3 9 2 5) to (2 3 5 7 9)"
                        '(7 3 9 2 5)  '(2 3 5 7 9)
                   #'shell-sort-recursive)

 (check-shell-sort "Functional Shell Sort Test 5: Sorting (1 2 2) to (1 2 2)"
                    '(1 2 2)  '(1 2 2)
                   #'shell-sort-recursive)

 (check-shell-sort "Functional Shell Sort Test 7: Sorting (8) to (8)"
                   '(8)  '(8)
                   #'shell-sort-recursive)
```
### Тестування
```lisp
DONE... Functional Shell Sort Test 1: Sorting (4 3 2 1) to (1 2 3 4)
DONE... Functional Shell Sort Test 3: Sorting (7 3 9 2 5) to (2 3 5 7 9)
DONE... Functional Shell Sort Test 5: Sorting (1 2 2) to (1 2 2)
DONE... Functional Shell Sort Test 7: Sorting (8) to (8)
```
## Лістинг функції з використанням деструктивного підходу
```lisp
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

(defun generate-gaps (n)
  (let ((gaps nil) (h 1))
    (loop while (< h n)
       do (push h gaps)
       (setf h (+ (* h 3) 1)))
    (reverse gaps)))
```
### Тестові набори
```lisp
 (check-shell-sort "Imperative Shell Sort Test 2: Sorting (4 3 2 1) to (1 2 3 4)"
                   '(4 3 2 1)  '(1 2 3 4)
                   #'shell-sort-imperative)

 (check-shell-sort "Imperative Shell Sort Test 4: Sorting (7 3 9 2 5) to (2 3 5 7 9)"
                   '(7 3 9 2 5)  '(2 3 5 7 9)
                   #'shell-sort-imperative)

 (check-shell-sort "Imperative Shell Sort Test 6: Sorting (1 2 2) to (1 2 2)"
                   '(1 2 2)  '(1 2 2)
                   #'shell-sort-imperative)

 (check-shell-sort "Imperative Shell Sort Test 8: Sorting (8) to (8)"
                   '(8)  '(8)
                   #'shell-sort-imperative)
```
### Тестування
```lisp
DONE... Imperative Shell Sort Test 2: Sorting (4 3 2 1) to (1 2 3 4)
DONE... Imperative Shell Sort Test 4: Sorting (7 3 9 2 5) to (2 3 5 7 9)
DONE... Imperative Shell Sort Test 6: Sorting (1 2 2) to (1 2 2)
DONE... Imperative Shell Sort Test 8: Sorting (8) to (8)
```
