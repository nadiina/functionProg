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
(defun shell-sort-recursive (lst)
  (let ((gaps (generate-gaps (length lst))))
    (labels ((shell-sort-recursive-helper (lst gaps)
                (if (null gaps)
                    lst
                  (let ((gap (car gaps)))
                    (shell-sort-recursive-helper (shell-sort-helper lst gap) (cdr gaps)))))
             (shell-sort-helper (lst gap)
                "Helper function that performs insertion sort on sublists with a given gap."
                (let ((sorted-lst lst))
                  (loop for i from gap below (length lst)
                        do (setf sorted-lst (insertion-sort-gap sorted-lst i gap)))
                  sorted-lst))
             (insertion-sort-gap (lst start gap)
                "Sorts the elements with a given gap using insertion sort."
                (let ((elem (nth start lst))
                      (j start))
                  (loop while (and (>= j gap) (> (nth (- j gap) lst) elem))
                        do (setf lst (replace-element lst j (nth (- j gap) lst)))
                           (setf j (- j gap)))
                  (replace-element lst j elem)))
             (replace-element (lst index value)
                "Replaces the element in lst at index with value."
                (let ((copy (copy-list lst)))
                  (setf (nth index copy) value)
                  copy)))
      (shell-sort-recursive-helper lst gaps))))
      
(defun generate-gaps (n)
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
