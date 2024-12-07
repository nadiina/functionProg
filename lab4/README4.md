<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Щербина Надія Іванівна</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
   роботи 3 з такими змінами:
   використати функції вищого порядку для роботи з послідовностями (де це
   доречно);
   додати до інтерфейсу функції (та використання в реалізації) два ключових
   параметра: key та test , що працюють аналогічно до того, як працюють
   параметри з такими назвами в функціях, що працюють з послідовностями. При
   цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
   варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
   можливості, має бути мінімізоване.

## Варіант першої частини 8
Алгоритм сортування Шелла за незменшенням.

## Лістинг реалізації першої частини завдання
```lisp
(defun shell-sort-recursive (lst &key (key #'identity) (test #'<))
  (let* ((length-lst (length lst))
         (gaps (generate-gaps length-lst)))
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

(defun generate-gaps (n)
  (let ((gaps nil) (h 1))
    (loop while (< h n)
       do (push h gaps)
       (setf h (+ (* h 3) 1)))
    (reverse gaps)))
```

### Тестові набори та утиліти першої частини
```lisp
(defun run-shell-sort-test (input key test expected-result test-description)
  (let ((result (shell-sort-recursive input :key key :test test)))
    (if (equal result expected-result)
        (format t "~A: DONE~%" test-description)
        (format t "~A: FAIL~%Expected: ~A~%Got: ~A~%~%"
                test-description expected-result result))))

(defun test-shell-sort-functional ()
  (format t "Тестування shell-sort-functional~%")
  (run-shell-sort-test '(5 3 8 6 2) #'identity #'< '(2 3 5 6 8) "1) Integers in ascending order")
  (run-shell-sort-test '(5 3 8 6 2) #'identity #'> '(8 6 5 3 2) "2) Integers in descending order")
  (run-shell-sort-test '(3 1 4 1 5 9 2 6) #'identity #'< '(1 1 2 3 4 5 6 9) "3) With duplicates")
  (run-shell-sort-test '("date" "apple" "carrot" "banana") #'identity #'string< '("apple" "banana" "carrot" "date") "4) Strings in alphabetical order")
  (run-shell-sort-test '("apple" "banana" "carrot") #'length #'> '("banana" "carrot" "apple") "5) Strings by length in reverse order")
  (run-shell-sort-test '() #'identity #'< '() "6) An empty list")
  (run-shell-sort-test '(42) #'identity #'< '(42) "7) A list with a single element"))

```
### Тестування першої частини
```lisp
(test-shell-sort)

Testing of shell-sort
1) Integers in ascending order: DONE
2) Integers in descending order: DONE
3) With duplicates: DONE
4) Strings in alphabetical order: DONE
5) Strings by length in reverse order: DONE
6) An empty list: DONE
7) A list with a single element: DONE
```

## Варіант другої частини 11
Написати функцію merge-spinning-tuples-fn , яка має один ключовий параметр —
shift-step . merge-spinning-tuples-fn має повернути функцію, яка при застосуванні в
якості першого аргументу mapcar робить наступне: об'єднує всі поточні елементи зі
списків-аргументів mapcar в один список (кортеж), циклічно зміщуючи елементи в
кортежі. Величина зміщення збільшується для кожного наступного кортежу на значення
shift-step , починаючи з 0. Якщо shift-step не зазначений користувачем, тоді
величина "кроку" зміщення — 1.
```lisp
CL-USER> (mapcar (merge-spinning-tuples-fn) '(1 2 3) '(a b c))
((1 A) (B 2) (3 C))
CL-USER> (mapcar (merge-spinning-tuples-fn :shift-step 2)
'(a b c)
'(1 2 3)
'(d e f))
((A 1 D) (E B 2) (3 F C))
```

## Лістинг функції з використанням деструктивного підходу
```lisp
(defun merge-spinning-tuples-fn (&key (shift-step 1))
  (let ((current-shift 0))
    (lambda (&rest tuple)
      (let* ((len (length tuple))
             (shifted-tuple (loop for i from 0 below len
                                  collect (elt tuple (mod (+ i current-shift) len)))))
        (incf current-shift shift-step)
        shifted-tuple))))        
```
### Тестові набори та утиліти
```lisp
(defun run-merge-spinning-tuples-test (lists shift-step expected-result test-description)
  (let* ((closure (merge-spinning-tuples-fn :shift-step shift-step))
         (result (apply #'mapcar closure lists)))
    (if (equal result expected-result)
        (format t "~A: DONE~%" test-description)
        (format t "~A: FAIL~%Expected: ~A~%Got: ~A~%~%"
                test-description expected-result result))))

(defun test-merge-spinning-tuples ()
  (format t "Testing merge-spinning-tuples-fn~%")
  (run-merge-spinning-tuples-test '((1 2 3) (a b c)) 1 '((1 A) (B 2) (3 C)) "1) Basic test with default shift-step")
  (run-merge-spinning-tuples-test '((a b c) (1 2 3) (d e f)) 2 '((A 1 D) (E B 2) (3 F C)) "2) Test with :shift-step 2")
  (run-merge-spinning-tuples-test '((x y z)) 1 '((X) (Y) (Z)) "3) Single list test")
  (run-merge-spinning-tuples-test '(()) 1 '() "4) Empty list test"))
```

### Тестування
```lisp
(test-merge-spinning-tuples)

Testing of merge-spinning-tuples-fn
1) Basic test with default shift-step: DONE
2) Test with :shift-step 2: DONE
3) Single list test: DONE
4) Empty list test: DONE
```
