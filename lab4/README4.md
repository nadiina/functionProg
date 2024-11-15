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
(defun shell-sort (sequence &key (key #'identity) (test #'<))
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
```

### Тестові набори та утиліти першої частини
```lisp
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
```
### Тестування першої частини
```lisp
(test-shell-sort)

Testing of shell-sort
1) Sort integers in ascending order: DONE
2) Sort integers in descending order: DONE
4) Sort empty list: DONE
5) Sort single-element list: DONE
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
  "Створює замикання для циклічного зсуву кортежів.
   Параметр :shift-step задає крок зсуву, за замовчуванням дорівнює 1."
  (let ((current-shift 0))
    (lambda (&rest lists)
      (let ((tuple (mapcar #'identity lists)))
        (setf tuple (loop for i from 0 below (length tuple)
                          collect (elt tuple (mod (+ i current-shift) (length tuple)))))
        (incf current-shift shift-step)
        tuple))))        
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
```

### Тестування
```lisp
(test-merge-spinning-tuples)

Testing of merge-spinning-tuples-fn
1) Basic test with default shift-step: DONE
2) Test with :shift-step 2: DONE
3) Single list test: DONE
4) Empty list of lists: DONE
```