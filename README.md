
<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>

<p align="center">
<b>Звіт з лабораторної роботи 1</b><br/>
"Обробка списків з використанням базових функцій"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right">Студентка: Щербина Надія Іванівна, КВ-13</p>
<p align="right"> Рік: 2024</p>

## Загальне завдання

### Пункт 1: Створення списку
```lisp
(setq custom-list (list 1 'a (list 'b 'c) ()))

(1 A (B C) NIL)
```

### Пункт 2: Отримання голови списку
```lisp
(car custom-list)

1
```

### Пункт 3: Отримання хвоста списку
```lisp
(cdr custom-list)

(A (B C) NIL) 
```

### Пункт 4: Отримання третього елемента списку
```lisp
(nth 2 custom-list)

(B C) 
```

### Пункт 5: Отримання останнього елемента списку
```lisp
(first (last custom-list))

NIL
```

### Пункт 6: Використання ATOM та LISTP
```lisp
(atom (car custom-list))
(atom (nth 2 custom-list))
(atom (car (last custom-list)))
T 
NIL 
T 

(listp (car custom-list))
(listp (nth 2 custom-list))
(listp (car (last custom-list)))
NIL 
T 
T 
```

### Пункт 7: Використання предикатів(eql, null, numberp)
```lisp
(eql (car custom-list) 1)
T

(null (nth 3 custom-list))
T

(numberp (second custom-list))
NIL
```

### Пункт 8: Об'єднання списків
```lisp
(append custom-list (nth 2 custom-list))

(1 A (B C) NIL B C)
```

## Варіант 7

<p align="center">
<img src="lab1.png">
</p>

### Створення списку за варіантом 7:
```lisp
(defvar list-task nil)
(defvar list-of-task nil)
(setq list-of-task '(d e f) list-task (list list-of-task (rest list-of-task) (list 'f) 4))
(print list-task)

; ((D E F) (E F) (F) 4) 
```



