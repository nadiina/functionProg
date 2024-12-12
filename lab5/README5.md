<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студентка</b>: Щербина Надія Іванівна КВ-13</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом
(п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV
файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним
типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від
типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а
також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або
структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз,
який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було
передано у select . При цьому лямбда-вираз в якості ключових параметрів може
отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку
лише заданими значеннями (виконати фільтрування). Вибірка повертається у
вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
структури у геш-таблиці
геш-таблиці у асоціативні списки
асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.

## Варіант 11
База даних: Космічні апарати
Тип записів: Геш-таблиця
Таблиці: 1. Компанії 2. Космічні апарати (База даних космічних апаратів для зв'язку,
дослідження, тощо.)

## Лістинг реалізації завдання
```lisp
(defun read-csv-to-hash-table (file-path hash-table key)
  (with-open-file (stream file-path :direction :input)
    (read-line stream)
    (cond
     ((eq key :companies)
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof))
        (let* ((fields (uiop:split-string line :separator ","))
               (id (parse-integer (first fields)))
               (name (second fields))
               (country (third fields))
               (nested-hash (make-hash-table)))
          (setf (gethash :id nested-hash) id)
          (setf (gethash :name nested-hash) name)
          (setf (gethash :country nested-hash) country)
          (setf (gethash id hash-table) nested-hash))))
     ((eq key :spacecrafts)
      (do ((line (read-line stream nil 'eof) (read-line stream nil 'eof)))
          ((eq line 'eof))
        (let* ((fields (uiop:split-string line :separator ","))
               (id (parse-integer (first fields)))
               (name (second fields))
               (type (third fields))
               (company-id (parse-integer (fourth fields)))
               (nested-hash (make-hash-table)))
          (setf (gethash :id nested-hash) id)
          (setf (gethash :name nested-hash) name)
          (setf (gethash :type nested-hash) type)
          (setf (gethash :company-id nested-hash) company-id)
          (setf (gethash id hash-table) nested-hash))))
     (t (format t "Unknown key: ~A~%" key)))))

(defun select (file-path key &rest filters)
  (lambda (&rest filters)
    (let ((result '())
          (hash-table (case key
                        (:companies (make-hash-table :test #'equal))
                        (:spacecrafts (make-hash-table :test #'equal))
                        (otherwise (error "Unknown key ~A" key)))))

      (read-csv-to-hash-table file-path hash-table key)

      (cond
       ((null filters)
        (maphash (lambda (key value)
                   (push value result))
                 hash-table))
       (t
        (let* ((filter-hash (make-hash-table :test #'equal)))
          (loop for (filter-key filter-value) on filters by #'cddr
                do (setf (gethash filter-key filter-hash) filter-value))

          (maphash (lambda (key value)
                     (let ((nested-hash value)
                           (matches t))
                       (maphash (lambda (filter-key filter-value)
                                  (let ((nested-value (gethash filter-key nested-hash)))
                                    (when (and nested-value
                                               (not (string= (write-to-string filter-value)
                                                             (write-to-string nested-value))))
                                      (setf matches nil))))
                                filter-hash)
                       (when matches
                         (push nested-hash result))))
                   hash-table))))

      (reverse result))))

(defun print-hash-tables (hash-tables)
  (let ((fields (let ((keys '()))
                  (maphash (lambda (key value)
                             (push key keys)) (first hash-tables))
                  (reverse keys))))
    (format t "~{~20A~}" (mapcar #'symbol-name fields))
    (format t "~%")
    (dolist (table hash-tables)
      (let ((values (mapcar (lambda (key) (gethash key table)) fields)))
        (format t "~{~20A~}" values)
        (format t "~%")))))

(defun write-hash-tables-to-csv (file-path data)
  (when data
    (let ((all-keys '(:id :name :country)))
      (with-open-file (stream file-path :direction :output :if-exists :supersede)
        (format stream "~{~A~^,~}~%" (mapcar #'symbol-name all-keys))

        (dolist (table data)
          (let ((values (mapcar (lambda (key)
                                  (or (gethash key table) ""))
                                all-keys)))
            (format stream "~{~A~^,~}~%" values)))))))

(defun hash-table-to-alist (hash-table)
  (let ((result '()))
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    (reverse result)))
```
### Тестові набори та утиліти
```lisp
(defun test-spacecrafts ()
  (format t "~%All data from companies.csv:~%")
  (print-hash-tables (funcall (select "C:/companies.csv" :companies)))

  (format t "~%All data from spacecrafts.csv:~%")
  (print-hash-tables (funcall (select "C:/spacecrafts.csv" :spacecrafts)))

  (format t "~%Spacecrafts by company id 1:~%")
  (print-hash-tables (funcall (select "C:/spacecrafts.csv" :spacecrafts) :company-id 1))

  (format t "~%Spacecrafts of type Research:~%")
  (print-hash-tables (funcall (select "C:/spacecrafts.csv" :spacecrafts) :type "Research"))

  (format t "~%Spacecrafts by type 'Transport' and company-id 3:~%")
  (print-hash-tables (funcall (select "C:/spacecrafts.csv" :spacecrafts) :type "Transport" :company-id 3)))

(defun test-hash-table-to-alist ()
  (let* ((hash-table (make-hash-table :test 'equal))
         (expected-alist '((:id . 1) (:name . "NeuroVision") (:model . "NeuroFluxNet"))))

    (setf (gethash :id hash-table) 1)
    (setf (gethash :name hash-table) "NeuroVision")
    (setf (gethash :model hash-table) "NeuroFluxNet")

    (let ((generated-alist (hash-table-to-alist hash-table)))
      (format t "~%Generated from hash to alist" )
      (if (equal expected-alist generated-alist)
          (format t "~%DONE: ~a~%" generated-alist)
          (format t "~%FAIL: ~a~%" generated-alist)))))


(defun test-write-companies-to-csv ()
  (let ((data (list (let ((table (make-hash-table :test 'equal)))
                      (setf (gethash :id table) 1)
                      (setf (gethash :name table) "NeuroVision")
                      (setf (gethash :country table) "USA")
                      table)
                    (let ((table (make-hash-table :test 'equal)))
                      (setf (gethash :id table) 2)
                      (setf (gethash :name table) "Starliner")
                      (setf (gethash :country table) "Germany")
                      table))))
    (write-hash-tables-to-csv "write.csv" data)

    (let ((read-data (funcall (select "write.csv" :companies))))
      (when read-data
        (format t "=== Data read from CSV: ===~%")
        (print-hash-tables read-data)))))

(defun test-all ()
  (format t "~%=== Testing full functionality ===~%")
  (test-spacecrafts)
  (test-hash-table-to-alist)
(handler-case
      (test-write-structure-to-csv)
    (error (e)
      (format t "Error in CSV test: " e)))
  (format t "~%=== All tests completed ===~%"))

(test-all)
```
### Тестування
```lisp
=== Testing full functionality ===

All data from companies.csv:
ID                  NAME                COUNTRY             
1                   SpaceX              USA
2                   Le                  Paris
3                   ESA                 Europe         
                       
All data from spacecrafts.csv:
ID                  NAME                TYPE                COMPANY-ID          
1                   Starlink            Communication       1                   
2                   Soyuz               Research            2                   
3                   Ariane              Transport           3                   

Spacecrafts by company id 1:
ID                  NAME                TYPE                COMPANY-ID          
1                   Starlink            Communication       1                   

Spacecrafts of type Research:
ID                  NAME                TYPE                COMPANY-ID          
2                   Soyuz               Research            2                   

Spacecrafts by type 'Transport' and company-id 3:
ID                  NAME                TYPE                COMPANY-ID          
3                   Ariane              Transport           3                   

Generated from hash to alist
DONE: ((ID . 1) (NAME . NeuroVision) (MODEL . NeuroFluxNet))

Data written to write.csv
=== Data read from CSV: ===
ID                  NAME                COUNTRY             
1                   NeuroVision         USA                 
2                   Starliner           Germany             

=== All tests completed ===
```
