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
        (let ((filter-hash (make-hash-table :test #'equal)))
          ;; Build the filter hash table
          (loop for (filter-key filter-value) on filters by #'cddr
                do (setf (gethash filter-key filter-hash) filter-value))

          ;; Apply filters
          (maphash (lambda (key value)
                     (let ((nested-hash (and (hash-table-p value) value))
                           (matches t))
                       (when nested-hash
                         ;; Compare values considering their types
                         (maphash (lambda (filter-key filter-value)
                                    (let ((nested-value (gethash filter-key nested-hash)))
                                      (when (and nested-value
                                                 (not (eql filter-value nested-value)))
                                        (setf matches nil))))
                                  filter-hash)
                         (when matches
                           (push nested-hash result)))))
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
  "Writes a list of hash tables to a CSV file. Assumes all tables have the same structure."
  (when data
    (let ((all-keys '(:id :name :country)))
      (with-open-file (stream file-path :direction :output :if-exists :supersede)
        ;; Write the header row
        (format stream "~{~A~^,~}~%" (mapcar #'symbol-name all-keys))

        ;; Write each hash table as a row
        (dolist (table data)
          (let ((values (mapcar (lambda (key)
                                  (or (gethash key table) ""))
                                all-keys)))
            (format stream "~{~A~^,~}~%" values)))))))

(defun hash-table-to-alist (hash-table)
  "Converts a hash-table to an association list."
  (let ((result '()))
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    (reverse result)))

;; Тестування
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
  (print-hash-tables (funcall (select "C:/spacecrafts.csv" :spacecrafts) :type "Transport" :company-id 3))
  )

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
  "Test the write-hash-tables-to-csv function with company data."
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

