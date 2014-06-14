;;;; author: lu4nx <lx@shellcodes.org>
;;;; date: 2014-02-15
;;;; parsing TLD from publicsuffix.org

(in-package :cl-tld)

(defvar *tld-data* nil)
(defvar *tld-data-path* (asdf:system-relative-pathname 'cl-tld "effective_tld_names.dat"))

(defun load-data ()
  (let ((tld-data (make-hash-table :test #'equal)))
    (with-open-file (stream *tld-data-path*)
      (loop for line = (read-line stream nil)
            while line
            when (and
                  (string/= line "")
                  (string/= line "//" :end1 2))
              do (setf (gethash line tld-data) line)))
    tld-data))

(setf *tld-data* (load-data))

(defun string-split (string delimiter)
  (loop for i = 0 then (1+ n)
        for n = (position delimiter
                          string
                          :start i)
        collect (subseq string i n)
        while n))

(defun get-tld-data (item)
  (gethash item *tld-data*))

(defun join-domain (domain-item-list)
  (format nil "~{~A~^.~}" domain-item-list))

(defun get-tld (domain)
  (if (not (position #\. domain))
      nil)
  (let ((items (string-split domain #\.)))
    (dotimes (i (length items))
      (let* ((domain-item (join-domain (subseq items i)))
             (tld domain-item)
             (tld-* (concatenate 'string "*." domain-item))
             (tld-! (concatenate 'string "!" domain-item)))
        (cond ((get-tld-data tld) (return domain-item))
              ((get-tld-data tld-!) (return (join-domain (rest (subseq items i)))))
              ((get-tld-data tld-*) (return (join-domain (subseq items (- i 1))))))))))
