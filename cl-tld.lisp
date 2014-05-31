;;;; author: lu4nx <lx@shellcodes.org>
;;;; date: 2014-02-15
;;;; parsing TLD from publicsuffix.org

(in-package :cl-tld)

(defvar *tld-data* nil)
(defvar *tld-data-path* (asdf:system-relative-pathname 'cl-tld "effective_tld_names.dat"))

(defun load-data ()
  (let ((tld-data (make-array 0 :fill-pointer 0 :adjustable t)))
    (with-open-file (stream *tld-data-path*)
      (loop for line = (read-line stream nil)
            while line
            when (and
                  (string/= line "")
                  (string/= line "//" :end1 2))
              do (vector-push-extend line tld-data)))
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
  (find item *tld-data* :test #'equal))

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
              ((get-tld-data tld-!) (return (first (last items))))
              ((get-tld-data tld-*) (return (join-domain (subseq items (- i 1))))))))))
