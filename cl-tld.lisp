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
            when (and (string/= line "")
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

(defun get-domain-tld (domain-item-list)
  (dotimes (i (length domain-item-list))
    (let* ((domain-item (join-domain (subseq domain-item-list i)))
           (tld domain-item)
           (tld-* (concatenate 'string "*." domain-item))
           (tld-! (concatenate 'string "!" domain-item)))
      (cond ((get-tld-data tld) (return domain-item))
            ((get-tld-data tld-!) (return (join-domain (rest (subseq domain-item-list i)))))
            ((get-tld-data tld-*) (return (join-domain (subseq domain-item-list (- i 1)))))))))

(defun is-domain-p (domain)
  (position #\. domain))

(defun get-tld (domain)
  (if (not (is-domain-p domain))
      (error "not is a domain")
      (get-domain-tld (string-split domain #\.))))

(defun get-domain-suffix (domain)
  (if (not (is-domain-p domain))
      (error "not is a domain")
      (let* ((domain-item-list (string-split domain #\.))
             (domain-tld (get-domain-tld domain-item-list))
             (dot-count (count #\. domain-tld)))
        (concatenate 'string
                     (nth (- (length domain-item-list)
                             (1+ dot-count)
                             1)
                          domain-item-list)
                     "."
                     domain-tld))))
