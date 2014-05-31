;;;; author: lu4nx <lux@knownsec.com>
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

(defun get-tld (domain)
  (if (not (position #\. domain))
      nil)
  (let* ((items (string-split domain #\.)))
    (dotimes (i (length items))
      (let* ((domain-item (format nil
                                 "~{~A~^.~}"
                                 (subseq items i)))
             (tld domain-item)
             (tld-* (concatenate 'string "*." domain-item))
             (tld-! (concatenate 'string "!" domain-item)))
        (cond ((get-tld-data tld) (return domain-item))
              ((get-tld-data tld-!) (return domain-item))
              ((get-tld-data tld-*) (return domain)))))))

(defun test-get-tld ()
  (assert (string= (get-tld "www.lx.com") "com"))
  (assert (string= (get-tld "www.lx.mm") "www.lx.mm"))
  (assert (string= (get-tld "www.teledata.mz") "teledata.mz"))
  (assert (string= (get-tld "xx.www.ck") "www.ck"))
  (assert (string= (get-tld "city.kitakyushu.jp") "city.kitakyushu.jp")))
