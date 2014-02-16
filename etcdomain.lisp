;;;; author: lu4nx(lx@shellcodes.org)
;;;; date: 2014-02-15

(in-package :cl-etcdomain)

;; 顶级域名数据
(defvar *tld-data* (make-array 0
                               :fill-pointer 0
                               :adjustable t))

(defun get-tld-data-path ()
  (asdf:system-relative-pathname 'cl-etcdomain "tld.dat"))

(defun load-tld-data ()
  "加载TLD数据，数据来源：http://mxr.mozilla.org/mozilla/source/netwerk/dns/src/effective_tld_names.dat?raw=1"
  (with-open-file (stream (get-tld-data-path))
    (loop for line = (read-line stream nil)
      while line
      when (and
        (string/= line "")
        (string/= line "//" :end1 2))
        do (vector-push-extend line *tld-data*))))

(load-tld-data)

(defun string-split-by-dot (string)
  "按小数点分割"
  (loop for i = 0 then (1+ n)
    for n = (position #\.
              string
              :start i)
    collect (subseq string i n)
    while n))

(defun in-tld-data-p (item)
  (find item *tld-data* :test #'equal))

(defun items->string (itmes)
  (let ((result (format nil "~{.~A~}" itmes)))
    (subseq result 1)))

(defun get-tld-items (domain)
  "返回域名的顶级域名（Top-Level Domain,TLD）"
  (if (not (position #\. domain))
      nil)
  (let ((result (loop for item
              in (reverse (string-split-by-dot domain))
              when (in-tld-data-p item)
                collect item)))
    (reverse result)))

(defun get-tld (domain)
  (items->string (get-tld-items domain)))

(defun get-sld-itmes (domain)
  "返回域名的二级域名（Second-Level Domain,SLD）"
  (let* ((tld-items (get-tld-items domain))
     (domain-items (string-split-by-dot domain)))
    (subseq domain-items (1- (- (length domain-items)
                                (length tld-items))))))

(defun get-sld (domain)
  (items->string (get-sld-itmes domain)))
