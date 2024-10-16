;;;; author: lu4nx <lx@shellcodes.org>
;;;; date: 2014-02-15
;;;; parsing TLD from publicsuffix.org

(in-package :cl-tld)

(defvar *tld-data* (make-hash-table :test #'equal))
(defvar *tld-data-path* (asdf:system-relative-pathname 'cl-tld "effective_tld_names.dat"))

(defun add-domain-name (domain-name &optional (type :unmanaged))
  (setf (gethash domain-name *tld-data*) type))

(defun load-data ()
  (with-open-file (stream *tld-data-path*)
    (loop for line = (read-line stream nil)
          with icann = :icann
          while line
          when (string= line "// ===BEGIN PRIVATE DOMAINS===")
            do (setf icann :private)
          when (and (string/= line "")
                    (string/= line "//" :end1 2))
            do (add-domain-name line icann))))

(load-data)

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
  (let ((length (length domain-item-list)))
   (dotimes (i length)
     (let* ((num-parts (- length i))
            (domain-item (join-domain (subseq domain-item-list i)))
            (tld domain-item)
            (tld-* (concatenate 'string "*." domain-item))
            (tld-! (concatenate 'string "!" domain-item)))
       (cond ((get-tld-data tld) (return (values domain-item
                                                 (get-tld-data tld))))
             ((get-tld-data tld-!) (return (values (join-domain (rest (subseq domain-item-list i)))
                                                   (get-tld-data tld-!))))
             ((get-tld-data tld-*) (return (values (join-domain (subseq domain-item-list (- i 1)))
                                                   (get-tld-data tld-*))))
             ((= 1 num-parts) (return (values domain-item :unmanaged))))))))

(defun is-domain-p (domain)
  (position #\. domain))

(defun get-tld (domain)
  (if (or (null domain)
          (string= "" domain))
      (error (format nil "\"~a\" is not a TLD." domain))
      (get-domain-tld (string-split domain #\.))))

(defun get-domain-suffix (domain)
  (if (not (is-domain-p domain))
      (error (format nil "\"~a\" is not a domain." domain))
      (let ((domain-item-list (string-split domain #\.)))
        (multiple-value-bind (domain-tld type) (get-domain-tld domain-item-list)
          (if (string= domain-tld domain)
              (error (format nil "\"~a\" is private TLD, not a registerable domain." domain))
              (let ((dot-count (count #\. domain-tld)))
                (values
                 (concatenate 'string
                              (nth (- (length domain-item-list)
                                      (1+ dot-count)
                                      1)
                                   domain-item-list)
                              "."
                              domain-tld)
                 type)))))))
