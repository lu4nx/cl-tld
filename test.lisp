(in-package :cl-tld)

(defun test-get-tld ()
  (assert (string= (get-tld "www.lx.com") "com"))
  (assert (string= (get-tld "www.lx.mm") "www.lx.mm"))
  (assert (string= (get-tld "www.teledata.mz") "teledata.mz"))
  (assert (string= (get-tld "xx.www.ck") "www.ck"))
  (assert (string= (get-tld "city.kitakyushu.jp") "city.kitakyushu.jp")))
