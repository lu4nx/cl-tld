(in-package :cl-tld)

(defun test-get-tld ()
  (assert (string= (get-tld "www.lx.com") "com"))
  (assert (string= (get-tld "www.lx.mm") "lx.mm"))
  (assert (string= (get-tld "a.b.kobe.jp") "b.kobe.jp"))
  (assert (string= (get-tld "www.teledata.mz") "mz"))
  (assert (string= (get-tld "xx.www.ck") "ck"))
  (assert (string= (get-tld "city.kitakyushu.jp") "jp")))
