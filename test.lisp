(in-package :cl-tld)

(defun test-cl-tld ()
  (assert (string= (get-tld "www.lx.com") "com"))
  (assert (string= (get-tld "www.lx.mm") "lx.mm"))
  (assert (string= (get-tld "a.b.kobe.jp") "b.kobe.jp"))
  (assert (string= (get-tld "www.teledata.mz") "mz"))
  (assert (string= (get-tld "xx.www.ck") "ck"))
  (assert (string= (get-tld "city.kitakyushu.jp") "kitakyushu.jp"))

  (assert (string= (get-domain-suffix "xx.www.ck") "www.ck"))
  (assert (string= (get-domain-suffix "www.lx.mm") "www.lx.mm"))
  (assert (string= (get-domain-suffix "city.kitakyushu.jp") "city.kitakyushu.jp")))
