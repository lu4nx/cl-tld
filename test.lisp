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
  (assert (string= (get-domain-suffix "city.kitakyushu.jp") "city.kitakyushu.jp"))

  ;; TLDs not present in the PSL are considered :UNMANAGED
  (multiple-value-bind (tld type) (get-tld "lx.localhost")
    (assert (string= tld "localhost"))
    (assert (eq type :unmanaged)))

  (multiple-value-bind (tld type) (get-tld "com")
    (assert (string= tld "com"))
    (assert (eq type :icann)))

  ;; Even though this domain has a #\., it's for a private TLD without a
  ;; registerable domain part.
  (eq nil (ignore-errors (get-domain-suffix "co.krd")))
  (eq nil (ignore-errors (get-domain-suffix "com")))
  (eq nil (ignore-errors (get-domain-suffix "")))
  (eq nil (ignore-errors (get-tld ""))))
