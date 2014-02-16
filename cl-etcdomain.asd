(asdf:defsystem #:cl-etcdomain
  :description "拆分域名结构，提取顶级域名、二级域名"
  :version "0.1"
  :author "lu4nx <lx@shellcodes.org>"
  :licence "Public Domain"
  :components ((:file "package")
               (:file "etcdomain")))
