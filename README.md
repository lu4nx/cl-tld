cl-tld
====
Extracts the TLD(Top Level Domain) from domain.TLD info from https://publicsuffix.org


Usage：
=====

    CL-USER> (ql:quickload "cl-tld")
    ("cl-tld")
    CL-USER> (cl-tld:get-tld "www.shellcodes.org")
    "org"

