;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem #:alcove
  :name "alcove"
  :serial t
  :components ((:file "package")
               (:file "alcove")
               (:file "file")
               (:file "superblock")
               (:file "messages")
               (:file "ohdr")
               ))
