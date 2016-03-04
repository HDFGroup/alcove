;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem #:alcove
  :components ((:file "package")
               (:file "alcove")
               (:file "file")
               (:file "messages")
               (:file "ohdr")
               (:file "superblock")
               ))
