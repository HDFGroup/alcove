;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(defpackage #:alcove
  (:use #:cl)
  (:export

   ;; alcove.lisp

   #:+hdf5-signature+
   #:read-bytes
   #:read-uinteger

   ;; file.lisp

   #:add-root-ohdr
   #:add-superblock
   #:get-file-name
   #:get-root-ohdr-address
   #:get-superblock
   #:make-file

   ;; ohdr.lisp

   #:read-ohdr

   ;; superblock.lisp

   #:find-format-signature
   #:get-root-ohdr-address
   #:read-superblock

   ))
