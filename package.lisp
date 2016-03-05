;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:alcove
  (:use #:cl)
  (:export

   ;; alcove.lisp

   #:+hdf5-signature+
   #:read-bytes
   #:read-uinteger

   ;; file.lisp

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
