;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage :alcove
  (:use :cl)
  (:export

   ;; alcove.lisp

   +hdf5-signature+
   read-bytes
   read-uinteger

   ;; file.lisp

   get-file-name
   get-superblock

   ;; superblock.lisp

   find-format-signature
   get-root-ohdr-address
   read-superblock

   ))
