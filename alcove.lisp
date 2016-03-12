;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*

(in-package #:alcove)


(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                           ,@ (when doc (list doc))))


(define-constant +hdf5-signature+
  (make-array 8 :element-type 'unsigned-byte
              :initial-contents '(137 72 68 70 13 10 26 10))
  "The HDF5 file format signature.")


(define-constant +heap-signature+
  (map 'vector #'char-code '(#\H #\E #\A #\P))
  "The local heap signature.")


(define-constant +snod-signature+
  (map 'vector #'char-code '(#\S #\N #\O #\D))
  "The symbol table node signature.")


(define-constant +tree-signature+
  (map 'vector #'char-code '(#\T #\R #\E #\E))
  "The v1 B-tree symbol table node signature.")


(defun read-bytes (input-stream count)
  "--------------------------------------------------------------[function-doc]
READ-BYTES
Args: (INPUT-STREAM COUNT)
Read COUNT bytes from INPUT-STREAM. Return a byte array.
------------------------------------------------------------------------------"
  (let ((result (make-array count :element-type 'unsigned-byte)))
    (assert (= count (read-sequence result input-stream)))
    result))


(defun read-uinteger (input-stream count)
  "--------------------------------------------------------------[function-doc]
READ-UINTEGER
Args: (INPUT-STREAM COUNT)
Read COUNT bytes from INPUT-STREAM. Return an (unsigned) integer.
------------------------------------------------------------------------------"
  (let ((result 0)
	(buffer (read-bytes input-stream count)))
    (dotimes (i count result)
      (incf result (* (ash 1 (* 8 i)) (aref buffer i))))))
