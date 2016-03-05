;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:alcove)


(defun add-superblock (input-stream file)
  (nconc file `((superblock . ,(read-superblock input-stream)))))


(defun get-base-adress (file)
  (cdr (assoc 'base-address (get-superblock file))))


(defun get-file-name (file)
  (cdr (assoc 'file-name file)))


(defun get-size-of-lengths (file)
  (cdr (assoc 'size-of-lengths (get-superblock file))))


(defun get-size-of-offsets (file)
 (cdr (assoc 'size-of-offsets (get-superblock file))))


(defun get-root-ohdr-address (file)
  (let* ((sblk (get-superblock file))
         (version (cdr (assoc 'sblk-version sblk))))
    (cond
      ((or (= version 0) (= version 1))
       (+ (cdr (assoc 'base-address sblk))
          (cdr (assoc 'ohdr-address (cdr (assoc 'root-stbl-entry sblk))))))
      ((= version 2)
       (+ (cdr (assoc 'base-address sblk))
          (cdr (assoc 'root-ohdr-address sblk))))
      (t nil))))


(defun get-superblock (file)
  (cdr (assoc 'superblock file)))


(defun make-file (file-name)
  `((file-name . ,file-name)))
