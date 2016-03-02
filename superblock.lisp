;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:alcove)


(defun find-format-signature (input-stream
                              &optional (max-offset
                                         (file-length input-stream)))
  "--------------------------------------------------------------[function-doc]
FIND-FORMAT-SIGNATURE
Args: (INPUT-STREAM &OPTIONAL MAX-OFFSET)
Return (if found) the offset of the HDF5 file format signature.
Don't search beyond MAX-OFFSET (Default: EOF).
------------------------------------------------------------------------------"
  (assert (file-position input-stream 0))
  ;; define a helper function to recursively look for the signature
  (labels ((find-it (in-stream offset)
             (let ((buffer (make-array 8 :element-type 'unsigned-byte))
                   (offset (file-position input-stream)))
               (when (and offset
                          (< (+ offset 8) max-offset)
                          (read-sequence buffer input-stream))
                 (if (every #'= +hdf5-signature+ buffer)
                     offset
                     (when (and (setq offset (if (< 0 offset)
                                                 (* 2 offset)
                                                 512))
                                (< offset max-offset)
                                (file-position input-stream offset))
                       (find-it input-stream max-offset)))))))
    (find-it input-stream max-offset)))


(defun read-superblock (input-stream)
  "--------------------------------------------------------------[function-doc]
READ-SUPERBLOCK
Args: (INPUT-STREAM)
Locate the HDF5 file format signature in INPUT-STREAM and read the superblock.
Return the superblock as an association list or NIL of the file format
signature is not found.
------------------------------------------------------------------------------"
  (assert (file-position input-stream 0))
  ; find the file format signature
  (let ((offset (find-format-signature input-stream)))
    (assert (not (null offset)))
    ; seek to the file format signature
    (assert (file-position input-stream (+ 8 offset)))
    ; read the superblock version and take it from here
    (let ((version (read-byte input-stream)))
      (assert (and (<= 0 version) (<= version 2)))
      (cond
        ((or (= version 0)
             (= version 1))
         (read-v01-superblock version input-stream offset))
        ((= version 2)
          (read-v2-superblock input-stream offset))))))


(defun read-v01-superblock (version input-stream offset)
  "--------------------------------------------------------------[function-doc]
READ-V01-SUPERBLOCK
Args: (VERSION INPUT-STREAM OFFSET)
Read a VERSION 0 or 1 superblock from INPUT-STREAM at OFFSET. Return the
superblock as an association list.
------------------------------------------------------------------------------"
  (assert (or (= version 0) (= version 1)))
  (let* ((free-space-version (read-byte input-stream))
         (root-stbl-version (read-byte input-stream))
         (reserved1 (read-byte input-stream))
         (sohm-version (read-byte input-stream))
         (size-of-offsets (read-byte input-stream))
         (size-of-lengths (read-byte input-stream))
         (reserved2 (read-byte input-stream)))
    (assert (= reserved1 0))
    (assert (= reserved2 0))
    (append
     `((sblk-version . ,version)
       (free-space-version . ,free-space-version)
       (root-stbl-version . ,root-stbl-version)
       (sohm-version . ,sohm-version)
       (size-of-offsets . ,size-of-offsets)
       (size-of-lengths . ,size-of-lengths)
       (group-leaf-node-k . ,(read-uinteger input-stream 2))
       (group-internal-node-k . ,(read-uinteger input-stream 2))
       (file-consistency-flags . ,(read-bytes input-stream 4)))
     ;; version 1 only ===
     (when (= version 1)
       (let* ((indexed-storage-internal-node-k (read-uinteger input-stream 2))
              (reserved3 (read-bytes input-stream 2)))
         `((indexed-storage-internal-node-k
            .
            ,indexed-storage-internal-node-k))))
     ;; ==================
     `((base-address
        .
        ,(let ((base-address
                (read-uinteger input-stream size-of-offsets)))
           (if (= base-address offset)
               base-address
               (+ offset base-address))))
       (free-space-info-address
        .
        ,(read-uinteger input-stream size-of-offsets))
       (eof-address . ,(read-uinteger input-stream size-of-offsets))
       (driver-info-block-address
        .
        ,(read-uinteger input-stream size-of-offsets))
       ;; root group symbol table entry
       (root-stbl-entry
        .
        ,(let* ((link-name-offset
                 (read-uinteger input-stream size-of-offsets))
                (ohdr-address
                 (read-uinteger input-stream size-of-offsets))
                (cache-type (read-bytes input-stream 4))
                (reserved (read-bytes input-stream 4))
                (scratch (read-bytes input-stream 16)))
           (assert (every #'= reserved '#(0 0 0 0)))
           `((link-name-offset . ,link-name-offset)
             (ohdr-address . ,ohdr-address)
             (cache-type . ,cache-type)
             (scratch . ,scratch))))))))


(defun read-v2-superblock (input-stream offset)
  "--------------------------------------------------------------[function-doc]
READ-V2-SUPERBLOCK
Args: (INPUT-STREAM OFFSET)
Read a version 2 superblock from INPUT-STREAM at OFFSET. Return the superblock
as a an association list.
------------------------------------------------------------------------------"
  (let ((size-of-offsets (read-byte input-stream)))
    `((sblk-version . 2)
      (size-of-offsets . ,size-of-offsets)
      (size-of-lengths . ,(read-byte input-stream))
      (file-consistency-flags . ,(read-byte input-stream))
      (base-address
       .
       ,(let ((base-address
               (read-uinteger input-stream size-of-offsets)))
          (if (= base-address offset)
              base-address
              (+ offset base-address))))
      (sblk-ext-address
       . ,(read-uinteger input-stream size-of-offsets))
      (eof-address . ,(read-uinteger input-stream size-of-offsets))
      (root-ohdr-address . ,(read-uinteger input-stream
                                           size-of-offsets))
      (sblk-checksum . ,(read-bytes input-stream 4)))))
