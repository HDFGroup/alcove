;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :alcove)


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
Read the superblock from INPUT-STREAM. Return the superblock as a property
list.
------------------------------------------------------------------------------"
  (assert (file-position input-stream 0))
  ; find the file format signature
  (let ((offset (find-format-signature input-stream)))
    (assert (not (null offset)))
    ; seek to the file format signature
    (assert (file-position input-stream (+ 8 offset)))
    ; read the superblock version and take it from here
    (let ((version (read-byte input-stream)))
      (cond
        ((= version 0)  ;; version 0 superblock
         (let* ((free-space-version (read-byte input-stream))
                (root-stbl-version (read-byte input-stream))
                (reserved1 (read-byte input-stream))
                (sohm-version (read-byte input-stream))
                (size-of-offsets (read-byte input-stream))
                (size-of-lengths (read-byte input-stream))
                (reserved2 (read-byte input-stream)))
           (assert (= reserved1 0))
           (assert (= reserved2 0))
           (list `(sblk-version . 0)
                 `(free-space-version . ,free-space-version)
                 `(root-stbl-version . ,root-stbl-version)
                 `(sohm-version . ,sohm-version)
                 `(size-of-offsets . ,size-of-offsets)
                 `(size-of-lengths . ,size-of-lengths)
                 `(group-leaf-node-k . ,(read-uinteger input-stream 2))
                 `(group-internal-node-k . ,(read-uinteger input-stream 2))
                 `(file-consistency-flags . ,(read-bytes input-stream 4))
                 `(base-address
                   .
                   ,(let ((base-address
                           (read-uinteger input-stream size-of-offsets)))
                         (if (= base-address offset)
                             base-address
                             (+ offset base-address))))
                 `(free-space-info-address
                   .
                   ,(read-uinteger input-stream size-of-offsets))
                 `(eof-address . ,(read-uinteger input-stream size-of-offsets))
                 `(driver-info-block-address
                   .
                   ,(read-uinteger input-stream size-of-offsets))
                 `(root-stbl-entry
                   .
                   ,(let* ((link-name-offset
                            (read-uinteger input-stream size-of-offsets))
                           (ohdr-address
                            (read-uinteger input-stream size-of-offsets))
                           (cache-type (read-bytes input-stream 4))
                           (reserved (read-bytes input-stream 4))
                           (scratch (read-bytes input-stream 16)))
                          (assert (every #'= reserved '#(0 0 0 0)))
                          (list `(link-name-offset . ,link-name-offset)
                                `(ohdr-address . ,ohdr-address)
                                `(cache-type . ,cache-type)
                                `(scratch . ,scratch)))))))
        ((= version 1)  ;; version 1 superblock
         (let* ((free-space-version (read-byte input-stream))
                (root-stbl-version (read-byte input-stream))
                (reserved1 (read-byte input-stream))
                (sohm-version (read-byte input-stream))
                (size-of-offsets (read-byte input-stream))
                (size-of-lengths (read-byte input-stream))
                (reserved2 (read-byte input-stream)))
           (assert (= reserved1 0))
           (assert (= reserved2 0))
           (list :sblk-version 1
                 :free-space-version free-space-version
                 :root-stbl-version root-stbl-version
                 :sohm-version sohm-version
                 :size-of-offsets size-of-offsets
                 :size-of-lengths size-of-lengths
                 :group-leaf-node-k (read-uinteger input-stream 2)
                 :group-internal-node-k (read-uinteger input-stream 2)
                 :file-consistency-flags (read-bytes input-stream 4)
                 :indexed-storage-internal-node-k (read-uinteger input-stream
                                                                 2)
                 :reserved3 (read-bytes input-stream 2)
                 :base-address
                 (let ((base-address
                        (read-uinteger input-stream size-of-offsets)))
                   (if (= base-address offset)
                       base-address
                       (+ offset base-address)))
                 :free-space-info-address
                 (read-uinteger input-stream size-of-offsets)
                 :eof-address (read-uinteger input-stream size-of-offsets)
                 :driver-info-block-address
                 (read-uinteger input-stream size-of-offsets)
                 :root-stbl-entry
                 (list :link-name-offset
                       (read-uinteger input-stream size-of-offsets)
                       :ohdr-address (read-uinteger input-stream
                                                    size-of-offsets)
                       :cache-type (read-bytes input-stream 4)
                       :reserved (read-bytes input-stream 4)
                       :scratch (read-bytes input-stream 16)))))
        ((= version 2)  ;; version 2 superblock
         (let ((size-of-offsets (read-byte input-stream)))
           (list `(sblk-version . 2)
                 `(size-of-offsets . ,size-of-offsets)
                 `(size-of-lengths . ,(read-byte input-stream))
                 `(file-consistency-flags . ,(read-byte input-stream))
                 `(base-address
                   . ,(let ((base-address
                             (read-uinteger input-stream size-of-offsets)))
                           (if (= base-address offset)
                               base-address
                               (+ offset base-address))))
                 `(sblk-ext-address
                   . ,(read-uinteger input-stream size-of-offsets))
                 `(eof-address . ,(read-uinteger input-stream size-of-offsets))
                 `(root-ohdr-address . ,(read-uinteger input-stream
                                                       size-of-offsets))
                 `(sblk-checksum . ,(read-bytes input-stream 4)))))))))
