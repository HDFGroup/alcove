;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:alcove)


(defun read-ohdr-message (input-stream ohdr file)
  (let ((ohdr-version (cdr (assoc 'ohdr-version ohdr)))
        (size-of-lengths (get-size-of-lengths file))
        (size-of-offsets (get-size-of-offsets file)))
    (cond
      ;; version 1 OHDR
      ((= ohdr-version 1)
       (let* ((msg-type (read-uinteger input-stream 2))
              (msg-size (read-uinteger input-stream 2))
              (msg-flags (read-byte input-stream))
              (reserved (read-bytes input-stream 3)))
         `((msg-type . ,msg-type)
           (msg-data-size . ,msg-size)
           (msg-flags . ,msg-flags)
           (msg-data . ,(parse-ohdr-msg-data input-stream msg-type msg-size
                                             msg-flags ohdr file)))))

      ;; version 2 OHDR
      ((= ohdr-version 2)
       (let* ((msg-type (read-byte input-stream))
              (msg-size (read-uinteger input-stream 2))
              (msg-flags (read-byte input-stream))
              (msg-crt-order (when (cdr (assoc 'track-attr-crt-order ohdr))
                               (read-uinteger input-stream 2))))
         `((msg-type . ,msg-type)
           (msg-data-size . ,msg-size)
           (msg-flags . ,msg-flags)
           (msg-crt-order . ,msg-crt-order)
           (msg-data . ,(parse-ohdr-msg-data input-stream msg-type msg-size
                                             msg-flags ohdr file)))))
      ;; default
      (t nil))))


(defun parse-ohdr-msg-data (input-stream msg-type msg-size msg-flags ohdr file)
  (let ((result nil)
        (size-of-offsets (get-size-of-offsets file))
        (size-of-lengths (get-size-of-lengths file)))
    (cond
      ((= msg-type 0)
       `((name . "NIL")
         (data . ,(read-bytes input-stream msg-size))))

      ((= msg-type 2)
       (let* ((version (read-byte input-stream))
              (flags (read-byte input-stream))
              (result `((name . "Link Info")
                        (version . ,version)
                        (flags . ,flags))))
         (when (ldb-test (byte 1 0) flags)
           (nconc result
                  `((max-creation-index . ,(read-uinteger input-stream 8)))))
         (nconc result
                `((fractal-heap-address . ,(read-uinteger input-stream
                                                          size-of-offsets))
                  (name-index-v2-b-tree-address . ,(read-uinteger
                                                    input-stream
                                                    size-of-offsets))))
         (when (ldb-test (byte 1 1) flags)
           (nconc result
                  `((creation-order-index-v2-b-tree-address
                     .
                     ,(read-uinteger input-stream size-of-offsets)))))
         result))

      ((= msg-type 10)
       (let* ((version (read-byte input-stream))
              (flags (read-byte input-stream))
              (result `((name . "Group Info")
                        (version . ,version)
                        (flags . ,flags))))
         (when (ldb-test (byte 1 0) flags)
           (nconc result
                  `((max-compact-value . ,(read-uinteger input-stream 2))
                    (min-dense-value . ,(read-uinteger input-stream 2)))))
         (when (ldb-test (byte 1 1) flags)
           (nconc result
                  `((est-num-entries . ,(read-uinteger input-stream 2))
                    (est-link-name-length . ,(read-uinteger input-stream 2)))))
         result))

      ((= msg-type 17)
       `((name . "Symbol Table")
         (v1-b-tree-address . ,(read-uinteger input-stream size-of-offsets))
         (local-heap-address . ,(read-uinteger input-stream size-of-offsets))))

      ((= msg-type 18)
       `((name . "Object Modification Time")
         (version . ,(read-byte input-stream))
         (reserved . ,(read-bytes input-stream 3))
         (seconds-after-unix-epoch . ,(read-uinteger input-stream 4))))

      (t (read-bytes input-stream msg-size)))))
