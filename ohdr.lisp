;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package :alcove)


(defun read-ohdr (input-stream address file)
  "--------------------------------------------------------------[function-doc]
READ-OHDR
Args: (INPUT-STREAM ADDRESS FILE)
Read an object header (OHDR) from INPUT-STREAM at ADDRESS.
Return an association list.
------------------------------------------------------------------------------"

  (assert (file-position input-stream address))

  (let ((peek (read-byte input-stream)))
    (cond
      ;; version 1 OHDR
      ((= peek 1)
       (let* ((result (read-v1-ohdr-prefix input-stream file))
             (message-count (cdr (assoc 'message-count result))))
         (nconc result
                (list
                 (cons 'messages
                       (loop for m from 1 to message-count
                          collect
                            (let ((pos (file-position input-stream)))
                              ;; in v1 header messages are 8-byte aligned
                              (file-position input-stream
                                             (incf pos (mod pos 8)))
                              (read-ohdr-message input-stream result file))))))
         result))

      ;; version 2+ OHDR
      ((= peek 79)
       (let* ((h (read-byte input-stream))
              (d (read-byte input-stream))
              (r (read-byte input-stream))
              (version (read-byte input-stream)))
         (when (and (= h 72) (= d 68) (= r 82))
           (cond
             ;; version 2 OHDR
             ((= version 2)
              (let* ((result (read-v2-ohdr-prefix input-stream file))
                     (size-of-chunk0 (cdr (assoc 'size-of-chunk0 result)))
                     (messages nil))
                (when (< 0 size-of-chunk0)
                  (do* ((processed 0)
                        (rem size-of-chunk0 (- size-of-chunk0 processed)))
                       ((< rem 6)
                        (when (< 0 rem)
                          (nconc result
                                 (list (cons 'gap
                                             (read-bytes input-stream rem)))))
                        (nconc result
                               (list (cons 'checksum
                                           (read-bytes input-stream 4))))
                        result)
                    (let ((msg (read-ohdr-message input-stream result file)))
                      (incf processed (+ 6 (cdr (assoc 'msg-data-size msg))))
                      (setf messages (cons msg messages)))))
                (nconc result (list (cons 'messages (nreverse messages))))
                result))

             ;; unknown version
             (t (list (cons 'signature "OHDR") (cons 'version version)))))))
      (t nil))))


(defun read-v1-ohdr-prefix (input-stream file)
  (let* ((reserved (read-byte input-stream))
         (message-count (read-uinteger input-stream 2)))
    (list `(ohdr-version . 1)
          `(message-count . ,message-count)
          `(reference-count . ,(read-uinteger input-stream 4))
          `(header-size ,(read-uinteger input-stream 4)))))


(defun read-v2-ohdr-prefix (input-stream file)
  (let* ((result (list '(signature . "OHDR")
                       '(ohdr-version . 2)))
         (flags (read-byte input-stream))
         (size-of-chunk0 (ash 1 (ldb (byte 2 0) flags)))
         (track-attr-crt-order (ldb-test (byte 1 2) flags))
         (index-attr-crt-order (ldb-test (byte 1 3) flags))
         (cust-attr-stor-phase-change (ldb-test (byte 1 4) flags))
         (amcb-times-stored (ldb-test (byte 1 5) flags)))
    (nconc result
           (list
            `(flags . ,flags)
            `(track-attr-crt-order . ,track-attr-crt-order)
            `(index-attr-crt-order . ,index-attr-crt-order)
            `(cust-attr-stor-phase-change . ,cust-attr-stor-phase-change)
            `(abcm-times-stored . ,amcb-times-stored)))
    ;; timestamps
    (when amcb-times-stored
      (nconc result
             (list
              `(access-time . ,(read-uinteger input-stream 4))
              `(modification-time . ,(read-uinteger input-stream 4))
              `(change-time . ,(read-uinteger input-stream 4))
              `(birth-time . ,(read-uinteger input-stream 4)))))
    ;; attribute storage phase change
    (when cust-attr-stor-phase-change
      (nconc result
             (list
              `(max-compact-attr . ,(read-uinteger input-stream 2))
              `(min-dense-attr . ,(read-uinteger input-stream 2)))))
    ;; magic chunk0 size
    (nconc result
           (list `(size-of-chunk0
                   .
                   (read-uinteger input-stream size-of-chunk0))))
    result))
