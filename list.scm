(use-modules (ice-9 format))

(define filename "pokegra-hg.narc")

(let* ((narc (load-narc filename))
       (count (narc-file-count narc)))
  (do ((i 0 (1+ i)))
      ((>= i count))
    (format #t "~3d ~a~%" i
      (if (zero? (narc-get-file-size narc i))
        "(empty)"
        (let ((chunk (narc-load-file narc i)))
          (if chunk
              (get-magic chunk)
              "(error)"))))))
