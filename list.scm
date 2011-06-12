(use-modules (ice-9 format))

(define filename "pokegra-w.narc")

(define (try-load-file narc i)
  (catch 'narc-error
         (lambda () (narc-load-file narc i))
         (lambda (key . args) #f)))

(let* ((narc (load-narc filename))
       (count (narc-file-count narc)))
  (do ((i 0 (1+ i)))
      ((>= i count))
    (format #t "~3d ~a~%" i
      (let ((chunk (try-load-file narc i)))
        (if chunk
            (get-magic chunk)
            (if (zero? (narc-get-file-size narc i))
              "(empty)"
              "(error)"))))))
