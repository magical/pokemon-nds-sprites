(use-syntax (ice-9 syncase))

(define filename "pokegra-w.narc")
(define outdir "scmtest")

(define ENOENT 2)
(define EEXIST 17)

(define (mkdir-if-not-exist dir)
  (catch 'system-error
         (lambda () (mkdir dir) #t)
         (lambda (key subr msg args rest)
           (define code (car rest))
           (if (eq? code EEXIST)
               #f
               (throw key subr msg args rest)))))

(define-syntax dotimes (syntax-rules ()
  ((dotimes (var times) body ...)
   (let loop ((var 0) (ret #f))
     (if (< var times)
         (loop (1+ var) (begin body ...))
         ret)))))

(mkdir-if-not-exist outdir)

; Venusaur: 37 cells

(let* ((n 643)
       (base (* n 20))
       (narc (load-narc filename))
       (ncgr (narc-load-file narc (+ base 2) 'NCGR))
       (ncer (narc-load-file narc (+ base 4) 'NCER))
       (nanr (narc-load-file narc (+ base 5) 'NANR))
       (nclr (narc-load-file narc (+ base 18) 'NCLR)))
  (dotimes (cell (nanr-cell-count nanr))
    (dotimes (frame (nanr-frame-count nanr cell))
      (let ((image (make-image '(100 100))))
        (image-set-palette-from-nclr image nclr)
        (nanr-draw-frame nanr cell frame ncer ncgr image)
        (image-save-png image (format #f "~a/~a.~a.png" outdir cell frame))))))
