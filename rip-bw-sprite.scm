(use-syntax (ice-9 syncase))
(use-modules (ice-9 format))

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

(let* ((n 3)
       (base (* n 20))
       (narc (load-narc filename))
       (ncgr (narc-load-file narc (+ base 2) 'NCGR))
       (ncer (narc-load-file narc (+ base 4) 'NCER))
       (nanr (narc-load-file narc (+ base 5) 'NANR))
       (nmcr (narc-load-file narc (+ base 6) 'NMCR))
       (nclr (narc-load-file narc (+ base 19) 'NCLR))
       (cell 0))
  (dotimes (frame 28)
    (let ((image (make-image '(192 128))))
      (image-set-palette-from-nclr image nclr)
      (nmcr-draw nmcr cell (* frame 7) nanr ncer ncgr image '(96 112))
      (image-save-png image (format #f "~a/~a.~2,'0d.png" outdir cell frame)))))
