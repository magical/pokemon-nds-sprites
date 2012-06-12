(use-syntax (ice-9 syncase))

(define filename "pokegra-w.narc")
(define outdir "cells")

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

(define (rip-cells n ncer ncgr nclr outdir)
  (let ((image (make-image)))
    (image-set-palette-from-nclr image nclr)
    (dotimes (i (ncer-get-cell-count ncer))
      (image-set-pixels-from-ncer image ncer i ncgr)
      (image-save-png image (format #f "~a/~a-~a.png" outdir n i)))))

(let ((narc (load-narc filename)))
  (dotimes (n 711)
    (let ((base (* n 20)))
      (let ((ncgr (narc-load-file narc (+ base 2) 'NCGR))
            (ncer (narc-load-file narc (+ base 4) 'NCER))
            (nclr (narc-load-file narc (+ base 18) 'NCLR))
            (outdir_n (format #f "~a/~a" outdir n)))
        (format #t "~a~%" n)
        (mkdir-if-not-exist outdir_n)
        (rip-cells n ncer ncgr nclr outdir_n)))))
