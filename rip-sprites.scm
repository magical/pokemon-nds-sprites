(define filename "pokegra-hg.narc")
(define outdir "scmtest")

(define dirs '(("back/female" "back/female/shiny")
               ("back" "back/shiny")
               ("female" "shiny/female")
               ("" "shiny")))


(define (join-path . paths)
  (string-join paths "/"))

(define (flatten lst)
  (if (null? lst)
      lst
      (append (car lst) (flatten (cdr lst)))))

(define ENOENT 2)
(define EEXIST 17)

(define (maptail f lst)
  (if (null? lst) '()
      (cons (f lst) (maptail f (cdr lst)))))

(define (mkdir-if-not-exist dir)
  (catch 'system-error
         (lambda () (mkdir dir) #t)
         (lambda (key subr msg args data)
           (define code (list-ref data 0))
           (if (eq? code EEXIST)
               #f
               (throw key subr msg args data)))))
    
(define (mkdir-components path)
  (let* ((components (reverse! (string-split path #\/)))
         (paths (reverse! (maptail (lambda (x) (apply join-path (reverse x)))
                                   components))))
    (for-each mkdir-if-not-exist paths)))

(define (mkdir-all dir)
  (catch 'system-error
         (lambda () (mkdir-if-not-exist dir))
         (lambda (key subr msg args data)
           (define code (list-ref data 0))
           (if (eq? code ENOENT)
               (mkdir-components dir)
               (throw key subr msg args data)))))

(define (mkdirs dirs)
  (for-each mkdir-all dirs))
  
(define (narc-maybe-load-file narc i . args)
  (if (= 0 (narc-get-file-size narc i))
      #f
      (apply narc-load-file narc i args)))

(define (rip-sprites narc)
  (define (rip-pokemon n)
    (let* ((base (* n 6))
           (sprites (map (lambda (i) (narc-maybe-load-file narc (+ base i) 'NCGR))
                         (iota 4)))
           (palettes (list (narc-load-file narc (+ base 4) 'NCLR)
                           (narc-load-file narc (+ base 5) 'NCLR)))
           (image (make-image)))

      (define (rip-sprite ncgr dirs)
        (if ncgr (begin
          (ncgr-decrypt-pt ncgr)
          (image-set-pixels-from-ncgr image ncgr)
          (for-each (lambda (nclr d)
                      (define outfile (format #f "~a/~a/~a.png" outdir d n))
                      (image-set-palette-from-nclr image nclr)
                      (image-save-png image outfile))
                    palettes
                    dirs))))

      (for-each rip-sprite sprites dirs)))

  (let* ((count (narc-file-count narc))
         (n (floor (/ count 6))))
    (map rip-pokemon (cdr (iota n)))))

; -------------------------

(mkdirs (map (lambda (p) (join-path outdir p))
             (flatten dirs)))

(let ((narc (load-narc filename)))
  (rip-sprites narc))
