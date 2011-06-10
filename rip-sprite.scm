
(define filename "pokegra-hg.narc")
(define outfile "scmtest.png")

(let* ((narc (load-narc filename)))
  (let* ((ncgr (narc-load-file narc 8 'NCGR))
         (nclr (narc-load-file narc 10 'NCLR))
         (image (make-image)))

    (ncgr-decrypt-pt ncgr)
    (image-set-pixels-from-ncgr image ncgr)
    (image-set-palette-from-nclr image nclr)

    (image-save-png image outfile)))
