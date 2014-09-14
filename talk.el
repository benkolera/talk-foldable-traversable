(defun slide-init ()
  (interactive)
  (slide-init-haskell)
  (slide-init-slides))

(defun slide-init-slides ()
  (select-window (slide-slide-window))
  (beginning-of-buffer)
  (org-cycle)
  (org-narrow-to-subtree)
  (text-scale-set 3)
  (fci-mode 0)
  (setq org-src-fontify-natively t)
  ;; If this weren't a dodgy hack, I'd find a way to make this buffer
  ;; local
  (set-face-attribute 'org-level-1 nil :height 2.0)
  (set-face-attribute
   'org-block-begin-line nil
   :height 0.5
   :inherit 'org-hide)

  (set-face-attribute
   'org-block-end-line nil
   :height 0.5
   :inherit 'org-hide))

(defun slide-init-haskell ()
  (run-haskell)
  (select-window (slide-haskell-window))
  (slide-normal-layout))

(defun slide-move (n move)
   (ignore-errors (outline-up-heading nil))
   (org-cycle)
   (widen)
   (dotimes (i n) (funcall move))
   (org-cycle)
   (org-narrow-to-subtree))

(defun slide-next (n)
  (interactive "^p")
  (move-slide n (lambda () (outline-next-visible-heading 1))))

(defun slide-prev (n)
  (interactive "^p")
  (move-slide n (lambda () (outline-previous-visible-heading 1))))

(defun slide-maximize-haskell ()
  (interactive)
  (select-window (slide-haskell-window))
  (maximize-window))

(defun slide-normal-layout ()
  (interactive)
  (select-window (slide-haskell-window))
  (minimize-window)
  (window-resize nil 3 nil))

(defun slide-slide-window ()
  ;; TODO This is a giant turd.
  (caddr (car (window-tree))))

(defun slide-haskell-window ()
  ;; TODO A turd that has been C&P from a giant turd. Turd ^ Turd :(
  (cadddr (car (window-tree))))

(defun run-haskell-line-or-region ()
  (interactive)
  (inferior-haskell-send-command
   (inferior-haskell-process)
   (get-line-or-region)))

(defun get-line-or-region ()
  (let ((start (if (use-region-p) (mark) (point-at-bol)))
        (end (if (use-region-p) (point) (- (point-at-bol 2) 1))))
    (buffer-substring-no-properties start end)))
