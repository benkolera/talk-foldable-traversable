(defun slide-init ()
  (interactive)
  (slide-init-haskell)
  (slide-init-slides))

(defun slide-init-slides ()
  (select-window (slide-slide-window))
  (setq org-src-fontify-natively t)
  ;; fontify stuff doesn't come into effect unless you reload, so lets
  ;; do that.
  (revert-buffer t t)
  (beginning-of-buffer)
  (org-cycle)
  (org-narrow-to-subtree)
  (text-scale-set 3)
  (fci-mode 0)
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
  (text-scale-set 3)
  (fci-mode 0)
  (evil-emacs-state)
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
  (slide-move n (lambda () (outline-next-visible-heading 1))))

(defun slide-prev (n)
  (interactive "^p")
  (slide-move n (lambda () (outline-previous-visible-heading 1))))

(defun slide-maximize-haskell ()
  (interactive)
  (select-window (slide-haskell-window))
  (maximize-window))

(defun slide-normal-layout ()
  (interactive)
  (run-haskell)
  (select-window (slide-haskell-window))
  (minimize-window)
  (window-resize nil 3 nil)
  (select-window (slide-slide-window)))

(defun slide-slide-window ()
  ;; TODO This is a giant turd.
  (caddr (car (window-tree))))

(defun slide-haskell-window ()
  ;; TODO A turd that has been C&P from a giant turd. Turd ^ Turd :(
  (cadddr (car (window-tree))))

(defun run-haskell-line-or-region ()
  (interactive)
  (slide-haskell-send-command (get-line-or-region)))

(defun slide-haskell-send-command (cmd)
  (inferior-haskell-send-command (inferior-haskell-process) cmd))

(defun get-line-or-region ()
  (let ((start (if (use-region-p) (mark) (point-at-bol)))
        (end (if (use-region-p) (point) (- (point-at-bol 2) 1))))
    (buffer-substring-no-properties start end)))
