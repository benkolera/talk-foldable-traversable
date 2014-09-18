;; Notes
;; - Need ditaa installed and set up in org mode to generate images
;;   from ascii diagrams
;; Todo
;; - Probably want to make the org face changes a theme so that they
;;   can be unloaded later.
;; - Can probably hook into whatever ditaa uses with babel to run an
;;   entire haskell code block with C-c C-c as well as the existing
;;   by line and by region stuff.
;; - PDF / HTML generation should not be too much of a stretch.
;; - Probably want some kind of shortcut for adding a new slide so you
;;   don't have to widen and add things yourself.

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

(defun slide-first ()
  (beginning-of-buffer)
)

(defun slide-move (n move)
   (outline-back-to-heading)
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
  (window-resize nil 4 nil)
  (select-window (slide-slide-window)))

(defun slide-slide-window ()
  ;; TODO This is a giant turd.
  (caddr (car (window-tree))))

(defun slide-haskell-window ()
  ;; TODO A turd that has been C&P from a giant turd. Turd ^ Turd :(
  (cadddr (car (window-tree))))

(defun run-haskell-line-or-region ()
  (interactive)
  (let ((select (get-line-or-region))
        (proc (inferior-haskell-process)))
    (if (> (length (split-string select "\n" t)) 1)
      (progn
        (comint-send-string proc (inferior-haskell-wrap-decl select))
        (slide-haskell-send-command "-- Multiline command evaluated"))
      (slide-haskell-send-command select))))

(defun slide-haskell-send-command (cmd)
  (inferior-haskell-send-command (inferior-haskell-process) cmd))

(defun get-line-or-region ()
  (let ((start (if (use-region-p) (mark) (point-at-bol)))
        (end (if (use-region-p) (point) (- (point-at-bol 2) 1))))
    (buffer-substring-no-properties start end)))
