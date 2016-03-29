(require 'package)
(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(push '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
(package-initialize)

;; install packages that are missing
(defvar my-packages '(evil evil-leader undo-tree neotree))
;; install missing packages
; (package-refresh-contents)
; (dolist (p my-packages)
;  (when (not (package-installed-p p))
;   (package-install p)))

;; Evil
(setq evil-want-C-u-scroll t)
(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "wh" 'evil-window-left
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wl" 'evil-window-right
  "bd" 'evil-buffer-delete
  "w>" 'evil-window-increase-height
  "w<" 'evil-window-decrease-height)
(evil-mode 1)
(define-key evil-normal-state-map "u" 'undo-tree-undo)
(define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)

;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; neotree
(require 'neotree)
(evil-leader/set-key "[" 'neotree-toggle)
(add-hook 'neotree-mode-hook
 (lambda ()
  (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

; include local .emacs.d/lib in path
(add-to-list 'load-path "~/.emacs.d/lib")

; load custom darais input method
(load-library "unicode")
(set-input-method "darais")

; ; make mac option key be meta
; (setq mac-option-modifier 'meta)
; (setq mac-command-modifier 'super)
; ; Disable Aquamacs Toolbar
; (tool-bar-mode 0)
; ; Start maximized
; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Agda
(load-file (let ((coding-system-for-read 'utf-8))
            (shell-command-to-string "agda-mode locate")))
(add-hook 'agda2-mode-hook (lambda () (set-input-method "darais")))
(add-hook 'evil-insert-state-entry-hook (lambda () (set-input-method "darais")))
(add-hook 'evil-normal-state-entry-hook (lambda () (set-input-method "darais")))

;; Proof General
(load-file "~/lib/ProofGeneral-4.3/generic/proof-site.el")

(custom-set-variables
 '(tool-bar-mode nil))
(custom-set-faces)
