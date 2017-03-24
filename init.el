;;
;; Melpa
;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;;
;; Stuff
;;

(setq ring-bell-function 'ignore)

;; because M-S-< stopped working 
;; (global-set-key (kbd "C-<") 'end-of-buffer)
;; works again

;; delete current selection when typing
(delete-selection-mode t)

;; disable unusefull and distracting commands
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Theme
(load-theme 'badwolf t)

;;Disable start page
(setq inhibit-startup-screen t)


;; Autopair
(autopair-global-mode t)

;; Anzu mode
(global-anzu-mode +1)

;; Bind Anzu
(bind-key "M-R" 'anzu-query-replace)

;; Switching windows
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Remove annoying keybindings
(global-unset-key (kbd "C-x C-<left>"))
(global-unset-key (kbd "C-x C-<up>"))
(global-unset-key (kbd "C-x C-<right>"))
(global-unset-key (kbd "C-x C-<down>"))


;; Bind find file in project which works with git/svn
(bind-key "M-F" 'find-file-in-project)



;;Hippie-Expand
(global-set-key (kbd "C-S-x") 'hippie-expand)

;; Backward line kill
(defun backward-kill-line (arg)
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-j") 'backward-kill-line)

;; Smart move to line beginning
;; C-a now move the point to the beginning of the indentation
(defun smart-line-beginning ()
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))
(global-set-key (kbd "C-a") 'smart-line-beginning)


;;
;; Undo-tree
;;

(global-undo-tree-mode)
(global-set-key (kbd "C-z") 'undo) 
(global-set-key (kbd "C-S-z") 'undo-tree-redo)


;;
;; Linum mode
;;

(global-linum-mode t)

;; set fixed height to 100, so linum works while zooming in/out
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#222120" :foreground "#666462" :height 100)))))


;;
;; Magit
;;


(global-set-key (kbd "C-x g") 'magit-status)


;; Company mode
(require 'company)
(global-company-mode t)
 
;;
;; Helm
;;

;; Activating helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)

;; Hotkeys in helm
(define-key helm-find-files-map (kbd "<tab>")         'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-<backspace>") 'helm-find-files-up-one-level)
(define-key helm-map (kbd "C-z") 'helm-select-action)

;; Global hotkeys
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
;; kill ring in helm / disabled for now
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; Fuzzy matching for buffers
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;;
;; Python
;;

;; (package-initialize)
;; (elpy-enable)

;; (require 'py-autopep8)
;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;; ;;Avoid conflict between elpy and company

;; (defun company-yasnippet-or-completion ()
;;   "Solve company yasnippet conflicts."
;;   (interactive)
;;   (let ((yas-fallback-behavior
;;          (apply 'company-complete-common nil)))
;;     (yas-expand)))

;; (add-hook 'company-mode-hook
;;           (lambda ()
;;             (substitute-key-definition
;;              'company-complete-common
;;              'company-yasnippet-or-completion
;;              company-active-map)))


;;
;; Ocaml
;;

;; Load tuareg
;; (load "/home/omar/.opam/4.03.0/share/emacs/site-lisp/tuareg.el")
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")

;;
;; JavaScript
;;

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; tern
(add-hook 'js-mode-hook (lambda () (tern-mode t)))

;; company-tern
(add-to-list 'company-backends 'company-tern)



;;
;; Auto-generated
;;


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0)
 '(package-selected-packages
   (quote
    (company-tern tern js2-refactor bind-key free-keys js2-mode w3m undo-tree tuareg ssh py-autopep8 nyan-mode merlin markdown-mode magit jedi helm-projectile helm-gtags flycheck elpy company-c-headers better-defaults badwolf-theme autopair auctex anzu))))
