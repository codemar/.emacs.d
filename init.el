;;
;; Melpa
;;
(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;
;; Stuff
;;

(setq ring-bell-function 'ignore)

;; because M-S-< stopped working 
(global-set-key (kbd "C-<") 'end-of-buffer)

;; delete current selection when typing
(delete-selection-mode 1)

;; disable unusefull and distracting commands
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Theme
(load-theme 'badwolf t)

;;Disable start page
(setq inhibit-startup-screen t)


;; Autopair
(autopair-global-mode 1)

;; Anzu mode
(global-anzu-mode +1)

;; Switching windows
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;;Speedbar
(global-set-key (kbd "C-<tab> b") 'sr-speedbar-toggle)
(setq sr-speedbar-skip-other-window-p t)

;;Hippie-Expand
(define-key elpy-mode-map (kbd "C-S-x") 'hippie-expand)

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

(global-linum-mode 1)

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



;;Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;Company mode
(require 'company)
(global-company-mode t)
(add-to-list 'company-backends 'company-c-headers)


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

(package-initialize)
(elpy-enable)


(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

;;Avoid conflict between elpy and company

(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0)
 '(flymake-allowed-file-name-masks
   (quote
    (("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init nil nil)
     ("\\.xml\\'" flymake-xml-init nil nil)
     ("\\.html?\\'" flymake-xml-init nil nil)
     ("\\.cs\\'" flymake-simple-make-init nil nil)
     ("\\.p[ml]\\'" flymake-perl-init nil nil)
     ("\\.php[345]?\\'" flymake-php-init nil nil)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup nil)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup nil)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup nil)
     ("\\.tex\\'" flymake-simple-tex-init nil nil)
     ("\\.idl\\'" flymake-simple-make-init nil nil))))
 '(global-company-mode t)
 '(package-selected-packages
   (quote
    (anzu markdown-mode ssh magit helm-projectile flycheck company-c-headers sr-speedbar helm-gtags ggtags undo-tree py-autopep8 nyan-mode helm elpy better-defaults badwolf-theme autopair)))
 '(speedbar-verbosity-level 0)
 '(sr-speedbar-default-width 20))


;;
;; C / C++
;;

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)



;;Latex

;; AUCTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)
(require 'tex-site)

;; RefTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) 
(add-hook 'latex-mode-hook 'turn-on-reftex)
(setq TeX-PDF-mode t)
