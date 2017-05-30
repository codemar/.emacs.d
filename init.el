;;
;; Melpa
;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

; list the packages you want
(setq package-list '(autopair anzu nlinum magit company helm elpy tuareg merlin
			      yasnippet js2-mode tern company-tern rtags company-rtags
			      irony flycheck cmake-ide bind-key undo-tree py-autopep8
			      company-irony-c-headers flycheck-rtags zenburn-theme better-defaults flycheck-irony
                              company-irony rust-mode racer electric eldoc flycheck-rust cargo))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


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
(load-theme 'zenburn t)

;; Font size for external monitor

(defun set-font-size (size)
  (interactive "nFont size: ")
  (set-face-attribute 'default nil :height size))

;;Disable start page
(setq inhibit-startup-screen t)

;; Autopair
(electric-pair-mode 1)
(show-paren-mode 1)

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
;; nlinum mode
;;

;;(global-linum-mode t)
(global-nlinum-mode t)

;; set fixed height to 100, so linum works while zooming in/out
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#3f3f3f" :foreground "#666462" :height 100)))))


;;
;; Magit
;;

(global-set-key (kbd "C-x g") 'magit-status)


;; Company mode
(require 'company)
(global-company-mode t)
;; keybind
(global-set-key (kbd "<C-tab>") 'company-complete)


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


;;
;; ;; Ocaml
;; ;;

;; ;; Load tuareg
;; (load "/home/omar/.opam/4.03.0/share/emacs/site-lisp/tuareg.el")
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; (require 'tuareg)

;; ;; Load merlin mode
;; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;   (when (and opam-share (file-directory-p opam-share))
;;     (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;     (autoload 'merlin-mode "merlin" nil t nil)
;;     (add-hook 'tuareg-mode-hook 'merlin-mode t)))

;;
;; JavaScript
;;

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))




(defun my-tern-mode-config ()
  "For use in 'tern-mode-hook'."
  (local-set-key (kbd "C-c C-t") 'tern-get-type)
  (local-unset-key (kbd "C-c C-c"))
  ;; Todo
  )


;; tern
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'tern-mode-hook 'my-tern-mode-config)

(setenv "PATH" (concat (getenv "PATH") ":/home/omar/node_modules/tern/bin/"))
    (setq exec-path (append exec-path '("/home/omar/node_modules/tern/bin/")))




;; company-tern
(add-to-list 'company-backends 'company-tern)

;; (require 'indium)
;; (add-hook 'js-mode-hook #'indium-interaction-mode)



;; 
;; ;; C/C++
;; ;; 

;; (require 'rtags)
;; (require 'company-rtags)

;; (setq rtags-completions-enabled t)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)

;; ;; To index a C/C++ project:
;; ;; $ rdm &
;; ;; $ cd /path/to/project/root
;; ;; $ cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
;; ;; $ rc -J .

;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)
;; (add-hook 'objc-mode-hook 'irony-mode)

;; (defun my-irony-mode-hook ()
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))

;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; ;; Irony also needs:
;; ;; $ cd /path/to/project/root
;; ;; $ cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1

;; (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;; (setq company-backends (delete 'company-semantic company-backends))

;; (require 'company-irony-c-headers)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends '(company-irony-c-headers company-irony)))

;; ;; flycheck

;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (add-hook 'c-mode-hook 'flycheck-mode)


;; ;; integrating rtags with flycheck
;; (require 'flycheck-rtags)

;; (defun my-flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-check-syntax-automatically nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;; ;; integrating flycheck with irony (or the other way around)

;; (eval-after-load 'flycheck
;; '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; (cmake-ide-setup)

;; To have cmake-ide automatically create a compilation commands file in your project root create a .dir-locals.el containing the following:
;; ((nil . ((cmake-ide-build-dir . "<PATH_TO_PROJECT_BUILD_DIRECTORY>"))))

;;
;; Auto-generated
;;


;;
;; Rust
;;

;
;; To use race-mode, racer needs to be installed and the rust sources need to be downloaded:
;; cargo install racer
;; rustup component add rust-src

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook  #'racer-mode)
(add-hook 'rust-mode-hook  #'flycheck-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook
          '(lambda ()
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
             (cargo-minor-mode 1)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0)
 '(custom-safe-themes
   (quote
    ("203dfe036c9a450718efdd871a89ee7a6599129ac605c041d543d09831070a44" default)))
 '(frame-resize-pixelwise t)
 '(package-selected-packages
   (quote
    (cargo rust-mode ggtags xpm flycheck-irony company-irony-c-headers company-irony irony rtags nlinum js3-mode indium company-tern tern bind-key free-keys w3m undo-tree tuareg ssh py-autopep8 nyan-mode merlin markdown-mode magit jedi helm-projectile helm-gtags flycheck elpy company-c-headers better-defaults badwolf-theme autopair auctex anzu)))
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I/home/omar/projects/javascript/blocks2/server/lib")))))







