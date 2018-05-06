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


;; Don't create backup files in working directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

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

(menu-bar-mode -1) 
(toggle-scroll-bar -1) 
(tool-bar-mode -1)
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
;; (global-set-key (kbd "C-S-x") 'hippie-expand)

;; Backward line kill
(defun backward-kill-line (arg)
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-j") 'backward-kill-line)

(global-set-key (kbd "M-j") 'ace-jump-mode)
(global-set-key (kbd "M-m") 'iy-go-to-char)
(global-set-key (kbd "M-q") 'er/expand-region)
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
 '(linum ((t (:background "#000000" :foreground "#666462" :height 100)))))


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

;; Helm in new frame instead of buffer
;; 
;; (setq helm-display-function 'helm-display-buffer-in-own-frame
;;       helm-display-buffer-reuse-frame t
;;       helm-use-undecorated-frame-option t)


;;
;; Zoomies
;; 

(defvar original-font-size nil)

(defun adjust-font-size (delta)
  (let* ((old-size (face-attribute 'default :height))
         (new-size (max (max delta (- delta)) (min 200 (+ delta old-size)))))
    (setq original-font-size (or original-font-size old-size))
    (set-face-attribute 'default nil :height new-size)
    (message "Font size set to %d (was %d)" (face-attribute 'default :height) old-size)))

(defun zoom-in ()
  (interactive)
  (adjust-font-size +5))

(defun zoom-out ()
  (interactive)
  (adjust-font-size -5))

(defun zoom-reset ()
  (interactive)
  (when original-font-size
    (set-face-attribute 'default nil :height original-font-size)))

;; Zoom settings
(global-set-key [\C-mouse-4] 'zoom-in)
(global-set-key [\C-mouse-5] 'zoom-out)
(global-set-key [\C-kp-add] 'zoom-in)
(global-set-key [\C-kp-subtract] 'zoom-out)
(global-set-key (kbd "<C-kp-0>") 'zoom-reset)
(global-set-key (kbd "<C-x +>") 'zoom-in)
(global-set-key (kbd "<C-x ->") 'zoom-in)
(global-set-key (kbd "<C-x 0>") 'zoom-reset)



;; Copy file name to clipboard

;; (defun copy-file-name-to-clipboard ()
;;   "Copy the current buffer file name to the clipboard."
;;   (interactive)
;;   (let ((filename (if (equal major-mode 'dired-mode)
;;                       default-directory
;;                     (buffer-file-name))))
;;     (when filename
;;       (kill-new filename)
;;       (message "Copied buffer file name '%s' to the clipboard." filename))))

;;
;; Python
;;

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
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))




;; (defun my-tern-mode-config ()
;;   "For use in 'tern-mode-hook'."
;;   (local-set-key (kbd "C-c C-t") 'tern-get-type)
;;   (local-unset-key (kbd "C-c C-c"))
;;   ;; Todo
;;   )


;; ;; tern
;; (add-hook 'js-mode-hook (lambda () (tern-mode t)))
;; (add-hook 'tern-mode-hook 'my-tern-mode-config)

;; (setenv "PATH" (concat (getenv "PATH") ":/home/omar/projects/tern/bin"))
;;     (setq exec-path (append exec-path '("/home/omar/projects/tern/bin")))




;; company-tern
;; (add-to-list 'company-backends 'company-tern) 

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

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)


(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)



(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(defun my-irony-mode-hook ()		
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async)
  (define-key irony-mode-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key irony-mode-map (kbd "M-,") 'rtags-location-stack-back)
  (define-key irony-mode-map (kbd "C-c C-r" 'rtags-find-references-at-point)))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'rtags-start-process-unless-running)



;; C#

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'omnisharp-mode-hook 'flycheck-mode)
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(defun my-csharp-mode-setup ()
  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)
  (local-set-key (kbd "C-c C-c") 'recompile)
  (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
  (define-key omnisharp-mode-map (kbd "C-M-.") 'omnisharp-go-to-definition-other-window))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)



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

;; Go
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
(setenv "GOPATH" "/home/omar/projects/go/")
(add-to-list 'exec-path "/home/omar/projects/go/bin/")

(require 'company-go)

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-echo-delay 0 t)
 '(company-idle-delay 0)
 '(company-minimum-prefix-length 1)
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







