;;; package --- Summary:
;;; Commentary:
;;; seems like the standard

;;; Code:

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; about appearance and general settings

(tool-bar-mode -1)
;; ;; already disable by emacs-mac compile
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq mac-use-title-bar t)
(setq-default abbrev-mode t)
(setq-default cursor-type 'bar)
(setq scroll-conservatively 50)

(if (daemonp)
    nil
  (setq confirm-kill-emacs 'yes-or-no-p))

(fset 'yes-or-no-p 'y-or-n-p)

;; (desktop-save-mode 1)
;; (push "/Users/gfgkmn/.emacs.d/save-sessions/" desktop-path)

(global-hl-line-mode 1)
(global-auto-revert-mode t)

(windmove-default-keybindings)

;; the different between setq and setq-default only exists when the variable to
;; deal is buffer-local variable, so you should use setq-default

;; tocreate
(setq ns-pop-up-frames nil)

;; ;; disable by doom theme or dashboard
;; (setq inhibit-startup-screen t)
;; (setq-default left-margin-width 3 right-margin-width 3)
;; (set-fringe-mode '(6 . 6))
;; (set-face-attribute 'fringe nil
;;       :foreground (face-foreground 'default)
;;       :background (face-background 'default))


;; set gui's color theme
(add-to-list 'custom-theme-load-path (concat user-emacs-directory
                                             (convert-standard-filename "themes")))

;; ;; should disable in term-mode.
;; ;; use prog-mode-hook instead temperaly
;; ;; tocreate https://stackoverflow.com/questions/6837511/automatically-disable-a-global-minor-mode-for-a-specific-major-mode
;; ;; tocreate https://stackoverflow.com/questions/29169210/how-to-disable-global-minor-mode-in-a-specified-major-mode
(setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)
;; (add-hook 'prog-mode-hook (unless (eq major-mode 'term-mode) 'display-line-numbers-mode))
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'Fundamental-mode-hook 'display-line-numbers-mode)
(add-hook 'special-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)

;; where to backup files.
(setq backup-directory-alist `(("" . ,(concat user-emacs-directory
                                              (convert-standard-filename "emacs-backup")))))

(setq backup-by-copying t      ; don't clobber symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory
                       (convert-standard-filename "emacs-autosaves")) t)))

(add-to-list 'load-path
             (concat user-emacs-directory
                     (convert-standard-filename "self_elisp/")))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
;; (electric-indent-mode nil)

;; load package for you use
(when (>= emacs-major-version 24)
  (package-initialize)
  (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                           ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))

(require 'cl)

;; Add Packages
(defvar my/packages '(
                      ;; for lisp programming
                      dash
                      s
                      f
                      ;; Self plugins
                      dashboard
                      all-the-icons
                      cnfonts
                      doom-themes
                      company
                      vimrc-mode
                      ein
                      hierarchy
                      async
                      ;; about ipython notebook
                      ob-async
                      ;; acculerate org-mode ipython execute
                      yasnippet
                      yasnippet-snippets
                      general
                      which-key
                      use-package
                      evil-leader
                      evil-org
                      evil-surround
                      evil-collection
                      undo-fu
                      popwin
                      dash-at-point
                      ;; package
                      ;; --- Better Editor ---
                      hungry-delete
                      swiper
                      counsel
                      ivy
                      imenu-list
                      prescient
                      ivy-prescient
                      company-prescient
                      smartparens
                      iedit
                      expand-region
                      winum
                      magit
                      ;; eaf dependency
                      ctable
                      epc
                      deferred
                      ;; roam research
                      org-roam
                      ;; ;; --- Major Mode ---
                      ;; markdown
                      markdown-mode
                      ;; swift-mode
                      swift-mode
                      ;; js2-mode
                      web-mode
                      emmet-mode
                      ;; ;; --- Minor Mode ---
                      ;; nodejs-repl
                      exec-path-from-shell
                      ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
  (loop for pkg in my/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (my/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg my/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PYTHONPATH")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:ask-p nil))


(use-package doom-themes
        :config
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
              doom-themes-enable-italic t) ; if nil, italics is universally disabled
        (load-theme 'doom-one t)

        ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
        (doom-themes-neotree-config)

        ;; ;; or for treemacs users
        ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
        ;; (doom-themes-treemacs-config)
        
        ;; Corrects (and improves) org-mode's native fontification.
        (doom-themes-org-config))

;; highlight the brackets where the cursor locate
(show-paren-mode)
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))


;; dashboasd's config
(dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner "/Users/gfgkmn/Configs/Emacs/emacs_red_small.png")
;; Value can be
;; 'official:  which displays the official emacs logo
;; 'logo:  which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" which displays whatever image you would prefer
;; ;; Content is not centered by default. To center, set
;; (setq dashboard-center-content t)
;; To disable shortcut "jump" indicators for each section, set
;; (setq dashboard-show-shortcuts nil)
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        ;; (projects . 5)
                        (agenda . 5)))
                        ;; (registers . 5)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))


;; which-key config
(which-key-mode 1)

;; evil config
(setq evil-want-keybinding nil)
(setq evil-want-fine-undo t)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))

(global-evil-leader-mode)
(evil-mode t)

(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

(use-package avy
  :ensure t)

(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1)
  (setq evil-snipe-scope 'whole-line)
  (setq evil-snipe-spillover-scope 'whole-visible))


(use-package evil-indent-plus
  :ensure t
  :config
  (evil-indent-plus-default-bindings))

(use-package evil-text-object-python
  :ensure t
  :config
  (add-hook 'python-mode-hook (evil-text-object-python-add-bindings)))

(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-focus-after-activation t))
(use-package exato :ensure t)

(use-package recentf-ext
  :ensure t)

;; evil-surround config
(global-evil-surround-mode 1)
(define-key evil-normal-state-map "u" 'undo-fu-only-undo)
(define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)

;; markdown-preview mode
(use-package livedown
  :load-path "~/.emacs.d/site-lisp/emacs-livedown"
  :custom
  (livedown-autostart nil) ; automatically open preview when opening markdown files
  (livedown-open t)        ; automatically open the browser window
  (livedown-port 1337)     ; port for livedown server
  (livedown-browser nil))

;; org-mode config
;; python
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (R . t) (shell . t)))
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-startup-folded nil)
(setq org-agenda-files '("~/Documents/org-note/"))
(global-set-key (kbd "C-c a") 'org-agenda)

;; define org-mode vim-style keymaping.
(add-to-list 'load-path "~/.emacs.d/plugins/evil-org-mode")

;; org-roam config
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "/Users/gfgkmn/Documents/roamwiki/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;; (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(setq org-roam-server-host "127.0.0.1"
      org-roam-server-port 9090
      org-roam-server-export-inline-images t
      org-roam-server-authenticate nil
      org-roam-server-network-label-truncate t
      org-roam-server-network-label-truncate-length 60
      org-roam-server-network-label-wrap-length 20)
;; (org-roam-server-mode)

;; ;; general config
;; bind a key globally in normal state; keymaps must be quoted
(setq general-default-keymaps 'evil-normal-state-map)
;; defind key-binding in evil normal mode.

;; define gcc comment and uncomment line
(defun comment-or-uncomment-regionline()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(setq leaderg "g")
(general-define-key :prefix leaderg
                    "c" 'comment-or-uncomment-regionline)

;; occur-mode config
(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

;; (require 'counsel)

(evil-leader/set-key
  "nt" 'neotree-toggle
  "be" 'ivy-switch-buffer
  "se" 'counsel-search
  "re" 'eval-last-sexp
  "ca" 'delete-other-windows
  ;; "dd" 'dash-at-point-with-docset)
  "dd" 'dash-at-point
  "ex" 'dired
  ;; "tg" 'counsel-imenu
  "tg" 'imenu-list-smart-toggle
  "si" 'customize-group
  "oc" 'occur-dwim
  "ll" 'ivy-resume
  "hh" 'counsel-apropos
  "rf" 'evil-show-jumps
  "af" 'avy-goto-char-timer)


;; (require 'popwin)
(popwin-mode t)

(defun open_emacs ()
  (interactive)
  (find-file user-init-file))

(defun eval_emacs ()
  (interactive)
  (load-file user-init-file))

(evil-leader/set-key
  "er" 'eval_emacs)

(evil-leader/set-key
  "oi" 'open_emacs)

;; use recent file
(recentf-mode 1)
(setq recentf-max-menu-items 100)

;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; tocreate: what's this
(global-visual-line-mode t)
(set-display-table-slot standard-display-table 'wrap ?\ )


;; ;; yasnippet's config
(yas-global-mode 1)

;; smartparens-mode config
(smartparens-global-mode)
;; disable ' pair in elisp mode 
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "`" nil :actions nil)

;; company-mode and yasnippet config
(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-idle-delay 0)
  (setq company-global-modes '(not recentf-dialog-mode))
  (setq company-backends
        '((company-files
           company-yasnippet
           company-keywords
           company-capf)
          (company-abbrev company-dabbrev))))
(add-hook 'emacs-lisp-mode-hook (lambda()
                                  (add-to-list (make-local-variable 'company-backends)
                                               'company-files)))

(use-package ycmd
  :ensure t
  :init
  (set-variable 'ycmd-server-command '("python" "/Users/gfgkmn/.emacs.d/ycmd_completer/ycmd/ycmd"))
  :config
  (add-hook 'after-init-hook #'global-ycmd-mode))

(evil-leader/set-key
  "gt" 'ycmd-goto-type
  "gd" 'ycmd-goto-declaration
  "gp" 'ycmd-goto-imprecise
  "gf" 'ycmd-goto-definition
  "gi" 'ycmd-goto-include
  "gr" 'ycmd-goto-reference
  "gm" 'ycmd-goto-implementation)

;; (defun ycmd-setup-completion-at-point-function ()
;;   "Setup `completion-at-point-functions' for `ycmd-mode'."
;;   (add-hook 'completion-at-point-functions
;;             #'ycmd-complete-at-point nil :local))

;; (add-hook 'ycmd-mode-hook #'ycmd-setup-completion-at-point-function)

(use-package company-ycmd
  :ensure t
  :config
  (company-ycmd-setup))


(setq company-global-modes '(not recentf-dialog-mode))
(add-hook 'after-init-hook 'global-company-mode)

(defun company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
      (call-interactively #'company-complete-common))))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-y") #'company-complete-selection))


(define-key evil-insert-state-map (kbd "C-j") #'yas-expand-from-trigger-key)
(define-key evil-insert-state-map (kbd "C-l") #'yas-describe-tables)

(global-set-key (kbd "C-=") #'er/expand-region)
(global-set-key (kbd "C-;") #'iedit-mode)

;; tocreate redundent with company-yasnippet-or-completion ?
(advice-add 'company-complete-common :before (lambda () (setq my-company-point (point))))
(advice-add 'company-complete-common :after (lambda () (when (eq my-company-point (point))
                                                         (yas-expand))))

;; ;; bind major mode
;; (add-to-list 'auto-mode-alist  '("\\.md\\'" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))
(setq x-select-enable-clipboard t)

;; cnfonts's config
(cnfonts-enable)

;; ein's config ?
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

;; common lisp config
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;; dash config
(add-hook 'elisp-mode-hook
          (lambda () (setq dash-at-point-docset "elisp")))
(add-hook 'lisp-mode-hook
          (lambda () (setq dash-at-point-docset "lisp")))


(define-abbrev-table 'global-abbrev-table '(
                                            ;; ("gs" "git remote -vv && echo $'\\n\\tCurrent repository status:' &&  git status")
                                            ("tc" "tocreate")
                                            ("oc" "ssh:yuhe@192.168.53.10")
                                            ("ocsu" "ssh:yuhe@192.168.53.10|sudo::")
                                            ("dv" "ssh:yuhe@192.168.53.6")
                                            ("ia" "ssh:yuhe@192.168.53.5")
                                            ))

;; Ivy counsel and swiper config
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq counsel-search-engine 'google)
;; maybe should indicate it only available when search on web
;; (ivy-partial-or-done)

;; (setq search-default-mode #'char-fold-to-regexp)
(define-key evil-normal-state-map "\C-s" 'swiper-isearch)
(define-key evil-normal-state-map "\M-n" 'swiper-isearch-thing-at-point)
;; tocreate when m-n at swiper, then error
(define-key evil-normal-state-map (kbd "M-x") 'counsel-M-x)
(define-key evil-normal-state-map (kbd "<f2> i") 'counsel-info-lookup-symbol)
(define-key evil-normal-state-map (kbd "<f2> u") 'counsel-unicode-char)
(define-key evil-normal-state-map (kbd "C-c g") 'counsel-git)
(define-key evil-normal-state-map (kbd "C-c j") 'counsel-git-grep)

(setq counsel-locate-db-path "~/.config/locatedb")

(defun counsel-locate-cmd-mdfind-all (input)
  "Return a `mdfind' shell command based on INPUT."
  (counsel-require-program "mdfind")
  (format "mdfind %s" (shell-quote-argument input)))

(defun counsel-locate-mdfind-function (input)
  "Call a \"locate\" style shell command with INPUT."
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
      (funcall #'counsel-locate-cmd-mdfind-all input))
     '("" "working..."))))


(defun counsel-locate-mdfind (&optional initial-input)
  "Call a \"locate\" style shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "Locate: " 'counsel-locate-mdfind-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-locate-history
            :action (lambda (file)
                      (when file
                        (with-ivy-window
                          (find-file
                           (concat (file-remote-p default-directory) file)))))
            :caller 'counsel-locate-mdfind))

(evil-leader/set-key
  "vo" 'counsel-find-file
  "vf" 'counsel-fzf
  "vg" 'counsel-locate-mdfind
  "vl" 'counsel-locate
  "vv" 'counsel-ag
  "vs" 'magit
  "hs" 'git-gutter:stage-hunk
  "hu" 'git-gutter:revert-hunk)

(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; disable doom theme's visual warning
(setq byte-compile-warnings '(cl-functions))

;; hippie-expand function config
(setq hippie-expand-try-function-list '(try-expand-debbrev
                                        try-expand-debbrev-all-buffers
                                        try-expand-debbrev-from-kill
                                        try-complete-file-name-partially
                                        try-complete-file-name
                                        try-expand-all-abbrevs
                                        try-expand-list
                                        try-expand-line
                                        try-complete-lisp-symbol-partially
					                    try-complete-lisp-symbol))
;; control+n and control + y conflict with emacs's control +y, tocreate
(global-set-key (kbd "C-n") 'hippie-expand)

(defun my-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))

(define-key evil-insert-state-map (kbd "C-x C-l") 'my-expand-lines)

;; * + 创建目录
;; * g 刷新目录
;; * C 拷贝
;; * D 删除
;; * R 重命名
;; * d 标记删除
;; * u 取消标记
;; * x 执行所有的标记

;; dired mode config
;; always copy folder recursive
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
;; make every folder in dired mode in only one buffer
(put 'dired-find-alternate-file 'disabled nil)

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))
;; (require 'dired-x)
(setq-default dired-dwin-target 1)

(use-package dired-git-info
  :ensure t
  :config
  (setq dgi-auto-hide-details-p nil)
  (add-hook 'dired-after-readin-hook 'dired-git-info-auto-enable))


;; web-mode config
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       '(("\\.html\\'" . web-mode))
       auto-mode-alist))

(setq leader-next "]")
(setq leader-previous "[")

;; flycheck config
(use-package flycheck
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode))
 
(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-disabled-checkers '(python-pylint)))

(general-define-key :prefix leader-previous
                    "q" 'flycheck-previous-error
                    "c" 'git-gutter:previous-hunk)
(general-define-key :prefix leader-next
                    "q" 'flycheck-next-error
                    "c" 'git-gutter:next-hunk)
(use-package flycheck-swift :ensure t)

(evil-leader/set-key
  "ql" 'flycheck-list-errors)


;; get current file name
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(setq leaderc ",")
(general-define-key :prefix leaderc
                    "cy" 'my-put-file-name-on-clipboard)


;; ;; eaf config, still a litter slow
;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
;;   :init
;;   (use-package epc :defer t :ensure t)
;;   (use-package ctable :defer t :ensure t)
;;   (use-package deferred :defer t :ensure t)
;;   (use-package s :defer t :ensure t)
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   :config
;;   (eaf-setq eaf-browser-enable-adblocker "true")
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki
