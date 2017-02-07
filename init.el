(require 'package)

(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;               '("elpy" . "https://jorgenschaefer.github.io/packages/") t)

(package-initialize)
(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst demo-packages
  '(anzu
    company
    duplicate-thing
    ggtags
    ycmd
	wakatime-mode
    flycheck
    flycheck-ycmd
    flycheck-irony
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
	ecb
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    zygospore
    chinese-pyim
    chinese-pyim-greatdict
    ))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package demo-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; close tool bar
(tool-bar-mode -1)
;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(setq helm-gtags-prefix-key "\C-cg")

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom/theme")
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom/theme/solarized")

(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-hightlight-symbol)
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)
(require 'setup-flycheck)

(require 'hl-todo)
(hl-todo-mode t)

(require 'chinese-pyim)
(require 'chinese-pyim-basedict)
(chinese-pyim-basedict-enable)
(setq default-input-method "chinese-pyim")
(global-set-key (kbd "C-\\") 'toggle-input-method)


(windmove-default-keybindings)

;; function-args
;; (require 'function-args)
;; (fa-config-default)
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)

;; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(delete 'company-semantic company-backends)
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
;; (define-key c-mode-map  [(control tab)] 'company-complete)
;; (define-key c++-mode-map  [(control tab)] 'company-complete)

;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)

;; hs-minor-mode for folding source code
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; (require 'ycmd)
;; (add-hook 'c++-mode-hook 'ycmd-mode)
;; (add-hook 'c-mode-hook 'ycmd-mode)
;; (set-variable 'ycmd-server-command '("python" "/home/juson/spacemacs/ycmd/ycmd"))
;; (set-variable 'ycmd-global-config "/home/juson/spacemacs/ycmd/ycm_extra_conf_for_clang.py")
(require 'company-ycmd)
(company-ycmd-setup)
(require 'flycheck-ycmd)
(flycheck-ycmd-setup)
;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
(setq
 c-default-style "linux" ;; set style to "linux"
 )

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; newline-without-break-of-line
(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"

  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(global-set-key (kbd "C-RET") 'newline-without-break-of-line)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; compile c
(global-set-key (kbd "C-<f9>") 'compile)
;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)
(setq tab-width 4) ; or any other preferred value
(defvaralias 'c-basic-offset 'tab-width)

;; Compilation
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

;; setup GDB
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Package: yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Package: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; Package: projejctile
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

(global-reveal-mode)

(defun juson_edit-emacs-init-file ()
  "Edit Emacs Init File"
  (interactive)
  (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el"))
  )

(defun juson_reload-emacs-init-file()
  "Reload Emacs Init File"
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )

;; (global-set-key (kbd "C-x C") 'edit-emacs-init-file)
;; (global-set-key (kbd "C-x L") 'reload-emacs-init-file)


(require 'tempo-c-cpp)

(defun cpp-highlight-if-0/1 ()
  "Modify the face of text in between #if 0 ... #endif."
  (interactive)
  (setq cpp-known-face '(foreground-color . "dim gray"))
  (setq cpp-unknown-face 'default)
  (setq cpp-face-type 'dark)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list
        '((#("1" 0 1
             (fontified nil))
           nil
           (foreground-color . "dim gray")
           both nil)
          (#("0" 0 1
             (fontified nil))
           (foreground-color . "dim gray")
           nil
           both nil)))
  (cpp-highlight-buffer t))

(defun jpk/c-mode-hook ()
  (cpp-highlight-if-0/1)
  (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local)
  )

(add-hook 'c-mode-common-hook 'jpk/c-mode-hook)

(setq backup-directory-alist (quote (("." . "~/.backups"))))

;; (require 'elpy nil t)
;; (elpy-enable)
;; (setq elpy-rpc-backend "jedi")
;; (setq elpy-rpc-python-command "python3")
;; (elpy-use-ipython)

;; (add-to-list 'load-path "/home/juson/.emacs.d/custom/neotree")
;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)

;; (global-auto-revert-mode t)


;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; auto-insert-mode
(auto-insert)
(auto-insert-mode)
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.\\(c\\|cxx\\|cpp\\)\\'" . "C skeleton")
     '(
       "Short Description: "
       "/**\n"
       " *\n"
       " * @File:   "
       (file-name-nondirectory (buffer-file-name)) "\n"
       " * @Date:   "
       (format-time-string "%A, %e %B %Y") "\n"
       " * @Author: Zhaosheng Qiu <JusonQiu@gmail.com>\n"
       " *\n"
       " **/\n"
       )
     )
  )

(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.\\(h\\|hxx\\|hpp\\)\\'" . "C Header skeleton")
     '(
       "Short Description: "
       "/**\n"
       " *\n"
       " * @File:   "
       (file-name-nondirectory (buffer-file-name)) "\n"
       " * @Date:   "
       (format-time-string "%A, %e %B %Y") "\n"
       " * @Author: Zhaosheng Qiu <JusonQiu@gmail.com>\n"
       " *\n"
       " **/\n"
       "#pragma once\n"
       )
     )
  )

(defun android-dir-locals ()
  (interactive)
  (insert
   "(
 (nil . (
         (company-clang-arguments . (
                                     \"-I/home/juson/Applications/Sdk/ndk-bundle/platforms/android-15/arch-arm/usr/include/\"
                                     )
                                  )
         )
      )
 )
"
   )
  );; end android...

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; parse main function of clang
(defun parse-c-main ()
  (interactive)
  (insert
   "#include <stdio.h>\n"
   "#include <stdlib.h>\n"
   "int main(int argc, char* argv[]){\n"
   "\n    return 0;\n"
   "}\n")
  )

(defun create-dir-locals ()
  (interactive)
  (write-region
   "((nil . ((company-clang-arguments . (
   \"-I \"
   )))))" nil ".dir-locals.el")
  (switch-to-buffer (find-file-noselect ".dir-locals.el")))


(global-hl-line-mode)
(global-linum-mode)
(setq-default cursor-type 'bar) ;'bar 'box

(set-face-attribute 'default nil :height 120)

;;;;;;;;;;;;;;;;;;;;;;;;;; Markdown Mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
             :ensure t
             :commands (markdown-mode gfm-mode)
             :mode (("README\\.md\\'" . gfm-mode)
                    ("\\.md\\'" . markdown-mode)
                    ("\\.markdown\\'" . markdown-mode))
             :init (setq markdown-command "multimarkdown"))


(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(add-hook 'markdown-mode
          (lambda()
            (local-unset-key (kbd "C-space"))))


(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(require 'rtags) ;; optional, must have rtags installed
(cmake-ide-setup)

(sml/setup)
(setq sml/theme 'powerline)
(display-time-mode)

(global-wakatime-mode)
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (load-theme 'smart-mode-line-powerline t)
      (load-theme 'atom-one-dark t))
  (load-theme 'smart-mode-line-powerline t)
  (load-theme 'material t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("82b67c7e21c3b12be7b569af7c84ec0fb2d62105629a173e2479e1053cff94bd" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "870a63a25a2756074e53e5ee28f3f890332ddc21f9e87d583c5387285e882099" "38143778a2b0b81fb7c7d0e286e5b0e27cd6b2ba1c3b0aa4efbc33e6ac2ed482" "7cbae9092a2138833ec757bd7547111bc29daf10cec8013f1b51d359ba547c99" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(display-time-mode t)
 '(ecb-history-buffer-name " *ECB LAST OPENED*")
 '(ecb-source-path (quote ("~/Workspace/NATTool/stun")))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (company-irony cmake-ide zygospore yasnippet ws-butler wakatime-mode volatile-highlights use-package undo-tree smartparens smart-mode-line-powerline-theme simple-call-tree pcache molokai-theme moe-theme markdown-mode iedit idea-darkula-theme hl-todo highlight-symbol helm-swoop helm-projectile helm-gtags ggtags function-args flycheck-ycmd flycheck-irony ecb duplicate-thing dtrt-indent company-ycmd company-c-headers comment-dwim-2 color-theme-sanityinc-tomorrow color-theme clean-aindent-mode chinese-pyim-wbdict chinese-pyim-greatdict atom-one-dark-theme anzu)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(wakatime-cli-path "/home/juson/.pyenv/versions/2.7.12/bin/wakatime"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
