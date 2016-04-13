(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages")))
;; ("melpa" . "http://melpa.milkbox.net/packages/")

(package-initialize)


(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(defalias 'yes-or-no-p 'y-or-n-p)

(defconst demo-packages
  '(anzu
    company
    duplicate-thing
    ggtags
    highlight-symbol
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    ;; function-args
    clean-aindent-mode
    comment-dwim-2
    dtrt-indent
    ws-butler
    iedit
    yasnippet
    smartparens
    projectile
    volatile-highlights
    undo-tree
    zygospore))

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
(add-to-list 'custom-theme-load-path "~/.emacs.d/custom/theme/")

(require 'setup-helm)
(require 'setup-helm-gtags)
(require 'setup-hightlight-symbol)
;; (require 'setup-ggtags)
(require 'setup-cedet)
(require 'setup-editing)

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

(require 'elpy nil t)
(elpy-enable)
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-python-command "python3")
(elpy-use-ipython)

(add-to-list 'load-path "/home/juson/.emacs.d/custom/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(global-auto-revert-mode t)


;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; auto-insert-mode
(auto-insert)
(auto-insert-mode)
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.\\(c\\|h\\|cpp\\)\\'" . "C skeleton")
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

(defun android-dir-locals ()
  (interactive)
  (insert
   "(
 (nil . (
         (company-clang-arguments . (
                                     \"-I/home/juson/Applications/Ndk/platforms/android-15/arch-arm/usr/include/\"
                                     \"-I/home/juson/Applications/Ndk/toolchains/arm-linux-androideabi-4.9/prebuilt/linux-x86_64/include/\"
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

(global-hl-line-mode)
(global-linum-mode)
(setq-default cursor-type 'bar) ;'bar 'box



;;(global-set-key
;; load-theme


;; (global-set-key (kbd "C-x 9") '(insert-file-contents "/home/juson/.emacs.d/cmain.tpl"))

;; --------------------- add system include path -----------------

;;----------------------------end config--------------------------
