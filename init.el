(package-initialize)


;; No tabs

(setq-default indent-tabs-mode nil)

;;
;; tool-bar
;;

(menu-bar-mode -1)


;;
;; colum-num
;;

(global-linum-mode t)
(setq linum-format "%3d ")
(set-face-foreground 'linum "gray60")

;;
;; company
;;

(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 3) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
(define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fで候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-mode t)
 '(minimap-recreate-window t)
 '(minimap-window-location (quote right))
 '(package-selected-packages (quote (minimap powerline yatex yasnippet company)))
 '(verilog-auto-wire-type nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "green"))))
 '(font-lock-doc-face ((t (:foreground "color-99"))))
 '(font-lock-function-name-face ((t (:foreground "color-33"))))
 '(font-lock-negation-char-face ((t (:foreground "brightwhite"))))
 '(font-lock-preprocessor-face ((t (:foreground "brightmagenta"))))
 '(font-lock-string-face ((t (:foreground "color-105"))))
 '(font-lock-type-face ((t (:foreground "color-130"))))
 '(font-lock-variable-name-face ((t (:foreground "brightblue"))))
 '(minibuffer-prompt ((t (:foreground "color-39")))))

;;
;; yatex
;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.sty$" . yatex-mode))))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)

;;
;; package
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(fset 'package-desc-vers 'package--ac-desc-version)

;;
;; powerline
;; https://github.com/milkypostman/powerline
;;

;(package-install 'powerline) ;自動インストール

(require 'powerline)
;(setq powerline-default-separator 'curve)

(setq-default mode-line-format
      '("%e"
	(:eval
	 (let* ((active (powerline-selected-window-active))
		(mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
		(mode-line (if active 'mode-line 'mode-line-inactive))
		(face0 (if active 'powerline-active0 'powerline-inactive0))
		(face1 (if active 'powerline-active1 'powerline-inactive1))
		(face2 (if active 'powerline-active2 'powerline-inactive2))
		(separator-left (intern (format "powerline-%s-%s"
						(powerline-current-separator)
						(car powerline-default-separator-dir))))
		(separator-right (intern (format "powerline-%s-%s"
						 (powerline-current-separator)
						 (cdr powerline-default-separator-dir))))
		(lhs (list (powerline-raw "%*" face0 'l)
			   (when powerline-display-buffer-size
			     (powerline-buffer-size face0 'l))
			   (when powerline-display-mule-info
			     (powerline-raw mode-line-mule-info face0 'l))
			   (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
			   (when (and (boundp 'which-func-mode) which-func-mode)
			     (powerline-raw which-func-format face0 'l))
			   (powerline-raw " " face0)
;			   (funcall separator-left face0 face1)
;			   (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
;			     (powerline-raw erc-modified-channels-object face1 'l))
			   (powerline-major-mode face1 'l)
			   (powerline-process face1)
			   (powerline-minor-modes face1 'l)
			   (powerline-narrow face1 'l)
			   (powerline-raw " " face1)
;			   (funcall separator-left face1 face2)
			   (powerline-vc face2 'r)))
		(rhs (list (powerline-raw global-mode-string face2 'r)
;			   (funcall separator-left face2 face1)
			   (powerline-raw "%3l" face1 'l)
			   (powerline-raw ":" face1 'l)
			   (powerline-raw "%3c" face1 'r)
;			   (funcall separator-left face1 face0)
			   (powerline-raw " " face0)
			   (powerline-raw "%2p" face0 'r)
			   (when powerline-display-hud
			     (powerline-hud face0 face2))
			   (powerline-fill face0 0)
			   )))
	   (concat (powerline-render lhs)
		   (powerline-fill face2 (powerline-width rhs))
		   (powerline-render rhs))))))

(make-face 'powerline-active1)
(set-face-attribute 'powerline-active0 nil
                    :foreground "#fffacd"
		    :background "#7777AA")

(set-face-attribute 'powerline-active1 nil
                    :foreground "#fffacd"
                    :background "#6666CC")

(make-face 'powerline-active2)
(set-face-attribute 'powerline-active2 nil
                    :foreground "#fffacd"
                    :background "#000077")

(make-face 'powerline-inactive0)
(set-face-attribute 'powerline-inactive0 nil
                    :foreground "#DDDDDD"
                    :background "#000022")

(make-face 'powerline-inactive1)
(set-face-attribute 'powerline-inactive1 nil
                    :foreground "#DDDDDD"
                    :background "#000044")

(make-face 'powerline-inactive2)
(set-face-attribute 'powerline-inactive2 nil
                    :foreground "#DDDDDD"
                    :background "#000066")

;;
;; verilog mode
;;

(require 'verilog-mode)
;;(defun prepend-path ( my-path )
;;(setq load-path (cons (expand-file-name my-path) load-path)))

;(defun append-path ( my-path )
;(setq load-path (append load-path (list (expand-file-name my-path)))))

;;Lood first in the directory ~/elisp for elisp files
;(prepend-path "~/.emacs.d/elisp")

;;Load verilog-mode only when needed
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )

;; Any files that end in .v should be in verilog mode

(setq auto-mode-alist (cons '("\\.v\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sv\\'" . verilog-mode) auto-mode-alist))

;; company-mode doesn't support verilog-mode yet. But it can be fixed by add verilog keywords to solve this problem
(add-to-list 'company-keywords-alist (cons 'verilog-mode verilog-keywords))

;; Any files in verilog mode shuold have their keywords colorized
(add-hook 'verilog-mode-hook '(lambda () (font-look-mode 1)))

(setq verilog-indent-level             4
      verilog-indent-level-module      4
      verilog-indent-level-declaration 4
      verilog-indent-level-behavioral  4
      verilog-auto-endcomments         t
      verilog-tab-always-indent        t
      verilog-auto-newline             nil)

; (setq verilog-indent-level             3
;       verilog-indent-level-module      3
;       verilog-indent-level-declaration 3
;       verilog-indent-level-behavioral  3
;       verilog-indent-level-directive   1
;       verilog-case-indent              2
;       verilog-auto-newline             t
;       verilog-auto-indent-on-newline   t
;       verilog-tab-always-indent        t
;       verilog-auto-endcomments         t
;       verilog-minimum-comment-distance 40
;       verilog-indent-begin-after-if    t
;       verilog-auto-lineup              '(all))

;;
;; use-package
;;

(require 'use-package)

;;
;; flycheck / verilator
;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-find-checker-executable 'verilog-verilator))

(defun flycheck-verilog-get-files()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "verilog-library-files:( *\"\\([^)]+\\)\" *)" nil t)
        (split-string (match-string-no-properties 1) "\" *\"") (list))))

;;
;; lisp-mode
;;

(add-to-list 'auto-mode-alist '("\\.el$" . lisp-mode))
(autoload 'lisp-mode "lisp" nil t) 

;;
;; c-mode
;;

(add-to-list 'auto-mode-alist '("\\.c$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
(autoload 'c-mode "cc-mode" nil t) 
(setq-default c-default-style "linux"
              c-basic-offset 4)
(put 'downcase-region 'disabled nil)

;;
;; makefile-mode
;;

(add-to-list 'auto-mode-alist '("Makefile$" . makefile-mode))

;;
;; sh-mode
;;

(add-to-list 'auto-mode-alist '(".bashrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.sh$" . sh-mode))
