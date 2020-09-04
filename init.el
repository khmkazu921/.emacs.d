(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; window size
(setq default-frame-alist '(
 (width . 120)
 (height . 155)
 (top . 0)
 (left . 0)
))
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(if window-system (progn
(toggle-scroll-bar -1)
(setq linum-format "%3d")
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha 95 95))
))
(global-linum-mode t)
(set-face-foreground 'linum "gray60")
(load-theme 'wombat t)
(if (not window-system) (progn
 (setq linum-format "%3d ")
))

;;
;; company
;;

(require 'company)
(global-company-mode)
(setq company-transformers '(company-sort-by-backend-importance)) 
(setq company-idle-delay 0) 
(setq company-minimum-prefix-length 2) 
(setq company-selection-wrap-around t)
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next) 
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(define-key company-active-map (kbd "C-i") 'company-complete-selection) 
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "C-f") 'company-complete-selection)
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)

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
;; powerline
;; https://github.com/milkypostman/powerline
;;

(require 'powerline)

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
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(setq auto-mode-alist (cons '("\\.v\\'" . verilog-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sv\\'" . verilog-mode) auto-mode-alist))
(add-to-list 'company-keywords-alist (cons 'verilog-mode verilog-keywords))
(add-hook 'verilog-mode-hook '(lambda () (font-look-mode 1)))

(setq verilog-indent-level             4
      verilog-indent-level-module      4
      verilog-indent-level-declaration 4
      verilog-indent-level-behavioral  4
      verilog-case-indent              4
      verilog-indent-level-directive   1
      verilog-auto-endcomments         t
      verilog-auto-indent-on-newline   t
      verilog-tab-always-indent        t
      verilog-indent-begin-after-if    t
      verilog-auto-newline             nil
      verilog-auto-lineup              '(all)
;       verilog-tab-always-indent        t
;       verilog-auto-endcomments         t
;       verilog-minimum-comment-distance 40
)

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

;;
;; minimap-mode
;;

(if window-system (progn
(require 'minimap)
(minimap-mode t); 常に有効にする
(setq minimap-window-location 'right)
(setq minimap-update-delay 0.1)
(setq minimap-minimum-width 20)
(setq minimap-highlight-line t)
;; changing colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((((background dark)) (:background "gray40")) (t (:background "gray60"))) nil (quote minimap))
 '(minimap-current-line-face ((((background dark)) (:background "gray60")) (t (:background "gray80"))) nil (quote minimap)))
(set-face-attribute 'region nil :background "#666")
))
  
(require 'neotree)
(use-package neotree
;;  :init
;;  (setq-default neo-keymap-style 'concise)
  :config
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq-default neo-show-hidden-files t)
  (setq neo-theme 'ascii)
  (bind-key [f9] 'neotree-toggle)
  (bind-key "RET" 'neotree-enter-hide neotree-mode-map)
  (bind-key "a" 'neotree-hidden-file-toggle neotree-mode-map)
  (bind-key "<left>" 'neotree-select-up-node neotree-mode-map)
  (bind-key "<right>" 'neotree-change-root neotree-mode-map)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cuda-mode yatex yasnippet use-package powerline neotree minimap flycheck company))))

