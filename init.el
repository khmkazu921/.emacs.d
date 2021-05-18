;; (setq debug-on-error t)

(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;;
;; window
;;
(set-face-attribute 'default nil :font "UbuntuMono-12" )
(setq default-frame-alist '((top . 0) (left . 2500) (width . 150) (height . 156)))
(setq-default indent-tabs-mode nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode t)
(if window-system (progn
                    (toggle-scroll-bar -1)
                    (setq linum-format "%3d")
                    (set-frame-parameter (selected-frame) 'alpha '(100 100))
                    (add-to-list 'default-frame-alist '(alpha 100 100))))

(set-face-foreground 'linum "gray60")
(load-theme 'wombat t)
(if (not window-system) (progn
                          (setq linum-format "%3d ")))

(delete-selection-mode 1)

;;
;; auto-mode-alist
;;

(setq auto-mode-alist
      (append '(("\\.tex\\'"   . yatex-mode)
                ("\\.sty\\'"   . yatex-mode)
                ("\\.v\\'"     . verilog-mode)
                ("\\.sv\\'"    . verilog-mode)
                ("\\.c\\'"     . c-mode)
                ("\\.h\\'"     . c-mode)
                ("\\.cpp\\'"   . c++-mode)
                ("\\.cc\\'"    . c++-mode)
                ("\\.cxx\\'"   . c++-mode)
                ("\\.hh\\'"    . c++-mode)
                ("\\.py\\'"    . python-mode)
                ("Makefile\\'" . makefile-mode)
                (".bashrc\\'" . sh-mode)
                ("\\.sh\\'"   . sh-mode)
                ("\\.el\\'"  .  lisp-mode))))

;;
;; backup
;;

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;;
;; company
;;

(require 'company)
(global-company-mode)
(setq company-transformers '(company-sort-by-backend-importance)) 
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1) 
(setq company-selection-wrap-around t)
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(define-key company-active-map [tab] 'company-complete-selection)

;;
;; org mode
;;

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; (setq org-capture-templates
;;       '(;;("c" "* NOTES - %t \n %a \n %i \n")
;;         ("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
;;          "* TODO %?\n  %i\n  %a")
;;         ("j" "Journal" entry (file+datetree "~/org/journal.org")
;;          "* %?\nEntered on %U\n  %i\n  %a")))


;;
;; yatex
;;

;(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-height 15)
(setq doom-modeline-bar-width 2)


;;
;; use-package
;;

(require 'use-package)

;;
;; lsp-mode
;;

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode t)
  :config
  ;; Company Flx adds fuzzy matching to company, powered by the sophisticated
  ;; sorting heuristics  in =flx=
  (use-package company-flx
    :ensure t
    :after company
    :init (company-flx-mode t))
  (use-package company-quickhelp
    :after company
    :ensure t
    ;; :init (company-quickhelp-mode t)
    :hook (prog-mode . (lambda ()
                         (when (window-system)
                           (company-quickhelp-local-mode))))
    :config
    (setq company-quickhelp-delay 0.0
          company-quickhelp-max-lines nil)))

(add-hook 'verilog-mode-hook #'lsp)

(use-package lsp-mode
  :defer t
  :ensure t
  :commands lsp
  :config
  (setq lsp-log-io nil
        lsp-auto-configure t
        lsp-auto-guess-root t
        lsp-enable-completion-at-point t
        lsp-enable-xref t
        lsp-prefer-flymake nil
        lsp-use-native-json t
        lsp-enable-indentation t
        lsp-response-timeout 10
        lsp-restart 'auto-restart
        lsp-keep-workspace-alive t
        lsp-eldoc-render-all nil
        lsp-enable-snippet nil
        lsp-enable-folding t)
                                        ; lsp-ui gives us the blue documentation boxes and the sidebar info
  
  (use-package lsp-ui
    :defer t
    :ensure t
    :after lsp
    :commands lsp-ui-mode
    :config               
    (setq lsp-ui-sideline-ignore-duplicate t
          lsp-ui-sideline-delay 0.5
          lsp-ui-sideline-show-symbol t
          lsp-ui-sideline-show-hover t
          lsp-ui-sideline-show-diagnostics t
          lsp-ui-sideline-show-code-actions t
          lsp-ui-peek-always-show t
          lsp-ui-doc-use-childframe t)
    :bind
    (:map lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references] . lsp-ui-peek-find-references))
    :hook
    (
     (lsp-mode . lsp-ui-mode)
     (lsp-managed-mode . (lambda ()
                           (setq-local company-backends '(company-capf))
                           )
                       )
     ;;      (lsp-after-open . (lambda ()
     ;;                          (lsp-ui-flycheck-enable t)
     ;;                          (lsp-ui-sideline-enable t)
     ;;                          (lsp-ui-imenu-enable t)
     ;;                          (lsp-lens-mode t)
     ;;                          (lsp-ui-peek-enable t)
     ;;                          (lsp-ui-doc-enable t)))
     )
    )
  )

;;
;; verilog mode
;;

(use-package verilog-mode
  :defer t
  :config
  (require 'lsp)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
   :major-modes '(verilog-mode)
   :priority -1
   ))
  :hook (verilog-mode . (lambda()
      (lsp)
      (flycheck-mode t)
      (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog")))))

(setq verilog-indent-level             4
      verilog-indent-level-module      4
      verilog-indent-level-declaration 4
      verilog-indent-level-behavioral  4
      verilog-case-indent              4
      verilog-indent-level-directive   4
      verilog-auto-endcomments         t
      verilog-auto-indent-on-newline   t
      verilog-tab-always-indent        t
      verilog-indent-begin-after-if    t
      verilog-auto-newline             nil
      verilog-auto-lineup              '(all)
      verilog-align-ifelse             nil
;       verilog-tab-always-indent        t
;       verilog-auto-endcomments         t
;       verilog-minimum-comment-distance 40
      )

;; ;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; ;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;;
;; flycheck / verilator
;;

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode t))

;;
;; c-mode
;;

(autoload 'c-mode "cc-mode" nil t) 
(setq-default c-default-style "linux"
              c-basic-offset 4)
(put 'downcase-region 'disabled nil)
(eval-after-load 'c-modea (global-set-key (kbd "C-c p") "printf(\"%d\",);") )
(eval-after-load 'c-modea (global-set-key (kbd "C-c f") "for(int i=0; i<; i++) { }") )
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

;;
;; minimap-mode
;;

(if window-system
    (progn
      (require 'minimap)
      (minimap-mode t); 常に有効にする
      (setq minimap-window-location 'right)
      (setq minimap-update-delay 0)
      (setq minimap-minimum-width 20)
      (setq minimap-highlight-line t)
      (setq minimap-width-fraction 0.1
            minimap-dedicated-window t)
      ;; changing colors
      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(minimap-active-region-background ((((background dark)) (:background "gray40")) (t (:background "gray60"))) nil (quote minimap))
       '(minimap-current-line-face ((((background dark)) (:background "gray60")) (t (:background "gray80"))) nil (quote minimap)))
      (set-face-attribute 'region nil :background "#666")
      )
  )

;;
;; neo-tree
;;

(require 'neotree)
(defun neotree-startup ()
  (interactive)
  (neotree-show)
  (call-interactively 'other-window))

(if (daemonp)
    (add-hook 'server-switch-hook #'neotree-startup)
  (add-hook 'after-init-hook #'neotree-startup)
  )

(use-package neotree
;;  :init
;;  (setq-default neo-keymap-style 'concise)
  :config
  (setq neo-smart-open t)
  (setq neo-create-file-auto-open t)
  (setq-default neo-show-hidden-files nil)
  (setq neo-theme 'nerd)
  (bind-key [f10] 'neotree-toggle)
  (bind-key "RET" 'neotree-enter-hide neotree-mode-map)
  (bind-key "a" 'neotree-hidden-file-toggle neotree-mode-map)
  (bind-key "<left>" 'neotree-select-up-node neotree-mode-map)
  (bind-key "<right>" 'neotree-change-root neotree-mode-map)
  )


;;
;; grep
;;
(global-set-key (kbd "C-x C-g") 'grep)
(setq grep-use-null-device nil)
(custom-set-variables
 '(grep-command "grep --color -rin -e ")
 '(grep-find-command
   '("find . -type f -exec grep --color -nH --null -e  \\{\\} +" . 49))
 '(inhibit-startup-buffer-menu t))

;;
;; highlight symbol
;;

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.1)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
(global-set-key (kbd "M-s M-r") 'highlight-symbol-query-replace)
(bind-key* [mouse-3] '(lambda (c) (interactive "e") (mouse-set-point c) (highlight-symbol-at-point)))
(bind-key* [mouse-8] 'mode-line-previous-buffer)
(bind-key* [mouse-9] 'mode-line-next-buffer)
(bind-key* "<mode-line> <mouse-1>" '(lambda (c) (interactive "e") (mouse-buffer-menu c)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(highlight-symbol all-the-icons which-key verilog-mode use-package tramp powerline neotree minimap lsp-ui lsp-treemacs imenu-list flycheck company-quickhelp company-flx)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((((background dark)) (:background "gray40")) (t (:background "gray60"))) nil 'minimap)
 '(minimap-current-line-face ((((background dark)) (:background "gray60")) (t (:background "gray80"))) nil 'minimap))
