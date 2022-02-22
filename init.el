;; (setq debug-on-error t)

(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;;
;; window
;;
;;"Monospace-12:weight=bold:slant=italic"
(set-face-attribute 'default nil :font "RobotoMono-10.5:weight=Medium")
(setq default-frame-alist '((top . 0) (left . 2500) (width . 150) (height . 156)))
(setq-default indent-tabs-mode nil)
(menu-bar-mode t)
(tool-bar-mode -1)
(scroll-bar-mode t)
(global-linum-mode t)
;(pixel-scroll-mode t)

(if (not window-system)
    (progn
      ;; activate mouse-based scrolling
      (xterm-mouse-mode 1)
      (defun mouse-scroll-down() (interactive)(scroll-down 10))
      (defun mouse-scroll-up()   (interactive)(scroll-up   10))
      (global-set-key (kbd "<mouse-4>") 'mouse-scroll-down)
      (global-set-key (kbd "<mouse-5>") 'mouse-scroll-up)
      )
  )

;; good scroll mode
(good-scroll-mode 1)
(global-set-key [next] #'good-scroll-up-full-screen)
(global-set-key [prior] #'good-scroll-down-full-screen)
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
;; mozc
;;

(require 'mozc)
;(add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-mozc")                                       
;(load-library "mozc")
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(set-fontset-font t 'japanese-jisx0208 "TakaoPGothic")

;;
;; auto-mode-alist
;;

(setq auto-mode-alist
      (append '(("\\.tex\\'"   . tex-mode)
                ("\\.sty\\'"   . tex-mode)
                ("\\.v\\'"     . verilog-mode)
                ("\\.sv\\'"    . verilog-mode)
                ("\\.c\\'"     . c-mode)
                ("\\.h\\'"     . c-mode)
                ("\\.cpp\\'"   . c++-mode)
                ("\\.cc\\'"    . c++-mode)
                ("\\.hpp\\'"   . c++-mode)
                ("\\.hh\\'"    . c++-mode)
                ("\\.cu\\'"    . cuda-mode)
                ("\\.hcu\\'"   . cuda-mode)
                ("\\.py\\'"    . python-mode)
                ("Makefile\\'" . makefile-mode)
                (".bashrc\\'" . sh-mode)
                ("\\.sh\\'"   . sh-mode)
                ("\\.el\\'"  .  lisp-interaction-mode))))

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

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(setq doom-modeline-height 25)
(setq doom-modeline-bar-width 4)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((((background dark)) (:background "gray40")) (t (:background "gray60"))) nil 'minimap)
 '(minimap-current-line-face ((((background dark)) (:background "gray60")) (t (:background "gray80"))) nil 'minimap)
 '(mode-line ((t (:font "RobotoMono:weight=bold" :height 0.95))))
 '(mode-line-inactive ((t (:font "RobotoMono:weight=bold" :height 0.95)))))

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
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
  ;;  :major-modes '(verilog-mode)
  ;;  :priority -1
  ;;  ))
  :hook (verilog-mode . (lambda()
      (lsp)
      (flycheck-mode t)
      (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog")))))


(setq verilog-align-ifelse                    nil
      verilog-auto-delete-trailing-whitespace t
      verilog-auto-inst-param-value           nil
      verilog-auto-inst-vector                nil
      verilog-auto-lineup                     (quote all)
      verilog-auto-newline                    nil
      verilog-auto-save-policy                nil
      verilog-auto-template-warn-unused       t
      verilog-case-indent                     4
      verilog-cexp-indent                     4 ;; no begin-end
      verilog-highlight-grouping-keywords     nil
      verilog-highlight-modules               t
      verilog-indent-level                    4
      verilog-indent-level-behavioral         4
      verilog-indent-level-declaration        4
      verilog-indent-level-module             4
      verilog-tab-to-comment                  nil)

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

(require 'flycheck-grammarly)

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode t))

(add-to-list 'flycheck-checkers 'cuda-nvcc)

;;
;; c-mode
;;

(autoload 'c-mode "cc-mode" nil t) 
(setq-default c-default-style "linux"
              c-basic-offset 4)

(c-set-offset 'case-label 4)

(put 'downcase-region 'disabled nil)
(eval-after-load 'c-modea (global-set-key (kbd "C-c p") "printf(\"%d\",);") )
(eval-after-load 'c-modea (global-set-key (kbd "C-c f") "for(int i=0; i<; i++) { }") )
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

;;
;; cuda-mode
;;


(setq enable-local-variables :safe)

(setq flycheck-cuda-include-path '("$CUDA_HOME/include" "/usr/local/cuda/samples/common/inc" "."))
(flycheck-define-checker cuda-nvcc
  "A CUDA C/C++ syntax checker using nvcc.

See URL `https://developer.nvidia.com/cuda-llvm-compiler'."
  :command ("/usr/local/cuda/bin/nvcc"
            "-c" ;; Compile Only
            "-gencode=arch=compute_80,code=sm_80"
            "--output-file" "/dev/null" ;; avoid creating output .o
            "--x=cu" ;; explicitly specify it's a CUDA language file
            (option "-std=" flycheck-cuda-language-standard concat)
            (option-list "-include" flycheck-cuda-includes)
            (option-list "-D" flycheck-cuda-definitions concat)
            (option-list "-I" flycheck-cuda-include-path)
            source)
  :error-patterns
  ((error line-start
          (message "In file included from")
          " " (or "<stdin>" (file-name))
          ":" line ":" line-end)
   (error line-start (or "<stdin>" (file-name))
          "(" line "): error: " (message) line-end)
   (error line-start (or "<stdin>" (file-name))
          ":" line ":" column
          ": fatal error: " (optional (message)) line-end)
   (warning line-start (or "<stdin>" (file-name))
            "(" line "): warning: " (message) line-end))
  :modes cuda-mode)

;; This is a function copied from stackoverflow to facify #if 0/#else/#endif keywords.
;; The comments are added by myself to make it understandable. 
(defun my-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
	;; Search #if/#else/#endif using regular expression.
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
	  ;; Handle #if.
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
		;; Handle neariest 0.
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
	    ;; Handle #else, here we can decorate #if 0->#else block using 'font-lock-comment-face'.
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
	    ;; Handle #endif, return to upper block if possible.
            (when (string= str "endif")
              (setq depth (1- depth)))))
	;; Corner case when there are only #if 0 (May be you are coding now:))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)
(defun my-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;
;; minimap-mode
;;

;; (if window-system
;;     (progn
;;       (require 'minimap)
;;       (minimap-mode nil); 常に有効にする
;;       (setq minimap-window-location 'right)
;;       (setq minimap-update-delay 0)
;;       (setq minimap-minimum-width 20)
;;       (setq minimap-highlight-line t)
;;       (setq minimap-width-fraction 0.1
;;             ;;minimap-dedicated-window t
;;             )
;;       ;; changing colors
;;       (custom-set-faces
;;        '(minimap-active-region-background ((((background dark)) (:background "gray40")) (t (:background "gray60"))) nil (quote minimap))
;;        '(minimap-current-line-face ((((background dark)) (:background "gray60")) (t (:background "gray80"))) nil (quote minimap)))
;;       (set-face-attribute 'region nil :background "#666")
;;       )
;;   )

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
;; yatex
;; 

(require 'yatex)

;; Tex mode
(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 4)
             ;; (setq indent-line-function (quote insert-tab))
             ))


;;
;; grep
;;
(global-set-key (kbd "C-x C-g") 'grep)
(setq grep-use-null-device nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(grep-command "grep --color -rin -e ")
 '(grep-find-command
   '("find . -type f -exec grep --color -nH --null -e  \\{\\} +" . 49))
 '(inhibit-startup-buffer-menu t)
 '(package-selected-packages
   '(cuda-mode flycheck-grammarly multiple-cursors yatex highlight-symbol all-the-icons which-key verilog-mode use-package tramp powerline neotree minimap lsp-ui lsp-treemacs imenu-list flycheck company-quickhelp company-flx))
 '(safe-local-variable-values
   '((eval setq
           '("$CUDA_HOME" ".")
           (add-to-list flycheck-cuda-include-path)))))

;;
;; multiple cursors
;;

(require 'multiple-cursors)
(if window-system
    (progn
      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-unset-key (kbd "M-<mouse-1>"))
      (bind-key* "M-<down-mouse-1>" 'mouse-drag-region)
      (bind-key* "M-<drag-mouse-1>" '(lambda (c) (interactive "e") (mouse-set-region c) (mc/edit-lines)))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
      )
  )

;;
;; highlight symbol
;;

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.1)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
(global-set-key (kbd "M-s M-r") 'highlight-symbol-query-replace)
(bind-key* [mouse-3] '(lambda (c) (interactive "e") (mouse-set-point c) (highlight-symbol-at-point)))

(global-unset-key [C-down-mouse-1])
(bind-key* [C-mouse-1] '(lambda (c) (interactive "e") (mouse-set-point c) (highlight-symbol-at-point)))

(bind-key* [mouse-8] 'mode-line-previous-buffer)
(bind-key* [mouse-9] 'mode-line-next-buffer)

;(bind-key* "<mode-line> <mouse-1>" '(lambda (c) (interactive "e") (mouse-buffer-menu c)))
(setq highlight-symbol-colors
      '("CadetBlue" "OrangeRed" "yellow1" "bisque" "DeepSkyBlue1" "red" "tomato" "violet" "#6262ff" "magenta" 
        "orange"  "chartreuse1" "gold"  "yellowGreen"  "lightyellow" "DodgerBlue" "greenYellow" "green"
        "cyan" "IndianRed" "cyan3"  "MediumOrchid" "turquoise1" "brown"  "chocolate"   "salmon" "Skyblue"
        "snow3" "slateGrey"  "pink" "slateBlue"  "darkGoldenrod" "blueViolet" "OrangeRed3"))

