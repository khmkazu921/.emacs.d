(setq debug-on-error t)

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("elpa" . "https://elpa.gnu.org/packages/")
                       ("stable" . "https://stable.melpa.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	                   ("tromey" . "http://tromey.com/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    ;;(package-refresh-contents t)
    (package-install 'leaf))

  (leaf leaf-keywords :ensure t
    :config
    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone . t)))
    (leaf-keywords-init)))

(leaf straight
  :init
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(add-to-list 'load-path "~/.emacs.d/user-el")

;;
;; text mode
;;
(setq-default initial-major-mode 'text-mode)
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook (lambda ()
                           (modify-syntax-entry ?/ "'")
                           ;;(modify-syntax-entry ?/ ". 12")
                           (modify-syntax-entry ?# "<")
                           (modify-syntax-entry ?$  "'")))

;;
;; Doom modeline, Doom theme
;;
(leaf *theme
  :init
  ;; (leaf doom-modeline :ensure t :require t
  ;;   :hook (after-init-hook . doom-modeline-mode)
  ;;   :custom
  ;;   ((doom-modeline-bar-width . 4)
  ;;    (doom-modeline-height . 25)))
  (leaf doom-themes :require t :ensure t
    :init
    (setq-default doom-themes-padded-modeline 4)
    (load-theme 'doom-tomorrow-night t)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))

  (leaf mood-line :ensure t :require t
    :hook (after-init-hook . mood-line-mode)  :config
    :setq-default
    ((mood-line-glyph-alist . mood-line-glyphs-unicode)))
  );; (leaf *theme)

(leaf *completion
  :config
  (leaf *ido-mode
    :init (ido-mode 1)
    :custom
    ((ido-use-filename-at-point . 'guess)
     ;;(setq ido-create-new-buffer 'never)
     (ido-enable-flex-matching  . t)
     (ido-default-buffer-method . 'selected-window))
    :config
    (defun local/switch-buffer-ido-fullpath ()
      "Switch buffer via `ido`, showing full path if available."
      (interactive)
      (let* ((choices (mapcar (lambda (b) (or (buffer-file-name b) (buffer-name b))) (buffer-list)))
             (choice (ido-completing-read "Buffer: " choices nil t)))
        (when choice (switch-to-buffer (or (get-file-buffer choice) (get-buffer choice))))))
    ) ;; ido-mode
  
  (leaf amx :require t :ensure t
    :init (amx-mode 1))
  
  (leaf ido-completing-read+ :require t :ensure t
    :init (ido-ubiquitous-mode 1))

  (leaf ido-vertical-mode :require t :ensure t
    :init (ido-vertical-mode t))
  ;; ) (fido-vertical-mode)

  (leaf orderless :require t :ensure t
    :custom
    (completion-styles . '(orderless basic))
    (completion-category-overrides . '((file (styles basic partial-completion)))))

  (leaf recentf :require t :ensure t
    :init
    (recentf-mode 1)
    (setq-default completions-sort nil)
    (setq-default recentf-max-saved-items 1000)
    (setq-default recentf-auto-cleanup 'never)
    (setq-default recentf-auto-save-timer
                  (run-with-idle-timer 30 t 'recentf-save-list))
    (defun local/recentf-ido-vertical ()
      "Use `ido-completing-read` with vertical display to open recent files."
      (interactive)
      (let ((file (ido-completing-read "Recentf: " recentf-list nil t)))
        (when file (find-file file)))))

  (leaf company :ensure t :require t
    :url "https://github.com/company-mode/company-mode"
    :doc "http://company-mode.github.io"
    :defun (global-company-mode
            company-abort)
    :config
    (global-company-mode)
    :custom
    ((auto-complete-mode . nil)
     (company-transformers . '(company-sort-by-backend-importance))
     (company-idle-delay .  0.2)
     (company-minimum-prefix-length . 2)
     (company-selection-wrap-around . t)
     (completion-ignore-case . t)
     (company-dabbrev-downcase . nil)
     (company-shell-dont-fetch-meta . t)))

  (leaf flycheck :require t :ensure t
    :config
    (global-flycheck-mode t)
    (add-to-list 'flycheck-checkers 'cuda-nvcc)
    :setq-default
    (enable-local-variables . :all)
    (flycheck-checker-error-threshold . 10000))
  );; (leaf *completion)

(leaf mozc :require t :ensure t
  :config
  ;;(load-library "mozc")
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")
  (global-set-key (kbd "C-\\") 'toggle-input-method))
;;   (prefer-coding-system 'utf-8)
;;   (prefer-coding-system 'utf-8-unix))

(leaf *font
  :init
  ;; Default
  (set-face-attribute 'default nil :family "Roboto Mono" :weight 'normal :height 105)
  (set-face-attribute 'italic nil  :family "Roboto Mono Italic" :foundry "pyrs"
                      :underline nil :slant 'italic :height 105 :width 'normal )
  ;; Japanese
  (set-fontset-font t 'japanese-jisx0208 "TakaoGothic")
  (add-to-list 'face-font-rescale-alist '(".*Takao .*" . 0.5))
  ;; Emoji
  (setq use-default-font-for-symbols nil)
  (dolist (cat '((emoji  . ("Apple Color Emoji" "Noto Emoji" "Segoe UI Emoji" "Symbola" "Noto Color Emoji"))
                 (symbol . ("Segoe UI Symbol" "Apple Symbols" "Symbola"))))
    (let ((font (seq-find (lambda (f) (member f (font-family-list))) (cdr cat))))
      (when font (set-fontset-font t (car cat) font))))
  (when (eq system-type 'windows-nt)
    (set-fontset-font t '(#x1F300 . #x1F5FF) "Segoe UI Symbol"))
  ;; Limit font scale
  (setq-default local/max-font-height 300)
  (defun local/limit-text-scale-increase (orig-fn &rest args)
    "Wrap `text-scale-increase` and `text-scale-adjust` to limit font height."
    (let* ((base-height (face-attribute 'default :height nil))
           (scale (or text-scale-mode-amount 0))
           (arg (or (car args) 1))
           (new-scale (+ scale arg))
           (new-height (truncate (* base-height (expt text-scale-mode-step new-scale)))))
      (if (> new-height local/max-font-height)
          (message "Font height limit exceeded.")
        (apply orig-fn args))))
  (advice-add 'text-scale-increase :around #'local/limit-text-scale-increase))

(leaf *global-key-binds
  :bind*
  ;; other-window
  ;; ("<C-tab>" . ace-window)
  ;; buffer related
  ("<mouse-8>" . mode-line-previous-buffer)
  ("<mouse-9>" . mode-line-next-buffer)
  ("C-x k"     . kill-this-buffer)
  ("C-x b"     . local/switch-buffer-ido-fullpath)
  ("C-x C-b"   . local/switch-buffer-ido-fullpath)
  ("C-c C-d"   . toggle-current-window-dedication)
  ("C-x C-r"   . local/recentf-ido-vertical)
  ;; window resize
  ("C-x {" . (lambda () (interactive) (enlarge-window -20 t)))
  ("C-x }" . (lambda () (interactive) (enlarge-window  20 t)))
  ("C-x _" . (lambda () (interactive) (enlarge-window -10)))
  ("C-x +" . (lambda () (interactive) (enlarge-window  10)))
  ("C-x |" . balance-windows)
  ;; grep
  ("C-x C-g" . rgrep)
  :init
  (global-unset-key [M-mouse-1])
  (global-unset-key [M-drag-mouse-1])
  (global-unset-key [M-down-mouse-1])
  (global-unset-key [M-mouse-3])
  (global-unset-key [M-mouse-2])
  ;; grep
  (setq grep-use-null-device nil)
  
  (leaf *q-map
    :preface
    ;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
    (defun revert-buffer-no-confirm ()
      "Revert buffer without confirmation."
      (interactive) (revert-buffer :ignore-auto :noconfirm))

    (defun copy-filename-on-clipboard ()
      "Put the current file name on the clipboard"
      (interactive)
      (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))) (when filename (with-temp-buffer (insert filename) (clipboard-kill-region (point-min) (point-max))) (message filename))))

    (defun copy-dirname-on-clipboard ()
      "Put the current directory name on the clipboard"
      (interactive)
      (let ((dirname (if (equal major-mode 'dired-mode) default-directory (file-name-directory (buffer-file-name))))) (when dirname (with-temp-buffer (insert dirname) (clipboard-kill-region (point-min) (point-max))) (message dirname))))

    (defun copy-selected-lines-with-path-and-numbers ()
      "Copy the currently selected region's lines with their line numbers, prefixed by the file's absolute path."
      (interactive)
      (when (and (use-region-p) (buffer-file-name))
	(let ((file-path (buffer-file-name))
              (start-line (line-number-at-pos (region-beginning)))
              (end-line (line-number-at-pos (region-end)))
              (result ""))
	  (save-excursion
            (goto-char (region-beginning))
            (setq result (concat file-path "\n")) ; Add file path at the beginning
            (while (<= (line-number-at-pos) end-line)
              (setq result (concat result (format "%d: %s" (line-number-at-pos) (thing-at-point 'line t))))
              (forward-line 1)))
	  (with-temp-buffer
            (insert result)
            (call-process-region (point-min) (point-max) "xclip" nil nil nil "-sel" "p"))
	  (message "Copied from %s : %d - %d" file-path start-line end-line))))
    :config
    (defvar ctl-q-keymap (let ((map (make-sparse-keymap)))
			   (define-key map (kbd "C-r") 'revert-buffer-no-confirm)
			   (define-key map (kbd "C-e") 'eval-region)
			   (define-key map (kbd "C-w") 'whitespace-mode)
			   (define-key map (kbd "C-t") 'toggle-truncate-lines)
			   (define-key map (kbd "C-f") 'copy-filename-on-clipboard)
			   (define-key map (kbd "C-d") 'copy-dirname-on-clipboard)
			   (define-key map (kbd "C-c") 'copy-selected-lines-with-path-and-numbers)
			   map)
      "Global keymap for characters following C-q.")
    (define-key global-map (kbd "C-q") ctl-q-keymap))
  ;; end of (leaf *q-map)
  )

(leaf *global-settings
  :init
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;;
  (setq mac-command-key-is-meta t
        mac-command-modifier 'meta)
  (delete-selection-mode t)
  (global-display-line-numbers-mode)
  (setq-default display-line-numbers-type t)
  (global-auto-revert-mode t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; (global-unset-key (kbd "C-z"))
  (xterm-mouse-mode 1)

  (setq warning-minimum-level :emergency)

  ;; scroll
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-scroll-amount '(5 ((shift) . 5) ((control) . nil)))
  ;; (pixel-scroll-precision-mode t)
    
  (setq font-lock-maximum-decoration t)

  ;; auto-mode-alist
  (setq auto-mode-alist
        (append '(("\\.sty\\'"   . tex-mode)
                  ("\\.sv\\'"    . verilog-mode)
                  ("\\.hpp\\'"   . c++-mode)
                  ("\\.hh\\'"    . c++-mode)
                  ("\\.cu\\'"    . cuda-mode)
                  ("\\.hcu\\'"   . cuda-mode)
                  ("Makefile\\'" . makefile-mode)
                  ("\\.el\\'"  .  lisp-interaction-mode)
                  ("\\.php\\'"  .  web-mode)
                  ("\\.plt\\'"  .  gnuplot-mode)
		  ) auto-mode-alist))
  
  (defun kz-home-startup ()
    (set-frame-size (selected-frame) 150 176)
    (set-frame-position (selected-frame) 0 1250)
    (set-frame-parameter (selected-frame) 'alpha '(100 100))
    (add-to-list 'default-frame-alist '(alpha 100 100))
    (treemacs)
    (other-window 1))

  (defun office-startup ()
    (add-hook 'window-setup-hook 'toggle-frame-maximized t)
    (treemacs)
    (other-window 1))

  (cond ((or (equal (system-name) "kz-home") (equal (system-name) "kz-thinkpad"))
	 (add-hook 'emacs-startup-hook 'kz-home-startup))
	(t
	 (add-hook 'emacs-startup-hook 'office-startup)))
  
  ;;
  ;; backup
  ;;
  (setq backup-by-copying t
        backup-directory-alist '(("." . "~/.emacs.d/backup"))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  
  :setq-default
  (indent-tabs-mode           . nil)
  (tab-width                  . 4)
  (c-basic-offset             . 4)
  (require-final-newline      . t)
  (ring-bell-function         . 'ignore)
  (inhibit-splash-screen      . t)
  (pop-up-windows             . nil)
  (display-line-numbers-width . 1)
  (fill-column                . 100))

(leaf htmlize :ensure t)

(leaf elscreen :ensure t :require t
  :init
  (elscreen-start)
  (defun elscreen-frame-title-update ()
    (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
      (let* ((screen-list (sort (elscreen-get-screen-list) '<))
             (screen-to-name-alist
              (cl-remove-if (lambda (item)
                              (string-match "\\*NeoTree\\*" (cdr item)))
                            (elscreen-get-screen-to-name-alist)))
             (title (concat "|    "
                            (mapconcat
                             (lambda (screen)
                               (let ((screen-name (cdr (assoc screen screen-to-name-alist))))
                                 (if screen-name
                                     (format (if (string-equal "+" (elscreen-status-label screen)) "[%d] %s" " %d  %s")
                                             screen (elscreen-truncate-screen-name screen-name 40))
                                   "")))
                                     screen-list "    |    ") "    |")))
        (if (fboundp 'set-frame-name)
            (set-frame-name title)
          (setq frame-title-format title)))))

  (eval-after-load "elscreen"
    '(add-hook 'elscreen-screen-update-hook 'elscreen-frame-title-update))

  :custom ((elscreen-prefix-key . "\C-z")
           (elscreen-display-tab . nil)
           (elscreen-tab-display-kill-screen . nil)
           (elscreen-tab-display-control . nil)))

(leaf atomic-chrome :ensure t :require t :straight t
  :init (atomic-chrome-start-server))

(leaf company :ensure t :require t
  :url "https://github.com/company-mode/company-mode"
  :doc "http://company-mode.github.io"
  ;:blackout t
  :defun (global-company-mode
          company-abort)
  :config
  (global-company-mode)
  :custom
  ((auto-complete-mode . nil)
   (company-transformers . '(company-sort-by-backend-importance))
   (company-idle-delay .  0.2)
   (company-minimum-prefix-length . 2)
   (company-selection-wrap-around . t)
   (completion-ignore-case . t)
   (company-dabbrev-downcase . nil)
   (company-shell-dont-fetch-meta . t)))

(leaf flycheck :require t :ensure t
  :config
  (global-flycheck-mode t)
  (add-to-list 'flycheck-checkers 'cuda-nvcc)
  :setq-default
  (enable-local-variables . :all)
  (flycheck-checker-error-threshold . 10000))

(leaf *prog-mode
  :init  
  (leaf tcl
    :init
    ;;(add-hook 'tcl-mode-hook 'tcl-synp-mode)
    (add-hook 'tcl-mode-hook (lambda ()
			       (modify-syntax-entry ?_ "w" tcl-mode-syntax-table)
			       (modify-syntax-entry ?$ "'" tcl-mode-syntax-table))))

  (leaf org
    :config
    (org-babel-do-load-languages 'org-babel-load-languages
				                 '((shell . t)))
    (load-file "~/.emacs.d/user-el/user-org.el")
    (global-set-key (kbd "<C-S-y>") 'my-org-paste-image)
    (when (and (eq system-type 'gnu/linux)
               (string-match
                "Linux.*Microsoft.*Linux"
                (shell-command-to-string "uname -a")))
      (setq
       browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
       browse-url-generic-args     '("/c" "start")
       browse-url-browser-function #'browse-url-generic))
    ;; (add-to-list 'org-src-lang-modes '("tcl" . tcl-synp))
    :custom
    ((org-edit-src-content-indentation . 0)
     (org-todo-keywords
      . '((sequence "TODO" "INPROGRESS" "|" "DONE" "HOLD" "CANCELLED" )))
     (org-startup-folded . nil)
     (org-use-speed-commands . t)))
  
  (leaf markdown-mode :require t :ensure t
    :config
    (add-to-list 'auto-mode-alist
		 '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . gfm-mode)))

  (leaf c-mode
    :init
    (setq-default c-default-style "linux"
		          c-basic-offset 4)
    (c-set-offset 'case-label 4)
    (put 'downcase-region 'disabled nil)
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11"))))
  
  (leaf cuda-mode :require t :ensure t
    :init
    (setq enable-local-variables :safe)
    (setq flycheck-cuda-include-path '("$CUDA_HOME/include" "/usr/local/cuda/samples/common/inc" "."))
    (flycheck-define-checker cuda-nvcc
      "A CUDA C/C++ syntax checker using nvcc. See URL `https://developer.nvidia.com/cuda-llvm-compiler'."
      :command ("/usr/local/cuda/bin/nvcc"
		        "-c" ;; Compile Only
		        "-gencode=arch=compute_80,code=sm_80"
		        "--output-file" "/dev/null" ;; avoid creating output .o
		        "--x=cu" ;; explicitly specify it's a CDA language file
		        (option "-std=" flycheck-cuda-language-standard concat)
		        (option-list "-include" flycheck-cuda-includes)
		        (option-list "-D" flycheck-cuda-definitions concat )
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
      :modes cuda-mode))

  (defun my-c-mode-font-lock-if0 (limit)
    (save-restriction
      (widen)
      (save-excursion
	    (goto-char (point-min))
	    (let ((depth 0) str start start-depth)
          (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
            (setq str (match-string 1))
            (if (string= str "if")
		        (progn
                  (setq depth (1+ depth))
                  (when (and (null start) (looking-at "\\s-+0"))
                    (setq start (match-end 0)
                          start-depth depth)))
              (when (and start (= depth start-depth))
		        (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
		        (setq start nil))
              (when (string= str "endif")
		        (setq depth (1- depth)))))
          (when (and start (> depth 0))
            (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
    nil)
  (defun my-c-mode-common-hook ()
    (font-lock-add-keywords
     nil
     '((my-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  
  (leaf gnuplot :require t :ensure t)
  
  (leaf verilog-mode :require t :ensure t
    :config
    (add-to-list 'hs-special-modes-alist
                 '(verilog-mode
                   "\\<\\(begin\\|case\\|fork\\|task\\|function\\)\\>"
                   "\\<\\(end\\|endcase\\|join\\|endtask\\|endfunction\\)\\>"
                   nil
                   (lambda (_arg) (verilog-forward-sexp)) nil))
    :hook ( (verilog-mode-hook . hs-minor-mode) )
    :bind ( ("<C-tab>" . hs-toggle-hiding) )
    :custom
    ((verilog-align-ifelse                     . nil)
     (verilog-auto-delete-trailing-whitespace . t   )
     (verilog-auto-inst-param-value           . nil )
     (verilog-auto-inst-vector                . nil )
     (verilog-auto-lineup                     . (quote all) )
     (verilog-auto-newline                    . nil )
     (verilog-auto-save-policy                . nil )
     (verilog-auto-template-warn-unused       . t   )
     (verilog-case-indent                     . 4   )
     (verilog-cexp-indent                     . 4   );; no begin-end
     ;;(verilog-highlight-grouping-keywords     . nil )
     ;;(verilog-highlight-modules               . t   )
     (verilog-indent-level                    . 4   )
     (verilog-indent-level-behavioral         . 4   )
     (verilog-indent-level-directive          . 0   )
     (verilog-indent-level-declaration        . 4   )
     (verilog-indent-level-module             . 4   )
     (verilog-tab-to-comment                  . nil )
     (verilog-indent-lists                    . nil ))
    ) ;; (leaf verilog-mode)
  ) ;; (leaf *prog-mode)

(leaf multiple-cursors :require t :ensure t
  :bind
  (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->"         . 'mc/mark-next-like-this)
   ("C-<"         . 'mc/mark-previous-like-this)
   ("M-C->"       . 'mc/unmark-next-like-this)
   ("M-C-<"       . 'mc/unmark-previous-like-this)
   ("C-c C-<"     . 'mc/mark-all-like-this)))

(leaf web-mode :require t :ensure t
  :setq-default
  (web-mode-enable-auto-indentation . nil))

(leaf which-key :require t :ensure t
  :config
  (which-key-mode)
  (setq-default which-key-idle-delay 1.0))

;; ;; TODO: popup function
;; (defun toggle-current-window-dedication ()
;;   (interactive)
;;   (let* ((window    (selected-window))
;; 	 (dedicated (window-dedicated-p window)))
;;     (set-window-dedicated-p window (not dedicated))
;;     (message "Window %sdedicated to %s"
;; 	     (if dedicated "no longer " "")
;; 	     (buffer-name))))

(leaf popper :require t :ensure t
  :init (popper-mode +1)
  :bind ((elscreen-map ("r" . 'popper-cycle) ("m" . 'multi-term) ("t" . 'popper-toggle)
                       ("y" . 'popper-toggle-type) ("g" . 'popper-kill-latest-popup)))
  :setq-default
  (popper-mode-line . " Pop ")
  (popper-reference-buffers . '(("^\\*eshell.*\\*$" . eshell-mode)
                                ("^\\*term.*\\*$" . multi-term)
                                ("^\\*gnuplot.*\\*$" . gnuplot-mode)
                                ("^\\*Warnings.*\\*$" . special-mode))))

(leaf multi-term :require t :ensure t
  :config
  (set-face-attribute 'comint-highlight-prompt nil :inherit nil)
  (add-hook 'term-mode-hook
            (lambda () (previous-buffer) (popper-toggle-type) (read-only-mode -1)))
  ;;:custom-face
  ;;(term-color-white . ((t (:foreground "#8d9fa1"))))
  :custom
  ((term-bind-key-alist
    . '(("C-p"           . term-send-up)
	    ("C-n"           . term-send-down)
	    ("C-y"           . term-paste)
	    ("C-c C-d"       . toggle-current-window-dedication)
	    ("M-d"           . (lambda () (interactive) (term-send-raw-string "\ed")))
	    ("M-<backspace>" . (lambda () (interactive) (term-send-raw-string "\e\d")))
	    ("C-c C-c"       . (lambda () (interactive) (term-send-raw-string "\C-c")))
	    ("C-c C-x"       . (lambda () (interactive) (term-send-raw-string "\C-x")))
	    ("C-c C-z"       . (lambda () (interactive) (term-send-raw-string "\C-z")))
	    ("<left>"        . (lambda () (interactive) (term-send-raw-string "\033[D")))
	    ("<right>"       . (lambda () (interactive) (term-send-raw-string "\033[C")))
	    ("<up>"          . (lambda () (interactive) (term-send-raw-string "\033[A")))
	    ("<down>"        . (lambda () (interactive) (term-send-raw-string "\033[B")))))))

(setq mode-line-misc-info 
      (cons '(:eval (if (window-dedicated-p) "(D)" "")) mode-line-misc-info))

;;
;; minimap mode
;;
;; (leaf minimap :ensure nil :require nil
;;   :custom
;;   ((minimap-window-location . 'right)
;;    ;;(minimap-hide-fringe . t)
;;    (minimap-update-delay . 0.05)
;;    (minimap-minimum-width . 10)
;;    (minimap-width-fraction . 0.01)
;;    (minimap-enlarge-certain-faces . 'always) )
;;   :custom-face
;;   (minimap-font-face . '((t (:family "Minimap" :height 10))))
;;   :config
;;   (define-advice minimap-new-minimap (:after () hide-truncation-indicators)
;;     "Hide truncation fringe indicators in the minimap buffer."
;;     (with-current-buffer minimap-buffer-name
;;       (fringe-mode '(4 . 4))
;;       (push '(truncation nil nil) fringe-indicator-alist)))
;;   (defun i-minimap-mouse-scroll (event)
;;     (interactive "e")
;;     (let* ((pointed-buffer (buffer-name (window-buffer (posn-window (event-start event)))))
;;            (delta (if (eq (event-basic-type event) 'mouse-4) -1 1)))
;;       (if (string-equal pointed-buffer minimap-buffer-name)
;; 	  (mwheel-scroll event delta))))
;; 	  ;; (if (eq delta -1) (scroll-down 40) (scroll-up 40))
;;   ;;(good-scroll-move (* delta 600)) (mwheel-scroll event delta) )))
;;   (global-set-key [mouse-4] #'i-minimap-mouse-scroll)
;;   (global-set-key [mouse-5] #'i-minimap-mouse-scroll)
;;   (global-set-key [left-fringe mouse-4] #'i-minimap-mouse-scroll)
;;   (global-set-key [left-fringe mouse-5] #'i-minimap-mouse-scroll)
;;   (global-set-key [right-fringe mouse-4] #'i-minimap-mouse-scroll)
;;   (global-set-key [right-fringe mouse-5] #'i-minimap-mouse-scroll))

;; (leaf neotree :require nil :ensure nil
;;   :bind
;;   ((neotree-mode-map
;;     ("<f10>"   . neotree-toggle)
;;     ("a"       . neotree-hidden-file-toggle)
;;     ("<left>"  . neotree-select-up-node)
;;     ("<right>" . neotree-change-root)))
;;   :custom
;;   ((neo-keymap-style quote concise)
;;    (neo-show-hidden-files . t)
;;    (neo-smart-open . t)
;;    (neo-create-file-auto-open . t)
;;    (neo-autorefresh . nil)
;;    (neo-window-fixed-size . nil)
;;    (neo-theme quote nerd)
;;    (neo-window-width . 20))
;;   :hook
;;   (neotree-mode-hook . (lambda() (display-line-numbers-mode -1))) )

(leaf treemacs :ensure t :require t
  :custom
  (treemacs-no-png-images          . t       )
  (treemacs-indentation            . '(10 px))
  (treemacs-text-scale             . 0       )
  (treemacs-indentation-string     . "-"     )
  (treemacs-show-hidden-files      . nil     )
  :custom-face
  (treemacs-window-background-face . '((nil (:height 0.8))))
  :bind
  ((treemacs-mode-map ("<f10>" . treemacs)))
  :hook
  (treemacs-mode-hook . (lambda() (display-line-numbers-mode -1))) )

(leaf idle-highlight-mode :require t :ensure t
  :custom (idle-highlight-idle-time . 0.05)
  :preface
  (defun advice/face-has-background-color (face)
    "Check if the FACE has a specified background color."
    (and (facep face) (not (equal (face-attribute face :background nil t) 'unspecified))))
  
  (defun advice/idle-highlight-has-no-background-color-at-point (orig-func pos)
    "Return nil if there is no background color at the position POS, t otherwise.
     Also serves as an advice function for idle-highlight--check-faces-at-point."
    (let ((faces (or (get-text-property pos 'face) 'default)))
      (unless (listp faces) (setq faces (list faces)))
      (if (not (seq-every-p 'advice/face-has-background-color faces))
          nil (funcall orig-func pos))))
  :advice
  (:around idle-highlight--check-faces-at-point advice/idle-highlight-has-no-background-color-at-point)
  :config
  (idle-highlight-global-mode)
  :custom-face
  (idle-highlight . '((t (:background "gray22")))))

(leaf highlight-symbol :require t :ensure t
  :bind ("<mouse-3>" . (lambda (c) (interactive "e") (mouse-set-point c) (highlight-symbol-at-point)))
  :custom*
  ((highlight-symbol-colors
    (list (doom-color 'red)   (doom-color 'yellow)  (doom-color 'blue) (doom-color 'orange)
          (doom-color 'green) (doom-color 'magenta) (doom-color 'cyan) (doom-color 'violet)
          (doom-color 'grey)  (doom-color 'fg)
	      (doom-lighten (doom-color 'red)     0.3) (doom-lighten (doom-color 'yellow) 0.3)
          (doom-lighten (doom-color 'blue)    0.3) (doom-lighten (doom-color 'green)  0.3)
          (doom-lighten (doom-color 'magenta) 0.3) (doom-lighten (doom-color 'cyan)   0.3)
          (doom-lighten (doom-color 'violet)  0.3)
	      (doom-darken  (doom-color 'red)     0.3) (doom-darken (doom-color 'yellow)  0.3)
          (doom-darken  (doom-color 'blue)    0.2) (doom-darken (doom-color 'orange)  0.2)
          (doom-darken  (doom-color 'green)   0.2) (doom-darken (doom-color 'magenta) 0.2)
          (doom-darken  (doom-color 'cyan)    0.2) (doom-darken (doom-color 'violet)  0.2)))))

;; (leaf copilot :ensure nil :require t
;;   :init
;;   (if (equal (shell-command-to-string "which node") "")
;;       (progn (message "node is not installed")
;; 	     (async-shell-command "curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash - && sudo apt-get install -y nodejs") )
;;     (message "node is already installed"))
;;   (straight-use-package
;;    '(copilot :host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el") :ensure t))
;;   :hook
;;   (prog-mode-hook . copilot-mode)
;;   :bind
;;   ((copilot-mode-map ("TAB"   . my/copilot-tab)))
;;   :custom
;;   (copilot-indent-offset-warning-disable . t)
;;   :config
;;   (defun my/copilot-tab ()
;;     (interactive) (or (copilot-accept-completion) (indent-for-tab-command))))

(leaf vlf :ensure t :require t
  :config
  (require 'vlf-setup)
  :custom
  (vlf-batch-size . 1000)
  (vlf-application . 'dont-ask))

;;
;; CUSTOM SET
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" default))
 '(grep-command "grep --color -rin -e ")
 '(grep-find-command
   '("find . -type f -exec grep --color -nH --null -e  \\{\\} +" . 49))
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(evil popper elscreen-tab idle-highlight-mode find-file-in-project tcl-synp counsel elscreen-multi-term treemacs with-editor magit rust-mode gnuplot verilog-ext ace-window swiper ivy multi-term markdown-mode web-mode flycheck which-key verilog-mode company doom-modeline htmlize mozc highlight-symbol doom-themes neotree good-scroll xclip atomic-chrome ido-vertical-mode bind-key el-get leaf-keywords))
 '(safe-local-variable-values
   '((eval setq
	   '("$CUDA_HOME" ".")
	   (add-to-list flycheck-cuda-include-path))))
 '(warning-suppress-types '((comp)))
 '(whitespace-style
   '(face trailing tabs spaces newline missing-newline-at-eof empty indentation space-after-tab space-before-tab space-mark tab-mark newline-mark)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(idle-highlight ((t (:background "gray22"))) nil "Customized with leaf in `idle-highlight-mode' block at `/home/kazuki/.emacs.d/init.el'")
 '(minimap-font-face ((t (:family "Minimap" :height 10))) nil "Customized with leaf in `minimap' block at `/home/kazuki/.emacs.d/init.el'"))
