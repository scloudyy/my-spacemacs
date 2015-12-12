;;; packages.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren
;;
;; Author: scloud <onecloud.shen@gmail.com>
;; URL: https://github.com/cosmicwalker/my-spacemacs.git
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq scloudyy-packages
      '(
        evil
        company
        cmake
        ;flycheck
        markdown
        helm
        ace-window
        org
        swiper
        youdao-dictionary
        powerline
        yasnippet
        cc-mode
        ycmd
        whitespace
        org-octopress
        chinese-pyim
        ;visual-regexp
        ;visual-regexp-steroids
        avy
        hydra
        prodigy
        helm-github-stars
        w3m
        (dired-mode :location built-in)
        helm-ls-git
        ;org-download
        ;org-tree-slide
        (occur-mode :location built-in)
 ;       counsel
        mu4e
        persp-mode
        ))

;;configs for EVIL mode
(defun scloudyy/post-init-evil ()
  (use-package evil
    :init
    (progn
      ;; make underscore as word_motion.
      (modify-syntax-entry ?_ "w")
      ;; ;; change evil initial mode state
      (loop for (mode . state) in
            '((shell-mode . normal))
            do (evil-set-initial-state mode state))

      ;;mimic "nzz" behaviou in vim
      (defadvice evil-ex-search-next (after advice-for-evil-search-next activate)
        (evil-scroll-line-to-center (line-number-at-pos)))

      (defadvice evil-ex-search-previous (after advice-for-evil-search-previous activate)
        (evil-scroll-line-to-center (line-number-at-pos)))

      (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

      (define-key evil-normal-state-map
        (kbd "Y") 'zilongshanren/yank-to-end-of-line)

      ;; rebind g,k to gj and gk
      (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
      (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

      (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
      (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))


      (define-key evil-normal-state-map (kbd "[ b") 'spacemacs/previous-useful-buffer)
      (define-key evil-normal-state-map (kbd "] b") 'spacemacs/next-useful-buffer)

      ;; (define-key evil-insert-state-map "\C-e" 'end-of-line)
      ;; (define-key evil-insert-state-map "\C-n" 'next-line)
      ;; (define-key evil-insert-state-map "\C-k" 'kill-line)
      (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
      (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)

      (evil-leader/set-key "bi" 'ibuffer)
      (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
      (define-key evil-ex-completion-map "\C-b" 'backward-char)
      (define-key evil-ex-completion-map "\C-k" 'kill-line)
      (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

      (define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
      (define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)
      (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
      (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
      (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
      (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)

      ;; in spacemacs, we always use evilify miscro state
      (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

      ;; (define-key evil-emacs-state-map (kbd "C-w h") 'evil-window-left)
      (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)
      ;; (define-key evil-emacs-state-map (kbd "C-w j") 'evil-window-down)
      ;; (define-key evil-emacs-state-map (kbd "C-w k") 'evil-window-up)
      ;; (define-key evil-emacs-state-map (kbd "C-w l") 'evil-window-right)

      ;; for emacs shell mode
      ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
      ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
      (evil-define-key 'emacs term-raw-map (kbd "C-w")
        'evil-delete-backward-word)
      (define-key evil-emacs-state-map (kbd "s-p") 'projectile-switch-project)

      (evil-leader/set-key "fR" 'zilongshanren/rename-file-and-buffer)

      ;; enable hybrid editing style
      (defadvice evil-insert-state (around zilongshanren/holy-mode activate)
        "Preparing the holy water flasks."
        (evil-emacs-state))
      (define-key input-decode-map [?\C-\[] (kbd "<C-[>"))
      (bind-keys ("<C-[>" . evil-normal-state))
      (setq evil-emacs-state-cursor '("chartreuse3" (bar . 2)))
      (define-key evil-emacs-state-map [escape] 'evil-normal-state)


      )))

(defun scloudyy/post-init-company ()
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.08)
  (global-set-key (kbd "C-.") 'company-complete)
  (when (configuration-layer/package-usedp 'company)
    (spacemacs|add-company-hook lua-mode)
    (spacemacs|add-company-hook nxml-mode)))

(defun scloudyy/post-init-cmake-mode ()
  (use-package cmake-mode
    :defer
    :config
    (progn
      (defun cmake-rename-buffer ()
        "Renames a CMakeLists.txt buffer to cmake-<directory name>."
        (interactive)
        (when (and (buffer-file-name)
                   (string-match "CMakeLists.txt" (buffer-name)))
          (setq parent-dir (file-name-nondirectory
                            (directory-file-name
                             (file-name-directory (buffer-file-name)))))
          (setq new-buffer-name (concat "cmake-" parent-dir))
          (rename-buffer new-buffer-name t)))

      (add-hook 'cmake-mode-hook (function cmake-rename-buffer)))))

;; (defun scloudyy/post-init-flycheck ()
;;   ;(use-package flycheck
;;   ;  :defer t
;;   ;  :config (progn
;;               ;(flycheck-package-setup)
;;               ;(setq flycheck-display-errors-function 'flycheck-display-error-messages)
;;               (setq flycheck-display-errors-delay 0.2)
;;               (dolist (mode '(c-mode-hook c++-mode-hook))
;;                 (spacemacs/add-flycheck-hook mode))
;;              ; (spacemacs|evilify-map flycheck-error-list-mode-map
;;              ;   :mode flycheck-error-list-mode
;;              ;   :bindings
;;              ;   (kbd "RET") 'flycheck-error-list-goto-error)
;;              ;  (evilify flycheck-error-list-mode flycheck-error-list-mode-map)
;;               ; ))
;; )

;; configs for writing
(defun scloudyy/post-init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :config
    (progn
      (when (configuration-layer/package-usedp 'company)
        (spacemacs|add-company-hook markdown-mode))
      (defun zilongshanren/markdown-to-html ()
        (interactive)
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name))
        (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

      (evil-leader/set-key-for-mode 'gfm-mode-map
        "mp" 'zilongshanren/markdown-to-html)
      (evil-leader/set-key-for-mode 'markdown-mode
        "mp" 'zilongshanren/markdown-to-html))))

(defun scloudyy/post-init-helm ()
  (use-package helm
    :init
    (progn
      (global-set-key (kbd "C-s-y") 'helm-show-kill-ring)
      ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
      ;; discussion of these options.
      (setq helm-split-window-in-side-p t
            helm-move-to-line-cycle-in-source t
            helm-ff-search-library-in-sexp t
            helm-ff-file-name-history-use-recentf t)

      (setq helm-completing-read-handlers-alist
            '((describe-function . ido)
              (describe-variable . ido)
              (debug-on-entry . helm-completing-read-symbols)
              (find-function . helm-completing-read-symbols)
              (find-tag . helm-completing-read-with-cands-in-buffer)
              (ffap-alternate-file . nil)
              (tmm-menubar . nil)
              (dired-do-copy . nil)
              (dired-do-rename . nil)
              (dired-create-directory . nil)
              (find-file . ido)
              (copy-file-and-rename-buffer . nil)
              (rename-file-and-buffer . nil)
              (w3m-goto-url . nil)
              (ido-find-file . nil)
              (ido-edit-input . nil)
              (mml-attach-file . ido)
              (read-file-name . nil)
              (yas/compile-directory . ido)
              (execute-extended-command . ido)
              (minibuffer-completion-help . nil)
              (minibuffer-complete . nil)
              (c-set-offset . nil)
              (wg-load . ido)
              (rgrep . nil)
              (read-directory-name . ido))))))

(defun scloudyy/post-init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (global-set-key (kbd "C-x C-o") #'ace-window)))

;;In order to export pdf to support Chinese, I should install Latex at here: https://www.tug.org/mactex/
;; http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;;http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun scloudyy/post-init-org ()
  (progn
    ;; https://github.com/syl20bnr/spacemacs/issues/2994#issuecomment-139737911
    ;; (when (configuration-layer/package-usedp 'company)
    ;;   (spacemacs|add-company-hook org-mode))
    (spacemacs|disable-company org-mode)

    (require 'org-compat)
    (require 'org)
    ;; (add-to-list 'org-modules "org-habit")
    (add-to-list 'org-modules 'org-habit)
    (require 'org-habit)

    ;; define the refile targets
    (setq org-agenda-files (quote ("/home/sclouds/KuaiPan/org-notes" )))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-targets
          '((nil :maxlevel . 4)
            (org-agenda-files :maxlevel . 4)))
    ;; config stuck project
    (setq org-stuck-projects
          '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

    (setq org-agenda-inhibit-startup t)       ;; ~50x speedup
    (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
    (setq org-agenda-window-setup 'current-window)
    (setq org-log-done t)

    ;; 加密文章
    ;; "http://coldnew.github.io/blog/2013/07/13_5b094.html"
    ;; org-mode 設定
    (require 'org-crypt)

    ;; 當被加密的部份要存入硬碟時，自動加密回去
    (org-crypt-use-before-save-magic)

    ;; 設定要加密的 tag 標籤為 secret
    (setq org-crypt-tag-matcher "secret")

    ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
    ;; (但是子項目還是會被加密喔)
    (setq org-tags-exclude-from-inheritance (quote ("secret")))

    ;; 用於加密的 GPG 金鑰
    ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
    (setq org-crypt-key nil)

    (add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))

    (setq org-mobile-directory "/home/sclouds/KuaiPan/org-notes/org")


    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                  (sequence "WAITING(w@/!)" "SOMEDAY(S)"  "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; Change task state to STARTED when clocking in
    (setq org-clock-in-switch-to-state "STARTED")
    ;; Save clock data and notes in the LOGBOOK drawer
    (setq org-clock-into-drawer t)
    ;; Removes clocked tasks with 0:00 duration
    (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line


    (setq org-default-notes-file "/home/sclouds/KuaiPan/org-notes/gtd.org")

    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "/home/sclouds/KuaiPan/org-notes/gtd.org" "Daily Tasks")
             "* TODO %?\n  %i\n"
             :empty-lines 1)
            ("n" "notes" entry (file+headline "/home/sclouds/KuaiPan/org-notes/notes.org" "Quick notes")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline "/home/sclouds/KuaiPan/org-notes/notes.org" "Blog Ideas")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("w" "work" entry (file+headline "/home/sclouds/KuaiPan/org-notes/gtd.org" "Lab Or School")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("c" "Chrome" entry (file+headline "/home/sclouds/KuaiPan/org-notes/notes.org" "Quick notes")
             "* TODO %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
             :empty-lines 1)
            ("l" "links" entry (file+headline "/home/sclouds/KuaiPan/org-notes/notes.org" "Quick notes")
             "* TODO %?\n  %i\n %a \n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree "/home/sclouds/KuaiPan/org-notes/journal.org")
             "* %?"
             :empty-lines 0)
            ("i" "idea" entry (file+headline "/home/sclouds/KuaiPan/org-notes/idea.org" "Idea")
             "* %?\n %U"
             :empty-lines 1)))

    (setq org-tags-match-list-sublevels nil)
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "+PRIORITY=\"B\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"lab or school\"")
            ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"scloud\"")
            ("W" "Weekly Review"
             ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states) ; turn off logging
        (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
    (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
    ;; used by org-clock-sum-today-by-tags
    (defun filter-by-tags ()
      (let ((head-tags (org-get-tags-at)))
        (member current-tag head-tags)))

    (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
      (interactive "P")
      (let* ((timerange-numeric-value (prefix-numeric-value timerange))
             (files (org-add-archive-files (org-agenda-files)))
             (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                             "LIFE" "PROJECT" "OTHER"))
             (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
             (output-string "")
             (tstart (or tstart
                         (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                         (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                         (org-time-today)))
             (tend (or tend
                       (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                       (+ tstart 86400)))
             h m file item prompt donesomething)
        (while (setq file (pop files))
          (setq org-agenda-buffer (if (file-exists-p file)
                                      (org-get-agenda-file-buffer file)
                                    (error "No such file %s" file)))
          (with-current-buffer org-agenda-buffer
            (dolist (current-tag include-tags)
              (org-clock-sum tstart tend 'filter-by-tags)
              (setcdr (assoc current-tag tags-time-alist)
                      (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
        (while (setq item (pop tags-time-alist))
          (unless (equal (cdr item) 0)
            (setq donesomething t)
            (setq h (/ (cdr item) 60)
                  m (- (cdr item) (* 60 h)))
            (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
        (unless donesomething
          (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
        (unless noinsert
          (insert output-string))
        output-string))


    ;; http://wenshanren.org/?p=327
    ;; change it to helm
    (defun scloudyy/org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
                "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
                "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
                "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
                "scheme" "sqlite")))
         (list (ido-completing-read "Source code type: " src-code-types))))
      (progn
        (newline-and-indent)
        (insert (format "#+BEGIN_SRC %s\n" src-code-type))
        (newline-and-indent)
        (insert "#+END_SRC\n")
        (previous-line 2)
        (org-edit-src-code)))

    (add-hook 'org-mode-hook '(lambda ()
                                ;; keybinding for editing source code blocks
                                ;; keybinding for inserting code blocks
                                (local-set-key (kbd "C-c i s")
                                               'scloudyy/org-insert-src-block)
                                ))
    (require 'ox-publish)
    (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                      ("\\section{%s}" . "\\section*{%s}")
                                      ("\\subsection{%s}" . "\\subsection*{%s}")
                                      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                      ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                      ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    ;; {{ export org-mode in Chinese into PDF
    ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
    ;; and you need install texlive-xetex on different platforms
    ;; To install texlive-xetex:
    ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
    ;; }}
    (setq org-latex-default-class "ctexart")
    (setq org-latex-pdf-process
          '(
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "xelatex -interaction nonstopmode -output-directory %o %f"
            "rm -fr %b.out %b.log %b.tex auto"))

    (setq org-latex-listings t)
    ;; improve org babel

    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
        ;(perl . t)
        ;(ruby . t)
        (sh . t)
        ;(js . t)
        (python . t)
        (emacs-lisp . t)
        ;(plantuml . t)
        (C . t)
        ;(R . t)
                                        ;(ditaa . t)
        ))

    (setq org-plantuml-jar-path
          (expand-file-name "~/.spacemacs.d/plantuml.jar"))
    (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")


    (defvar zilongshanren-website-html-preamble
      "<div class='nav'>
<ul>
<li><a href='http://zilongshanren.com'>博客</a></li>
<li><a href='/index.html'>Wiki目录</a></li>
</ul>
</div>")
    (defvar zilongshanren-website-html-blog-head
      " <link rel='stylesheet' href='css/site.css' type='text/css'/> \n
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/worg.css\"/>")
    (setq org-publish-project-alist
          `(
            ("blog-notes"
             :base-directory "/home/sclouds/KuaiPan/org-notes"
             :base-extension "org"
             :publishing-directory "/home/sclouds/KuaiPan/org-notes/public_html/"

             :recursive t
             :html-head , zilongshanren-website-html-blog-head
             :publishing-function org-html-publish-to-html
             :headline-levels 4         ; Just the default for this project.
             :auto-preamble t
             :exclude "gtd.org"
             :exclude-tags ("ol" "noexport")
             :section-numbers nil
             :html-preamble ,zilongshanren-website-html-preamble
             :author "zilongshanren"
             :email "guanghui8827@gmail.com"
             :auto-sitemap t               ; Generate sitemap.org automagically...
             :sitemap-filename "index.org" ; ... call it sitemap.org (it's the default)...
             :sitemap-title "我的wiki"     ; ... with title 'Sitemap'.
             :sitemap-sort-files anti-chronologically
             :sitemap-file-entry-format "%t" ; %d to output date, we don't need date here
             )
            ("blog-static"
             :base-directory "/home/sclouds/KuaiPan/org-notes"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
             :publishing-directory "/home/sclouds/KuaiPan/org-notes/public_html/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("blog" :components ("blog-notes" "blog-static"))))


    (global-set-key (kbd "C-c a") 'org-agenda)
    (define-key org-mode-map (kbd "s-p") 'org-priority)
    (define-key global-map (kbd "<f9>") 'org-capture)
    (global-set-key (kbd "C-c b") 'org-iswitchb)
    (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
    (evil-leader/set-key-for-mode 'org-mode
      "owh" 'plain-org-wiki-helm
      "owf" 'plain-org-wiki)

    ))

(defun scloudyy/init-swiper ()
  "Initialize my package"
  (use-package swiper
    :init
    (progn
      (setq ivy-use-virtual-buffers t)
      (setq ivy-display-style 'fancy)

      ;; http://oremacs.com/2015/04/16/ivy-mode/

      ;; http://oremacs.com/2015/04/19/git-grep-ivy/
      (defun counsel-git-grep-function (string &optional _pred &rest _u)
        "Grep in the current git repository for STRING."
        (split-string
         (shell-command-to-string
          (format
           "git --no-pager grep --full-name -n --no-color -i -e \"%s\""
           string))
         "\n"
         t))

      (defun counsel-git-grep ()
        "Grep for a string in the current git repository."
        (interactive)
        (let ((default-directory (locate-dominating-file
                                  default-directory ".git"))
              (val (ivy-read "pattern: " 'counsel-git-grep-function))
              lst)
          (when val
            (setq lst (split-string val ":"))
            (find-file (car lst))
            (goto-char (point-min))
            (forward-line (1- (string-to-number (cadr lst)))))))


      (use-package recentf
        :config
        (setq recentf-exclude
              '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
                ".*png$"))
        (setq recentf-max-saved-items 60))
      (evilified-state-evilify ivy-occur-mode ivy-occur-mode-map)
      (use-package ivy
        :defer t
        :config
        (progn
          (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
          (define-key ivy-minibuffer-map (kbd "s-o") 'ivy-dispatching-done)
          (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
          (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)))

      (define-key global-map (kbd "C-s") 'swiper)
      (ivy-mode t)
      ;;(evil-leader/set-key (kbd "bb") 'ivy-switch-buffer)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "C-c j") 'counsel-git-grep))))

(defun scloudyy/post-init-youdao-dictionary ()
  (evil-leader/set-key "oy" 'youdao-dictionary-search-at-point+))

(defun sclouydyy/post-init-powerline ()
  (setq powerline-default-separator 'arrow))

(defun scloudyy/post-init-yasnippet ()
  (progn
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                           org-mode-hook
                                                                           markdown-mode-hook))

    (defun zilongshanren/load-yasnippet ()
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
          (setq yas-snippet-dirs  my-snippet-dir)
          (yas-load-directory my-snippet-dir)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (spacemacs/add-to-hooks 'zilongshanren/load-yasnippet '(prog-mode-hook
                                                            markdown-mode-hook
                                                            org-mode-hook))
    ))

(defun scloudyy/post-init-cc-mode ()
  ;; company backend should be grouped
  ;(define-key c++-mode-map (kbd "s-.") 'company-ycmd)
  (setq company-backends-c-mode-common '((company-c-headers
                                          company-dabbrev-code
                                          company-keywords
                                          company-etags
                                          company-gtags :with company-yasnippet)
                                          company-files company-dabbrev ))
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  )


(defun scloudyy/post-init-ycmd ()
  (setq ycmd-tag-files 'auto)
  (setq ycmd-request-message-level -1)
  (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/Github/ycmd/ycmd/__main__.py")))
  (define-key c++-mode-map (kbd "M-[") 'ycmd-goto)
  (define-key c++-mode-map (kbd "C-M-y") 'ycmd-parse-buffer)
  )

(defun scloudyy/post-init-whitespace ()
  (set-face-attribute 'whitespace-tab nil
                      :background "#Adff2f"
                      :foreground "#00a8a8"
                      :weight 'bold)
  (set-face-attribute 'whitespace-trailing nil
                      :background "#e4eeff"
                      :foreground "#183bc8"
                      :weight 'normal)
  (diminish 'whitespace-mode))

(defun scloudyy/init-org-octopress ()
  (use-package org-octopress
    :init
    (progn
      (evilified-state-evilify org-octopress-summary-mode org-octopress-summary-mode-map)
      (add-hook 'org-octopress-summary-mode-hook
                #'(lambda () (local-set-key (kbd "q") 'bury-buffer)))

      (setq org-blog-dir "~/blogs/")
      (setq org-octopress-directory-top org-blog-dir)
      (setq org-octopress-directory-posts (concat org-blog-dir "source/_posts"))
      (setq org-octopress-directory-org-top org-blog-dir)
      (setq org-octopress-directory-org-posts (concat org-blog-dir "blog"))
      (setq org-octopress-setup-file (concat org-blog-dir "setupfile.org"))

      (defun scloudyy/org-save-and-export-blogs ()
        (interactive)
        (org-octopress-setup-publish-project)
        (org-publish-project "octopress" t))

      (evil-leader/set-key "ob" 'scloudyy/org-save-and-export-blogs)
      )))

(defun scloudyy/init-visual-regexp ()
  (use-package visual-regexp
    :init))

(defun scloudyy/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace))))

(defun scloudyy/post-init-avy ()
  (use-package avy
    :defer t
    :init
    (progn
      (require 'ace-pinyin)
      (setq ace-pinyin-use-avy t)
      (global-set-key (kbd "C-M-;") 'avy-goto-char-2))))

(defun scloudyy/init-hydra ()
  (use-package hydra
    :init
    (progn
      (when (configuration-layer/package-usedp 'org)
        ;; major mode hydra is really cool, don't need to switch mode anymore
        ;; C-c [a-z] and s-[a-z] is very quick to pressed even in emacs-state and F1-F9 is also the same
        ;; If the command will change the buffer, they should be put in these groups.
        ;; otherwise, use which-key + spacems + user defined key mappings in evil normal mode
        (defhydra hydra-org (:color blue :hint nil)
          "
              ^Org Mode^
--------------------------------------------
          _t_ags   _p_riority _P_roperty
          "
          ("p" org-priority)
          ("t" org-set-tags)
          ("P" org-set-property))
        (require 'org)
        (define-key org-mode-map (kbd "<f2>") 'hydra-org/body)
        (evil-leader/set-key-for-mode 'org-mode
          "." 'hydra-org/body)
        )
      (defhydra hydra-yasnippet (:color blue :hint nil)
        "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
        ("d" yas-load-directory)
        ("e" yas-activate-extra-mode)
        ("i" yas-insert-snippet)
        ("f" yas-visit-snippet-file :color blue)
        ("n" yas-new-snippet)
        ("t" yas-tryout-snippet)
        ("l" yas-describe-tables)
        ("g" yas/global-mode)
        ("m" yas/minor-mode)
        ("a" yas-reload-all))

      ;; (bind-key* "<f3>" 'hydra-yasnippet/body)

      (defhydra hydra-apropos (:color blue)
        "Apropos"
        ("a" apropos "apropos")
        ("c" apropos-command "cmd")
        ("d" apropos-documentation "doc")
        ("e" apropos-value "val")
        ("l" apropos-library "lib")
        ("o" apropos-user-option "option")
        ("u" apropos-user-option "option")
        ("v" apropos-variable "var")
        ("i" info-apropos "info")
        ("t" tags-apropos "tags")
        ("z" hydra-customize-apropos/body "customize"))

      (defhydra hydra-customize-apropos (:color blue)
        "Apropos (customize)"
        ("a" customize-apropos "apropos")
        ("f" customize-apropos-faces "faces")
        ("g" customize-apropos-groups "groups")
        ("o" customize-apropos-options "options"))

      (bind-key*  "<f4>" 'hydra-apropos/body)
      )))


(defun scloudyy/post-init-prodigy ()
  (prodigy-define-tag
    :name 'jekyll
    :env '(("LANG" "en_US.UTF-8")
           ("LC_ALL" "en_US.UTF-8")))
  ;; define service

  (prodigy-define-service
    :name "Hexo Server"
    :command "hexo"
    :args '("server")
    :cwd "~/blogs"
    :tags '(hexo server)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Hexo Deploy"
    :command "hexo"
    :args '("deploy" "--generate")
    :cwd "~/blogs"
    :tags '(hexo deploy)
    :kill-signal 'sigkill
    :kill-process-buffer-on-stop t)
)

(defun scloudyy/init-elfeed ()
  (use-package elfeed
    :init
    (global-set-key (kbd "C-x w") 'elfeed)
    :defer t
    :config
    (progn
      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsist.com/rss"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"
              "http://ergoemacs.org/emacs/blog.xml"))

      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :bindings
        "G" 'elfeed-update
        "g" 'elfeed-search-update--force)

      (defun elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring"
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank))))

(defun scloudyy/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "scloudyy")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))

(defun scloudyy/init-w3m ()
  ;;http://blog.chinaunix.net/uid-26185912-id-3248452.html
  ;;http://www.cnblogs.com/FelixLee/archive/2011/04/04/2412601.html
  (use-package w3m
    :defer t
    :config
    (progn
      (setq browse-url-browser-function 'w3m-browse-url)
      (autoload 'w3m "w3m" "interface for w3m on emacs" t)
      (setq w3m-command-arguments '("-cookie" "-F"))
      (setq w3m-use-cookies t)
      (setq w3m-home-page "http://www.baidu.com")
      ;(require 'mime-w3m)
      (setq w3m-default-display-inline-image nil)
      (setq w3m-default-toggle-inline-images nil))))

(defun scloudyy/init-dired-mode ()
  (use-package dired-mode
    :init
    (progn
      (defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
  (interactive)
  (let* (
         (ξfile-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (ξdo-it-p (if (<= (length ξfile-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))

    (when ξdo-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (fPath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t))) ξfile-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (fPath) (shell-command (format "open \"%s\"" fPath)))  ξfile-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath))) ξfile-list))))))

      (defun xah-open-in-desktop ()
        "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
        (interactive)
        (cond
         ((string-equal system-type "windows-nt")
          (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
         ((string-equal system-type "darwin") (shell-command "open ."))
         ((string-equal system-type "gnu/linux")
          (let (
                (process-connection-type nil)
                (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                                     "/usr/bin/gvfs-open"
                                   "/usr/bin/xdg-open")))
            (start-process "" nil openFileProgram "."))
          ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
          )))

      (defun dired-get-size ()
        (interactive)
        (let ((files (dired-get-marked-files)))
          (with-temp-buffer
            (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
            (message
             "Size of all marked files: %s"
             (progn
               (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
               (match-string 1))))))

      (defun dired-start-process (cmd &optional file-list)
        (interactive
         (let ((files (dired-get-marked-files
                       t current-prefix-arg)))
           (list
            (dired-read-shell-command "& on %s: "
                                      current-prefix-arg files)
            files)))
        (let (list-switch)
          (start-process
           cmd nil shell-file-name
           shell-command-switch
           (format
            "nohup 1>/dev/null 2>/dev/null %s \"%s\""
            (if (and (> (length file-list) 1)
                     (setq list-switch
                           (cadr (assoc cmd dired-filelist-cmd))))
                (format "%s %s" cmd list-switch)
              cmd)
            (mapconcat #'expand-file-name file-list "\" \"")))))

      (defun dired-open-term ()
        "Open an `ansi-term' that corresponds to current directory."
        (interactive)
        (let* ((current-dir (dired-current-directory))
               (buffer (if (get-buffer "*zshell*")
                           (switch-to-buffer "*zshell*")
                         (ansi-term "/bin/zsh" "zshell")))
               (proc (get-buffer-process buffer)))
          (term-send-string
           proc
           (if (file-remote-p current-dir)
               (let ((v (tramp-dissect-file-name current-dir t)))
                 (format "ssh %s@%s\n"
                         (aref v 1) (aref v 2)))
             (format "cd '%s'\n" current-dir)))))

      (defun dired-copy-file-here (file)
        (interactive "fCopy file: ")
        (copy-file file default-directory))

      ;;dired find alternate file in other buffer
      (defun my-dired-find-file ()
        "Open buffer in another window"
        (interactive)
        (let ((filename (dired-get-filename nil t)))
          (if (car (file-attributes filename))
              (dired-find-alternate-file)
            (dired-find-file-other-window))))

      ;; do command on all marked file in dired mode
      (defun zilongshanren/dired-do-command (command)
        "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
        (interactive "CRun on marked files M-x ")
        (save-window-excursion
          (mapc (lambda (filename)
                  (find-file filename)
                  (call-interactively command))
                (dired-get-marked-files))))

      (defun zilongshanren/dired-up-directory()
        "goto up directory and resue buffer"
        (interactive)
        (find-alternate-file ".."))
      )
    :defer t
  )
)

(defun scloudyy/init-helm-ls-git ()
  (use-package helm-ls-git
    :init
    (progn
      ;;beautify-helm buffer when long file name is present
      (setq helm-ls-git-show-abs-or-relative 'relative))))

(defun scloudyy/init-org-download ()
  (use-package org-download
    :defer t
    :init
    (org-download-enable)))

(defun scloudyy/init-org-tree-slide ()
  (use-package org-tree-slide
    :init
    (evil-leader/set-key "oto" 'org-tree-slide-mode)))

(defun scloudyy/init-counsel ()
  (use-package counsel
    :init
    (progn
      (global-set-key (kbd "C-h v") 'counsel-describe-variable)
      (global-set-key (kbd "C-h f") 'counsel-describe-function)
      (evil-leader/set-key "hdv" 'counsel-describe-variable)
      (evil-leader/set-key "hdf" 'counsel-describe-function)
      ;(bind-key* "M-x" 'counsel-M-x)
      (evil-leader/set-key dotspacemacs-command-key 'counsel-M-x)
      )))

(defun scloudyy/init-occur-mode ()
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
  (bind-key* "M-s o" 'occur-dwim)
  (evilified-state-evilify occur-mode occur-mode-map
    "RET" 'occur-mode-goto-occurrence))

(defun scloudyy/post-init-mu4e()
  );;; Set up some common mu4e variables
(setq mu4e-maildir "~/Maildir"         ;; top-level Maildir
      mu4e-trash-folder "/Trash"       ;; trashed messages
      mu4e-refile-folder "/Archive"    ;; saved messages
      mu4e-drafts-folder "/Drafts"     ;; unfinished messages
      mu4e-sent-folder   "/Sent"       ;; folder for sent messages
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval nil
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

;;; Mail directory shortcuts
(setq mu4e-maildir-shortcuts
      '(("/gmail/INBOX" . ?g)
        ("/college/INBOX" . ?c)))

;;; Bookmarks
(setq mu4e-bookmarks
      `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("mime:image/*" "Messages with images" ?p)
        (,(mapconcat 'identity
                     (mapcar
                      (lambda (maildir)
                        (concat "maildir:" (car maildir)))
                      mu4e-maildir-shortcuts) " OR ")
         "All inboxes" ?i)))

(defun scloudyy/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@C++"
    :binding "c"
    :body
    ))
