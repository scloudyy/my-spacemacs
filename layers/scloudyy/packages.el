;; packages.el --- scloudyy Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2018 scloudyy
;;
;; Author: scloudyy <onecloud.shen@gmail.com>
;; URL: https://github.com/scloudyy/my-spacemacs.git
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
        cmake-mode
        ;; flycheck
        markdown-mode
        ;; helm
        ace-window
        org
        ;; powerline
        yasnippet
        cc-mode
        ;; ycmd
        whitespace
        ;; chinese-pyim
        avy
        prodigy
        ;; helm-github-stars
        (dired-mode :location built-in)
        persp-mode
        engine-mode
        ;; beacon
        eshell
        ;; deft
        ;; helm-gtags
        ;; elpy
        smartparens
        ;; which-key
        evil-org
        ))

;;configs for EVIL mode
(defun scloudyy/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    ;; disable highlight when use swiper or evil ex search, this option won't effect evil-ex-search-next command
    (setq-default evil-ex-search-persistent-highlight nil)

    (push "TAGS" spacemacs-useless-buffers-regexp)

    (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)

    ;; ;; change evil initial mode state
    (loop for (mode . state) in
          '((shell-mode . normal))
          do (evil-set-initial-state mode state))

    ;;mimic "nzz" behaviou in vim
    (defadvice evil-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

    (defun my-evil-yank ()
      (interactive)
      (save-excursion
        (call-interactively 'evil-yank))
      (backward-char))

    (define-key evil-visual-state-map (kbd "y") 'my-evil-yank)

    (define-key evil-normal-state-map
      (kbd "Y") 'zilongshanren/yank-to-end-of-line)

    ;; rebind g,k to gj and gk
    ;; (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    ;; (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))


    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
    (define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)

    (define-key evil-emacs-state-map (kbd "s-f") 'forward-word)
    (define-key evil-insert-state-map (kbd "s-f") 'forward-word)
    (define-key evil-emacs-state-map (kbd "s-b") 'backward-word)
    (define-key evil-insert-state-map (kbd "s-b") 'backward-word)

    (spacemacs/set-leader-keys "bi" 'ibuffer)
    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)
    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    ;; (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
    ;; (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
    (define-key evil-visual-state-map (kbd "C-r") 'zilongshanren/evil-quick-replace)
    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "mp") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    (define-key evil-visual-state-map (kbd "mf") 'mc/mark-all-like-this-in-defun)


    ;; in spacemacs, we always use evilify miscro state
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)
    ;; Don't move back the cursor one position when exiting insert mode
    (setq evil-move-cursor-back nil)

    (define-key evil-emacs-state-map (kbd "C-w") 'evil-delete-backward-word)

    ;; for emacs shell mode
    ;; (define-key evil-emacs-state-map (kbd "s-b") 'ido-switch-buffer)
    ;; (define-key evil-emacs-state-map (kbd "s-f") 'ido-find-file)
    (evil-define-key 'emacs term-raw-map (kbd "C-w")
      'evil-delete-backward-word)
    ))

(defun scloudyy/post-init-company ()
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.08)
  (add-hook 'company-mode-hook
            (lambda() (spacemacs|diminish company-mode))))

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

;; (defun scloudyy/post-init-helm ()
;;   (use-package helm
;;     :init
;;     (progn
;;       (global-set-key (kbd "C-s-y") 'helm-show-kill-ring)
;;       ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
;;       ;; discussion of these options.
;;       (setq helm-split-window-in-side-p t
;;             helm-move-to-line-cycle-in-source t
;;             helm-ff-search-library-in-sexp t
;;             helm-ff-file-name-history-use-recentf t)

;;       (setq helm-completing-read-handlers-alist
;;             '((describe-function . ido)
;;               (describe-variable . ido)
;;               (debug-on-entry . helm-completing-read-symbols)
;;               (find-function . helm-completing-read-symbols)
;;               (find-tag . helm-completing-read-with-cands-in-buffer)
;;               (ffap-alternate-file . nil)
;;               (tmm-menubar . nil)
;;               (dired-do-copy . nil)
;;               (dired-do-rename . nil)
;;               (dired-create-directory . nil)
;;               (find-file . ido)
;;               (copy-file-and-rename-buffer . nil)
;;               (rename-file-and-buffer . nil)
;;               (w3m-goto-url . nil)
;;               (ido-find-file . nil)
;;               (ido-edit-input . nil)
;;               (mml-attach-file . ido)
;;               (read-file-name . nil)
;;               (yas/compile-directory . ido)
;;               (execute-extended-command . ido)
;;               (minibuffer-completion-help . nil)
;;               (minibuffer-complete . nil)
;;               (c-set-offset . nil)
;;               (wg-load . ido)
;;               (rgrep . nil)
;;               (read-directory-name . ido))))))

(defun scloudyy/post-init-org()
  (use-package org
    :defer t
    :init
    (setq org-image-actual-width '(700))

    ;; define the refile targets
    (setq org-agenda-files (quote ("~/KuaiPan/org-notes" )))
    (setq org-default-notes-file "~/KuaiPan/org-notes/gtd.org")

    ;; the %i would copy the selected text into the template
    ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;;add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/KuaiPan/org-notes/gtd.org" "Workspace")
             "* TODO %?\n  %i\n"
             :empty-lines 1)
            ("n" "notes" entry (file+headline "~/KuaiPan/org-notes/notes.org" "Quick notes")
             "* TODO [#C] %?\n  %i\n %U"
             :empty-lines 1)
            ("b" "Blog Ideas" entry (file+headline "~/KuaiPan/org-notes/notes.org" "Blog Ideas")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("w" "work" entry (file+headline "~/KuaiPan/org-notes/gtd.org" "Cocos2D-X")
             "* TODO %?\n  %i\n %U"
             :empty-lines 1)
            ("c" "Chrome" entry (file+headline "~/KuaiPan/org-notes/notes.org" "Quick notes")
             "* TODO %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
             :empty-lines 1)
            ("l" "links" entry (file+headline "~/KuaiPan/org-notes/notes.org" "Quick notes")
             "* TODO %?\n  %i\n %a \n %U"
             :empty-lines 1)
            ("j" "Journal Entry"
             entry (file+datetree "~/KuaiPan/org-notes/journal.org")
             "* %?"
             :empty-lines 0)
            ("i" "idea" entry (file+headline "~/KuaiPan/org-notes/idea.org" "Idea")
             "* %?\n %U"
             :empty-lines 0)
            ))

    ;;An entry without a cookie is treated just like priority ' B '.
    ;;So when create new task, they are default 重要且紧急
    (setq org-agenda-custom-commands
          '(
            ("w" . "任务安排")
            ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
            ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
            ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
            ("b" "Blog" tags-todo "BLOG")
            ("p" . "项目安排")
            ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"Lab\"")
            ("W" "Weekly Review"
             ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
              (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
              ))))

    (defun org-summary-todo (n-done n-not-done)
      "Switch entry to DONE when all subentries are done, to TODO otherwise."
      (let (org-log-done org-log-states)  ; turn off logging
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

    (eval-after-load 'org
      '(progn
         (global-set-key (kbd "C-c a") 'org-agenda)
         (define-key org-mode-map (kbd "s-p") 'org-priority)
         (global-set-key (kbd "C-c b") 'org-iswitchb)
         (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
         (evil-leader/set-key-for-mode 'org-mode
           "owh" 'plain-org-wiki-helm
           "owf" 'plain-org-wiki)
         ))

    (evil-define-key 'normal evil-org-mode-map (kbd "M-J") 'org-metadown)
    (evil-define-key 'normal evil-org-mode-map (kbd "M-j") nil)
    (evil-define-key 'normal evil-org-mode-map (kbd "M-K") 'org-metaup)
    (evil-define-key 'normal evil-org-mode-map (kbd "M-k") nil)

    ;; http://wenshanren.org/?p=327
    ;; change it to helm
    (defun zilongshanren/org-insert-src-block (src-code-type)
      "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
      (interactive
       (let ((src-code-types
              '("emacs-lisp" "python" "html" "C" "sh" "java" "js" "clojure"
                "C++" "css"  "sql" "latex" "lisp" "matlab" "org" "asm"
                )))
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
                                               'zilongshanren/org-insert-src-block)
                                (setq truncate-lines nil)
                                ))
    )
  )

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

;; (defun sclouydyy/post-init-powerline ()
;;   (setq powerline-default-separator 'arrow))

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
  (setq c-basic-offset 4)

  (setq-default indent-tabs-mode nil)

  (setq c-default-style "linux")

  (setq company-backends-c-mode-common '((company-c-headers
                                          company-dabbrev-code
                                          company-keywords
                                          company-etags
                                          company-gtags :with company-yasnippet)
                                          company-files company-dabbrev ))

  (add-hook 'c++-mode-hook (lambda() (progn
                                       (local-set-key (kbd "M-l") 'company-ycmd-semantic-complete)
                                       (local-set-key (kbd "M-k p") 'ycmd-parse-buffer)
                                       (local-set-key (kbd "M-k l") 'ycmd-toggle-force-semantic-completion))))
  )


;; (defun scloudyy/post-init-ycmd ()
;;   (setq ycmd-tag-files 'auto)
;;   (setq ycmd-request-message-level -1)
;;   (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/Github/ycmd/ycmd/__main__.py")))
;;   (require 'cc-mode)
;;   (define-key c++-mode-map (kbd "M-[") 'ycmd-goto)
;;   )

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
      (setq org-octopress-directory-org-posts "/home/sclouds/KuaiPan/blog")
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

;; (defun scloudyy/post-init-avy ()
;;   (use-package avy
;;     :defer t
;;     :init
;;     (progn
;;       (require 'ace-pinyin)
;;       (setq ace-pinyin-use-avy t))))

(defun scloudyy/post-init-hydra ()
  (progn
    (defhydra hydra-hotspots (:color blue)
      "Hotspots"
      ("b" blog-admin-start "blog")
      ("g" helm-github-stars "helm github stars")
      ("r" zilongshanren/run-current-file "run current file"))

    (defhydra multiple-cursors-hydra (:hint nil)
      "
       ^Up^            ^Down^        ^Other^
             ----------------------------------------------
         [_p_]   Next    [_n_]   Next    [_l_] Edit lines
         [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
         [_M-p_] Unmark  [_M-n_] Unmark [_r_] Mark by regexp
         ^ ^             ^ ^ [_q_] Quit
       "
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("r" mc/mark-all-in-region-regexp :exit t)
      ("q"
       nil))

    (defhydra
      hydra-apropos (:color blue)
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

    (defhydra
      hydra-customize-apropos (:color blue)
      "Apropos (customize)"
      ("a" customize-apropos "apropos")
      ("f" customize-apropos-faces "faces")
      ("g" customize-apropos-groups "groups")
      ("o" customize-apropos-options "options"))

    (define-key global-map (kbd "<f1>") 'hydra-hotspots/body)
    (spacemacs/set-leader-keys "oo" 'hydra-hotspots/body)
    ;; (bind-key*  "<f4>" 'hydra-apropos/body)
    (spacemacs/set-leader-keys "oh" 'hydra-apropos/body)))

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

;; (defun scloudyy/init-helm-github-stars ()
;;   (use-package helm-github-stars
;;     :defer t
;;     :config
;;     (progn
;;       (setq helm-github-stars-username "scloudyy")
;;       (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))

;; (defun scloudyy/init-w3m ()
;;   ;;http://blog.chinaunix.net/uid-26185912-id-3248452.html
;;   ;;http://www.cnblogs.com/FelixLee/archive/2011/04/04/2412601.html
;;   (use-package w3m
;;     :defer t
;;     :config
;;     (progn
;;       (setq browse-url-browser-function 'w3m-browse-url)
;;       (autoload 'w3m "w3m" "interface for w3m on emacs" t)
;;       (setq w3m-command-arguments '("-cookie" "-F"))
;;       (setq w3m-use-cookies t)
;;       (setq w3m-home-page "http://www.baidu.com")
;;       ;(require 'mime-w3m)
;;       (setq w3m-default-display-inline-image nil)
;;       (setq w3m-default-toggle-inline-images nil))))

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

;; (defun scloudyy/init-helm-ls-git ()
;;   (use-package helm-ls-git
;;     :init
;;     (progn
;;       ;;beautify-helm buffer when long file name is present
;;       (setq helm-ls-git-show-abs-or-relative 'relative))))

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

;; (defun scloudyy/post-init-mu4e()
;;   );;; Set up some common mu4e variables
;; (setq mu4e-maildir "~/Maildir"         ;; top-level Maildir
;;       mu4e-trash-folder "/Trash"       ;; trashed messages
;;       mu4e-refile-folder "/Archive"    ;; saved messages
;;       mu4e-drafts-folder "/Drafts"     ;; unfinished messages
;;       mu4e-sent-folder   "/Sent"       ;; folder for sent messages
;;       mu4e-get-mail-command "mbsync -a"
;;       mu4e-update-interval nil
;;       mu4e-compose-signature-auto-include nil
;;       mu4e-view-show-images t
;;       mu4e-view-show-addresses t)

;; ;;; Mail directory shortcuts
;; (setq mu4e-maildir-shortcuts
;;       '(("/gmail/INBOX" . ?g)
;;         ("/college/INBOX" . ?c)))

;; ;;; Bookmarks
;; (setq mu4e-bookmarks
;;       `(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
;;         ("date:today..now" "Today's messages" ?t)
;;         ("date:7d..now" "Last 7 days" ?w)
;;         ("mime:image/*" "Messages with images" ?p)
;;         (,(mapconcat 'identity
;;                      (mapcar
;;                       (lambda (maildir)
;;                         (concat "maildir:" (car maildir)))
;;                       mu4e-maildir-shortcuts) " OR ")
;;          "All inboxes" ?i)))

(defun scloudyy/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@C++"
    :binding "c"
    :body)
  (spacemacs|define-custom-layout "@python"
    :binding "p"
    :body)
  (spacemacs|define-custom-layout "@clojure"
    :binding "l"
    :body))

(defun scloudyy/post-init-engine-mode()
  (push '(baidu
          :name "Baidu"
          :url "https://www.baidu.com/s?wd=%s")
        search-engine-alist)
  )

;; (defun scloudyy/init-beacon ()
;;   (use-package beacon
;;     :init
;;     (progn
;;       (spacemacs|add-toggle beacon
;;         :status beacon-mode
;;         :on (beacon-mode)
;;         :off (beacon-mode -1)
;;         :documentation "Enable point highlighting after scrolling"
;;         :evil-leader "otb")

;;       (spacemacs/toggle-beacon-on))
;;     :config (spacemacs|hide-lighter beacon-mode)
;;     (setq beacon-color "#00ff00")))

(defun scloudyy/post-init-eshell()
  (use-package eshell
    :init
    (setq eshell-aliases-file "~/.spacemacs.d/eshell-alias")))

;; (defun scloudyy/post-init-deft()
;;   (setq deft-directory "~/KuaiPan/notes")
;;   (setq deft-auto-save-interval 0)
;;   (setq deft-recursive t))

(defun scloudyy/post-init-chinese-pyim()
  (setq pyim-enable-words-predict nil)
  ;; (setq pyim-use-tooltip 'pos-tip)
  (setq pyim-use-tooltip nil)
  (setq x-gtk-use-system-tooltips t))

;; (defun scloudyy/post-init-flycheck ()
;;   (with-eval-after-load 'flycheck
;;     (progn
;;       (setq flycheck-check-syntax-automatically '(mode-enabled save)))))

;; (defun scloudyy/post-init-helm-gtags ()
;;   (eval-after-load 'helm-gtags
;;     '(spacemacs|hide-lighter helm-gtags-mode)))

;; (defun scloudyy/init-elpy()
;;   (use-package elpy
;;     :init
;;     (spacemacs|add-company-hook python-mode)
;;     (push 'elpy-company-backend company-backends-python-mode)
;;     (setq elpy-remove-modeline-lighter nil
;;           elpy-modules '(elpy-module-sane-defaults
;;                          elpy-module-company
;;                          elpy-module-eldoc))
;;     (elpy-enable)
;;     (spacemacs/set-leader-keys-for-major-mode 'python-mode
;;       "hh" 'elpy-doc
;;       "gg" 'elpy-goto-definition
;;       "ia" 'elpy-importmagic-add-import
;;       "ii" 'elpy-importmagic-fixup)))

(defun scloudyy/post-init-smartparens()
  (add-hook 'smartparens-mode-hook
            (lambda() (spacemacs|diminish smartparens-mode))))

(defun scloudyy/post-which-key ()
  (add-hook 'which-key-mode-hook
            (lambda() (spacemacs|diminish which-key-mode))))

(defun scloudyy/post-init-evil-org()
  (add-hook 'evil-org-mode-hook
            (lambda() (spacemacs|diminish evil-org-mode))))

(defun scloudyy/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))

(defun scloudyy/post-init-avy ()
  (progn
    (global-set-key (kbd "C-s-'") 'avy-goto-char-2)
    (global-set-key (kbd "M-'") 'avy-goto-char-2)))

(defun scloudyy/post-init-markdown-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

    (with-eval-after-load 'markdown-mode
      (progn
        ;; (when (configuration-layer/package-usedp 'company)
        ;;   (spacemacs|add-company-hook markdown-mode))

        ;;(spacemacs/set-leader-keys-for-major-mode 'gfm-mode-map
        ;;  "p" 'zilongshanren/markdown-to-html)
        ;;(spacemacs/set-leader-keys-for-major-mode 'markdown-mode
        ;;  "p" 'zilongshanren/markdown-to-html)

        (evil-define-key 'normal markdown-mode-map (kbd "TAB") 'markdown-cycle)
        ))
    ))
