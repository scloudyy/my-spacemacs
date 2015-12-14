;;; funcs.el --- zilongshanren Layer packages File for Spacemacs
;;
;; Copyright (c) 2015-2016 zilongshanren 
;;
;; Author: zilongshanren <guanghui8827@gmail.com>
;; URL: https://github.com/zilongshanren/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'cl)


(defun zilongshanren/insert-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (zilongshanren/retrieve-chrome-current-tab-url)))

(defun zilongshanren/retrieve-chrome-current-tab-url()
  "Get the URL of the active tab of the first window"
  (interactive)
      (let ((result (do-applescript
                     (concat
                      "set frontmostApplication to path to frontmost application\n"
                      "tell application \"Google Chrome\"\n"
                      "	set theUrl to get URL of active tab of first window\n"
                      "	set theResult to (get theUrl) \n"
                      "end tell\n"
                      "activate application (frontmostApplication as text)\n"
                      "set links to {}\n"
                      "copy theResult to the end of links\n"
                      "return links as string\n"))))
        (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

(defun zilongshanren/org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun zilongshanren/org-archive-cancel-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/CANCELLED" 'file))

;; "https://github.com/vhallac/.emacs.d/blob/master/config/customize-org-agenda.el"
(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; VH: I changed this line from
      ;; (if (bh/is-project-p)
      (if (and (eq (point) (bh/find-project-task))
               (bh/is-project-p))
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

;; when save a buffer, the directory is not exsits, it will ask you to create the directory
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; remove all the duplicated emplies in current buffer
(defun zilongshanren/single-lines-only ()
  "replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))


;; for running long run ansi-term
(defun named-term (name)
  (interactive "sName: ")
  (ansi-term "/bin/zsh" name))

 (defun ash-term-hooks ()
      ;; dabbrev-expand in term
      (define-key term-raw-escape-map "/"
        (lambda ()
          (interactive)
          (let ((beg (point)))
            (dabbrev-expand nil)
            (kill-region beg (point)))
          (term-send-raw-string (substring-no-properties (current-kill 0)))))
      ;; yank in term (bound to C-c C-y)
      (define-key term-raw-escape-map "\C-y"
        (lambda ()
          (interactive)
          (term-send-raw-string (current-kill 0)))))

(defun terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer-other-window "*ansi-term*")
    (progn
      (split-window-right-and-focus)
      (ansi-term "/bin/zsh")))
  (get-buffer-process "*ansi-term*"))

(defalias 'tt 'terminal)

(defun zilongshanren/comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))

;;http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))


(defun zilongshanren/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun zilongshanren/word-count-for-chinese ()
  "「較精確地」統計中/日/英文字數。
- 文章中的註解不算在字數內。
- 平假名與片假名亦包含在「中日文字數」內，每個平/片假名都算單獨一個字（但片假
  名不含連音「ー」）。
- 英文只計算「單字數」，不含標點。
- 韓文不包含在內。

※計算標準太多種了，例如英文標點是否算入、以及可能有不太常用的標點符號沒算入等
。且中日文標點的計算標準要看 Emacs 如何定義特殊標點符號如ヴァランタン・アルカン
中間的點也被 Emacs 算為一個字而不是標點符號。"
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ; 去掉 org 文件的 OPTIONS（以#+開頭）
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                        ; 把註解行刪掉（不把註解算進字數）。
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文（含標點、片假名）
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文標點符號
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字數（不含標點）
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字數（不含標點）：%s
中日文字數（包含標點）：%s
英文字數（不含標點）：%s
=======================
中英文合計（不含標點）：%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))


(require 'ido)

(setq octopress-workdir (expand-file-name "/root/blogs/"))

(defun scloudyy/octopress-rake (command)
  "run rake commands"
  (let ((command-str (format "/bin/bash -l -c 'source $HOME/.rvm/scripts/rvm && rvm use ruby 2.0.0  && cd %s && rake %s'" octopress-workdir command)))
    (shell-command-to-string command-str)))

(defun scloudyy/octopress-qrsync (command)
  (let ((command-str (format "/usr/local/bin/qrsync %s" command )))
    (shell-command-to-string command-str)))

(defun scloudyy/octopress-generate ()
  "generate jekyll site"
  (interactive)
  (scloudyy/octopress-rake "generate")
  (message "Generate site OK"))

(defun scloudyy/octopress-deploy ()
  "default deploy task"
  (interactive)
  (scloudyy/octopress-rake "deploy")
  (scloudyy/octopress-qrsync "/root/blogs/scloudyy.json")
  (message "Deploy site OK"))

(defun scloudyy/octopress-gen-deploy ()
  "generate website and deploy"
  (interactive)
  (scloudyy/octopress-rake "gen_deploy")
  (scloudyy/octopress-qrsync "/root/blogs/scloudyy.json")
  (message "Generate and Deploy OK"))

(defun scloudyy/octopress-upimg ()
  (interactive)
  (scloudyy/octopress-qrsync "/root/blogs/scloudyy.json")
  (message "Up Img to Qiniu"))

(defun directory-parent (directory)
  (let ((parent (file-name-directory (directory-file-name directory))))
    (if (not (equal directory parent))
        parent)))

(defun zilongshanren/jekyll-serve ()
  (interactive)
  (let* ((default-directory
           (if (string-match "_posts/$" default-directory)
               (directory-parent (directory-parent default-directory))
             (directory-parent default-directory)))
         (buffer (if (get-buffer "*jekyll*")
                     (switch-to-buffer "*jekyll*")
                   (ansi-term "/bin/zsh" "jekyll")))
         (proc (get-buffer-process buffer)))
    (term-send-string proc "rake generate && rake preview\n")
    (sit-for 4)
    (browse-url "http://localhost:4000")))

(defun zilongshanren/hotspots ()
  "helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(zilongshanren//hotspots-sources))))

(defun zilongshanren//hotspots-sources ()
  "Construct the helm sources for my hotspots"
  `((name . "Mail and News")
   (candidates . (("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                  ("RSS" . elfeed)
                  ("Blog" . org-octopress)
                  ("Github" . (lambda() (helm-github-stars)))
                  ("Calculator" . (lambda () (helm-calcul-expression)))
                  ("Run current flie" . (lambda () (zilongshanren/run-current-file)))
                  ("Agenda" . (lambda () (org-agenda "" "a")))
                  ("sicp" . (lambda() (browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))))
   (candidate-number-limit)
   (action . (("Open" . (lambda (x) (funcall x)))))))

(defun prelude-shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun prelude-shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;; (defun zilongshanren/open-line-above()
;;   "open an empty line above the current line"
;;   (interactive)
;;   (save-excursion
;;     (evil-open-above 1)
;;     (evil-normal-state)
;;     ))

;; (defun zilongshanren/open-line-below()
;;   "open an empty line below the current line"
;;   (interactive)
;;   (save-excursion
;;     (evil-open-below 1)
;;     (evil-normal-state))
;;   )

(defun zilongshanren/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun zilongshanren/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
      (evil-ex command-string))
      )))


;; insert date and time
(defun zilongshanren/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun zilongshanren/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;; "http://stackoverflow.com/questions/2242572/emacs-todo-indicator-at-left-side"
(defun zilongshanren/annotate-todo ()
  "put fringe marker on TODO: lines in the curent buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO:" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
        (overlay-put overlay 'before-string (propertize "A"
                                                        'display '(left-fringe right-triangle)))))))


;; Screenshot
(defun zilongshanren//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

(defun zilongshanren/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq fullpath
        (concat (file-name-directory (buffer-file-name))
                "../images/posts/"
                (file-name-base (buffer-file-name))
                "_"
                basename))
  (setq relativepath
        (concat (file-name-base (buffer-file-name))
                "_"
                basename
                ".png"))
  (if (file-exists-p (file-name-directory fullpath))
      (progn
        (setq final-image-full-path (concat fullpath ".png"))
        (call-process "screencapture" nil nil nil "-s" final-image-full-path)
        (if (executable-find "convert")
            (progn
              (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
              (shell-command-to-string resize-command-str)))
        (zilongshanren//insert-org-or-md-img-link "http://guanghuiqu.qiniudn.com/" relativepath))
    (progn
      (call-process "screencapture" nil nil nil "-s" (concat basename ".png"))
      (zilongshanren//insert-org-or-md-img-link "./" (concat basename ".png"))))
  (insert "\n"))

;;js2-mode enhancement
(defun my-which-function ()
  ;; clean the imenu cache
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (setq imenu--index-alist nil)
  (which-function-mode t)
  (which-function))

(defun zilongshanren/run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be emacs lisp, php, perl, python, ruby, javascript, bash, ocaml, Visual Basic.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2015-08-21"
  (interactive)
  (let* (
         (ξsuffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ;; ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("lua" . "lua")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))
         (ξfname (buffer-file-name))
         (ξfSuffix (file-name-extension ξfname))
         (ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
         (ξcmd-str (concat ξprog-name " \""   ξfname "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))

    (if (string-equal ξfSuffix "el") ; special case for emacs lisp
        (load ξfname)
      (if ξprog-name
          (progn
            (message "Running…")
            (async-shell-command ξcmd-str "*zilongshanren/run-current-file output*"))
        (message "No recognized program file suffix for this file.")))))

;; "http://xuchunyang.me/Opening-iTerm-From-an-Emacs-Buffer/"
(defun zilongshanren/iterm-shell-command (command &optional prefix)
  "cd to `default-directory' then run COMMAND in iTerm.
With PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (zilongshanren/project-root)
                default-directory))
         ;; if COMMAND is empty, just change directory
         (cmd (format "cd %s ;%s" dir command)))
    (do-applescript
     (format
      "
  tell application \"iTerm\"
       activate
       set _session to current session of current terminal
       tell _session
            set command to get the clipboard
            write text \"%s\"
       end tell
  end tell
  " cmd))))

(defun zilongshanren/project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (or (locate-dominating-file directory ".git")
        (locate-dominating-file directory ".svn")
        (locate-dominating-file directory ".hg"))))


(define-minor-mode
 shadowsocks-proxy-mode
 :global t
 :init-value nil
 :lighter " SS"
 (if shadowsocks-proxy-mode
     (setq url-gateway-method 'socks)
   (setq url-gateway-method 'native)))


(define-global-minor-mode
 global-shadowsocks-proxy-mode shadowsocks-proxy-mode shadowsocks-proxy-mode
 :group 'shadowsocks-proxy)

(defun scloudyy/KuaiPan-Sync-All()
  (interactive)
  (shell-command-to-string "tar -czf /root/.emacs.d/.cache/emacs-dict.tar.gz /root/.emacs.d/.cache/pyim-bigdict.pyim /root/.emacs.d/.cache/pyim-personal.txt")
  (shell-command-to-string "cp /root/.emacs.d/.cache/emacs-dict.tar.gz /home/sclouds/KuaiPan")
  (message "Done!"))

(defun scloudyy/KuaiPan-Sync-Dict()
  (interactive)
  (shell-command-to-string "tar -czf /root/.emacs.d/.cache/emacs-dict.tar.gz /root/.emacs.d/.cache/pyim-bigdict.pyim /root/.emacs.d/.cache/pyim-personal.txt")
  (shell-command-to-string "mv /root/.emacs.d/.cache/emacs-dict.tar.gz /home/sclouds/KuaiPan")
  (message "Done!"))

(defun scloudyy/Sync-Config()
  (interactive)
  (shell-command-to-string "cp ~/.zshrc ~/Github/my-config/zsh/"))


(defun slcoudyy/Nil-Chinese()
  (interactive)
  (if (equal current-input-method "chinese-pyim")
       (toggle-input-method)
    (message "already En")))

(defun scloudyy/Open-Firefox()
  (interactive)
  (shell-command-to-string "open firefox"))
