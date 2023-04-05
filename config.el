;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Benjamin"
      user-mail-address "foo@bar.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq! org-directory "~/work/hf/orga/")
(setq! org-capture-todo-file "/home/vinh/work/hf/orga/todo.org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq! display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; GENERAL
(setq! evil-want-C-u-scroll nil
       evil-want-C-d-scroll nil)

;; MacOS modifier keys
(setq! mac-option-modifier 'super)
(setq! mac-command-modifier 'meta)

;; when in insert mode, use more or less normal emacs bindings
(setq! evil-disable-insert-state-bindings t)

;; entering normal mode doesn't move the cursor back
;; https://www.reddit.com/r/emacs/comments/imrwn7/exiting_insert_mode_places_cursor_1_character_pack/
(setq! evil-move-cursor-back nil)
(setq! evil-move-beyond-eol t)

;; using o or O on a comment line doesn't create another comment line
(setq! evil-want-o/O-to-continue-comments nil)

(use-package! evil
  :config
  ;; use visual-line motions even outside of visual-line-mode buffers
  ;; from https://youtu.be/xaZMwNELaJY?t=2337
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; start shell mode in insert mode
  (evil-set-initial-state 'shell-mode 'insert))

;; faster than SPC u
(map!
 :n "U" 'universal-argument)

;; shift click to select text, not open appearance menu
;; from: https://christiantietze.de/posts/2022/07/shift-click-in-emacs-to-select/
(global-set-key (kbd "S-<down-mouse-1>") #'mouse-set-mark)

(map! "C-ö" 'embark-act)

;; SEARCH

;; avy considers all visible windows
(setq! avy-all-windows t)

(map! :nv "ß" 'evil-avy-goto-char-timer)

;; note use g s SPC instead
(map!
 :n "g s r" 'counsel-rg)

(map!
 :nvi "C-s" 'swiper)

;; NAVIGATION

(map!
 :nv "J" 'forward-paragraph
 :nv "K" 'backward-paragraph)

(map!
 :n "ö" 'evil-window-next
 :n "Ö" 'evil-window-prev
 :n "ä" 'mode-line-other-buffer)

;; a bit easier and more familiar to type than $, existing kbd useless
(map!
 :nvim "C-e" 'evil-end-of-line)

;; add move line https://www.emacswiki.org/emacs/MoveLine
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(map!
 :ni "M-<up>" 'move-line-up
 :ni "M-<down>" 'move-line-down)

;; key chord

(use-package! key-chord
 :config
 (key-chord-mode 1))

(defun my-escape-and-save-file ()
    "Force normal state and save file"
    (interactive)
    (evil-force-normal-state)
    (save-buffer))

;; after editing text, press "vv" to instantly go back to normal mode and save
;; the file (basically ESC & SPC+f+s)
(after! key-chord
  (key-chord-define-global "vv" #'my-escape-and-save-file)
  (key-chord-define-global "VV" #'my-escape-and-save-file)
  (key-chord-define-global "JK" #'evil-force-normal-state))

;; EDIT
(map!
 :nv "M-'" 'comment-region)
(map!
 :nv "SPC c #" 'comment-dwim)

;; need this in visual mode, by default M-w (s-w) kills the buffer there
(map!
 :nv "M-w" 'kill-ring-save)


(defun my-kill-word-or-region-dwim ()
  "If region active kill it from START to END else backward kill word."
  ;; Don't use `(interactive "r") (start end)` since that doesn't work
  ;; when no mark is set (e.g. in a completely new buffer).
  (interactive)
  (let ((start (mark)) (end (point)))
  (if (use-region-p)
      (kill-region start end)
    (backward-kill-word 1))))

(map!
 :i "C-w" #'my-kill-word-or-region-dwim)

;; activate fine-grained undo in normal mode, instead of undoing the whole last
;; insertion -> works as known from default Emacs
;; https://stackoverflow.com/a/11656942
(setq! evil-want-fine-undo t)


;; this command requires the minor mode defined below
(defun my-window-at-top ()
  "Open a window of the same buffer at the top

  Basically, a way to jump to the top of the current file, mostly
  in order to quickly add new imports. This temporary window can
  be closed using 'C-c C-c'."
  (interactive)
  (setq buf-cur (current-buffer))
  (split-window-below 24)
  (switch-to-buffer buf-cur)
  (evil-goto-first-line 5)
  (recenter-top-bottom 5)
  (my-window-at-top--minor-mode 1)
  (message "C-c C-c to close window"))

;; local kbd to quickly close the window again
(defun my-window-at-top--close-and-deactivate ()
  (interactive)
  (my-window-at-top--minor-mode 0)
  (+workspace/close-window-or-workspace)
  (other-window -1))

;; We need a minor mode to create a local kbd
(defvar my-window-at-top--minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my-window-at-top--close-and-deactivate)
    map)
  "Minor mode specific for only #'my-window-at-top")

(define-minor-mode my-window-at-top--minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value nil
  :lighter " C-c C-c to close")

(map! :nvi "C-c i" #'my-window-at-top)


(defun my-join-line ()
  "Basically the opposite of fill-paragraph"
  (interactive)
  (evil-next-visual-line)
  (delete-indentation))

(map! :n "SPC i x" #'my-join-line)

;; Disable undo-fu mode because it messes with undoing in regions, which now
;; works again using "C-_". Theoretically, there is
;; undo-fu-allow-undo-in-region, but somehow it doesn't work consistently when
;; undoing across multiple regions.
(setq! undo-fu-mode nil)


;; APPEARANCE
(add-hook! org-mode 'org-superstar-mode)

(beacon-mode 1)

;; ORG

(map!
 :mode org-mode
 "M-<right>" 'org-metaright
 "M-<left>" 'org-metaleft
 "M-<up>" 'org-metaup
 "M-<down>" 'org-metadown)

;; special date and time addition
(defun my-org-date-time ()
  "Insert org date and time at the correct position."
  (interactive)
  (save-excursion
    (progn
      (beginning-of-line)
       ;; catch error if end of file
      (condition-case nil (forward-char 3) (error nil))
      (org-time-stamp 99)
      (insert " "))))

(defun my-org-date ()
  "Insert org date at the correct position."
  (interactive)
  (save-excursion
    (progn
      (beginning-of-line)
      ;; catch error if end of file
      (condition-case nil (forward-char 3) (error nil))
      (org-time-stamp nil)
      (insert " "))))

(map!
 :mode org-mode
 :n "SPC m j" 'my-org-date-time)

(defvar my-logseq-dir (file-name-concat (expand-file-name "~") "Dropbox" "logseq")
  "Logseq file directory, e.g. '/home/Bob/logseq/'")
(defun my-logseq-journal-entry (prompt time)
  "Create a journal entry PROMPT into logseq's journal at time TIME.

  Use the journal file of the current date and append to the end.

  If TIME argument left is empty, take the current time."
  (interactive (list (read-string "task: ")
                     (read-string
                      (format "time (default: '%s'): " (shell-command-to-string "echo -n $(date +%H:%M)")))))
  (let* ((cur-time (format-time-string "%H:%M"))
         (cur-date (format-time-string "%Y_%m_%d"))
         (filename (file-name-concat my-logseq-dir "journals" (format "%s.org" cur-date))))
    (progn
      (if (string= time "")
          (setq stime cur-time)
        (setq stime time))
      (setq entry (format "<%s> %s" stime prompt))
      ;;(message (format "echo '\n* <%s> %s' >> %s" stime prompt filename))
      (shell-command (format "echo '\n* %s' >> %s" entry filename))
      (message (format "'%s' added to %s" entry filename))
      )))

(map! :n "C-t" #'my-logseq-journal-entry)

;; capture
(after! org
  (add-to-list
   'org-capture-templates
     '("s" "screening"
      entry (file+headline "hiring.org" "In progress")
      "** TODO %c %^g\n- State \"TODO\"       from              %U")
     ))

(after! org
  (add-to-list
   'org-capture-templates
     '("T" "TODO"
      entry (file+headline "todo.org" "Inbox")
      "** TODO %?\n- State \"TODO\"       from              %U")
     ))

;; org reveal
(add-hook! 'org-mode #'org-re-reveal)

;; org roam
(setq! org-roam-directory "~/work/orga/roam")
(map!
 :mode org-mode
 :n "SPC n r t" #'org-roam-buffer-toggle-display)

;; journaling

;; (defun my-switch-to-journal ()
;;   "Switch to journal and create an entry.

;;   I did not manage to implement this with org capture because the
;;   title of the last entry in the journal is not fixed but depends
;;   on the data."
;;   (interactive)
;;   (let ((is-journal-buf-p (equal (buffer-name) "journal.org")))
;;     (unless is-journal-buf-p (switch-to-buffer "journal.org"))
;;     (progn
;;       (switch-to-buffer "journal.org")
;;       (evil-goto-line)
;;       (insert "** ")
;;       (insert (read-string "Journal entry: "))
;;       (my-org-date-time)
;;       (save-buffer)
;;       (unless is-journal-buf-p (mode-line-other-buffer)))))

;; (map! :n "C-t" #'my-switch-to-journal)

;; CODING
;; python
(use-package! pyvenv
  :config (setenv "WORKON_HOME" "~/anaconda3/envs"))

(map!
 :mode python-mode
 "M-<right>" 'python-indent-shift-right
 "M-<left>" 'python-indent-shift-left)

(map!
 :mode anaconda-mode
 :n "M-." 'anaconda-mode-find-definitions
 :n "M-r" 'anaconda-mode-find-references
 :n "M-=" 'anaconda-mode-find-assignments)

(defun my-python-breakpoint ()
  "Insert a highlighted Python breakpoint"
  (interactive)
  (evil-open-below 1)
  (insert "breakpoint()")
  (highlight-lines-matching-regexp "^[ ]*breakpoint")
  (evil-change-to-previous-state))

(map!
 :mode python-mode
 :nv "SPC c b" #'my-python-breakpoint)

;; when running the debugger from shell mode, open a buffer that shows the code
;; and highlights current line
(defun my-python-pdb-shell-mode-hook ()
  (add-hook
   'comint-output-filter-functions
   'python-pdbtrack-comint-output-filter-function t))
(add-hook 'shell-mode-hook 'my-python-pdb-shell-mode-hook)

;; RST (restructured text mode)

(defun my-rst-insert-external-link ()
  "Insert a link in rst mode."
  (interactive)
  (let* ((url (read-string "Enter URL: "))
         (desc (read-string "Enter description: ")))
    (insert (format "`%s <%s>`_" desc url))))

(map!
 :mode rst-mode
 :ni "C-c C-l" 'my-rst-insert-external-link)


(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; SHELL
(defun my-switch-shell-run-last-cmd ()
  "Switch to shell buffer and run last cmd."
  (interactive)
  (let ((bname (buffer-name)))
    (if (not (string= bname "*shell*"))
        (other-window 1))
    (switch-to-buffer "*shell*")
    (goto-char (point-max))
    (comint-previous-input 1)
    (comint-send-input)
    (if (not (string= bname "*shell*"))
        (other-window -1))))

(map!
 :n "SPC o o" 'my-switch-shell-run-last-cmd)


(defun my-previous-pytest ()
  "Run previous pytest command.

  Switches to shell and runs the last pytest command from comint
  history."
  (interactive)
  (let* ((buffer-now (buffer-name))
         (buffer-is-shell (string= buffer-now "*shell*")))
    (unless buffer-is-shell
      (progn
        (other-window 1)
        (switch-to-buffer "*shell*")))
    (goto-char (point-max))
    (message (comint-previous-matching-input "pytest" 1))
    (comint-send-input)
    (unless buffer-is-shell
      (other-window -1))))

(map!
 :mode python-mode
 "C-c SPC" #'my-previous-pytest)

;; LOCALE

;; https://stackoverflow.com/a/28938491
;; because of org mode dates
(setq system-time-locale "C")


;; https://stackoverflow.com/a/42252517
(defun my-transpose-region--pop-all (list-of-lists)
  "Pop each list in the list, return list of pop results
and an indicator if some list has been exhausted."
  (loop for tail on list-of-lists collect (pop (car tail))))

(defun my-transpose-region--transpose-list-of-list (list-of-lists)
  "Transpose the matrix."
  (loop with tails = (copy-list list-of-lists)
    while (some #'consp tails) ; or any?
    collect (my-transpose-region--pop-all tails)))

(defun my-transpose-region (beg end)
  "Transpose words inside the marked region."
  ;; Very primitive, ideally would use language specific lexing and preserve
  ;; white space
  (interactive "r")
  (setq reg (buffer-substring beg end))
  (setq lines (split-string reg "\n"))
  (setq list-of-tokens (mapcar (lambda (line) (split-string line " ")) lines))
  (setq list-transposed (my-transpose-region--transpose-list-of-list list-of-tokens))
  (setq lines-transposed (mapcar (lambda (tokens) (string-join tokens " ")) list-transposed))
  (setq reg-transposed (string-join lines-transposed "\n"))
  (delete-active-region)
  (insert reg-transposed))
