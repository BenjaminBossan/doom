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
(setq! org-directory "~/work/orga/")

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

;; SEARCH

;; avy considers all visible windows
(setq! avy-all-windows nil)

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
  (key-chord-define-global "vv" #'my-escape-and-save-file))

;; EDIT
(map!
 :nv "M-'" 'comment-region)

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

;; capture
(after! org
  (add-to-list
   'org-capture-templates
     '("s" "screening"
      entry (file+headline "hiring.org" "In progress")
      "** TODO %c %^g\n- State \"TODO\"       from              %U")
     ))

;; org reveal
(add-hook! 'org-mode #'org-re-reveal)

;; org roam
(setq! org-roam-directory "~/work/orga/roam")
(map!
 :mode org-mode
 :n "SPC n r t" #'org-roam-buffer-toggle-display)

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
    (message (comint-previous-matching-input "^py.test" 1))
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
