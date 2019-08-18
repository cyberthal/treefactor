;;; trs.el --- Move text & files thru a directory tree -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Leo Buchignani III

;; Author: Leo Buchignani III <texas.cyberthal@gmail.com>
;; Keywords: outlines, files, convenience
;; Package-Requires: ((emacs "24.3") (dash "2.16.0") (f "0.20.0"))
;; URL: https://github.com/cyberthal/treesort
;; Version: 1.0.4

;;; Commentary:

;; Treesort rapidly sorts text and files into outlines and the directory tree.

;; Treesort's main command is trs-throw. It moves text or files from the current
;; window to a target in the next window. A second function, trs-throw-up, moves
;; text or files up one directory level. You can throw directories as well as
;; files.

;; When you throw a file to a directory, trs-throw creates a child directory
;; <target-directory>/0-Inbox/ and puts the file there. This makes it easy to
;; remember which files are new arrivals.

;; When you throw text to a directory, trs-throw creates a file Inbox.org. Many
;; such files are created during a filing session. To quickly delete them, use
;; trs-delete-this-buffer-and-file.

;; Treesort can rapidly change the directory tree structure of your notes. It
;; helps to have some links that won't break when paths change. Use
;; trs-dired-zinks to create a file with an org-id link in it.

;; trs-throw can throw text into existing files or outlines. You can duplicate a
;; heading to another text window with trs-duplicate-heading-to-other-window.

;; When you throw text to an outline, trs-throw believes that the parent heading
;; is at the top of the visible region. It will only throw to direct children of
;; the parent. You should narrow appropriately before throwing.

;; When you throw text to a file, trs-throw puts the text at the bottom. EXCEPT
;; when the file already has a level-1 heading. Then trs-throw assumes this is a
;; polished document, not an inbox file. trs-throw worries you will forget about
;; text appended to polished documents. So it prepends the text before the
;; level-1 headline, where it will stick out like a sore thumb.

;; trs-throw assumes that most headings you file will have four stars. Why?
;; Imagine you are throwing headings to an outline. The level-1 heading is the
;; document title. The level-2 headings are categories. The level-3 headings are
;; sub-categories. The level-4 headings are topics. Outlines become unwieldy
;; when they get too deep, at which point it's better to create more files and
;; directories to spread the load.

;; trs-throw only imposes this opinion on you in one way: it creates Inbox.org
;; files with a "*** offset" at the top. You can still file level-5 headings,
;; but they might "vanish" if you file a level-4 heading that unintentionally folds
;; appended level-5 headings beneath it. You can also file level-3 headings,
;; although they won't be children of the "offset" heading, and might
;; unexpectedly fold appended level-4 headings. I recommend that you convert
;; headings to level 4 for transport, and then resize them at their destination.

;; The last text thrown is saved in the variable trs-object-text until the Emacs
;; session ends. Text is not saved to the kill ring. A message appears in the
;; minibuffer displaying the destination of text and files moved with ts-throw
;; commands.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done. (This package is not yet on MELPA.)

;;;;; Manual

;; Put this file in your load-path, and put this in your init
;; file: trs.el

;; (require 'trs)

;;;; Usage

;; Run one of these commands:

;; `trs-throw' throw text/files to the next window
;; `trs-throw-up' throw text/files one directory up
;; `trs-delete-this-buffer-and-file' self-explanatory
;; `trs-store-link-fold-drawer' store an org link and hide the drawer
;; `trs-dired-zinks' store an org link in a file, titled with relative path
;; `trs-duplicate-heading-to-other-window' self-explanatory

;;;; Tips

;; Use org-id for global link IDs that are not path-dependent.

;; Treesort encourages many org files in deeply nested directories. This can
;; make it challenging to construct an org-agenda files list. See here to load
;; org agenda files recursively:
;; https://stackoverflow.com/questions/17215868/recursively-adding-org-files-in-a-top-level-directory-for-org-agenda-files-take

;; It also helps to have a function that refreshes your org agenda file list, if
;; you've altered paths in that directory.

;; I recommend configuring Dired to sort directories before files. Where
;; possible, capitalize files and directories. This makes it easy to target them
;; with isearch in a few keystrokes. Omit extensions to reduce visual clutter.

;; Treesort filing is fast. Think with your fingers, not your brain. You can
;; always redo it later.

;;; My keybindings

;; By putting the following commands on convenient keys, you can file without thinking about it.

;; (global-set-key (kbd "H-f") 'trs-throw)
;; (global-set-key (kbd "H-g") 'trs-throw-up)
;; (global-set-key (kbd "C-c k") 'trs-delete-this-buffer-and-file)
;; (global-set-key (kbd "C-c l") 'trs-store-link-fold-drawer)
;; (global-set-key (kbd "H-a") 'other-window)
;; (global-set-key (kbd "H-w") 'outline-up-heading)
;; (global-set-key (kbd "H-e") 'outline-previous-visible-heading)
;; (global-set-key (kbd "H-r") 'outline-next-visible-heading)
;; (global-set-key (kbd "H-d") 'org-narrow-to-subtree)
;; (global-set-key (kbd "H-s") 'widen)
;; (global-set-key (kbd "H-1") 'spacemacs/toggle-maximize-buffer)
;; (global-set-key (kbd "H-2") 'delete-window)
;; (global-set-key (kbd "H-3") 'split-window-right)
;; (global-set-key (kbd "s-i") 'ido-dired)

;;; Comments and whitespace

;; Treesort departs from
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html
;; by placing close-parentheses on lines by themselves where it enhances
;; readability. I find it much easier to keep track of parenthetical nesting
;; with indentation than only by counting and font color. Ruby does something
;; similar by placing end-braces on lines by themselves when they embrace
;; multiple lines.

;; Treesort uses an Outshine-style outline to structure its code. Outline
;; navigation, narrowing and folding negates the disadvantage of using extra
;; lines to show end-parentheses indentation. Outshine supports todo tags. There
;; are two level-1 headings at the beginning of the code due to Outshine's
;; visibility-cycling behavior.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License

;;; Code:

;; * trs.el
;; * offset
;; ** require

(require 'org)
(require 'dired)
(require 'dash)
(require 'f)
;; ** throw

;; *** config

;; **** don't search invisible text in dired

(defun trs-dired-dont-search-invisible ()
  (make-local-variable 'search-invisible)
  (setq search-invisible nil)
  )
(add-hook 'dired-mode-hook 'trs-dired-dont-search-invisible)

;; **** define variables and declare functions

(defvar trs-object-text nil
  "Stores the last text treesort killed or copied.")

(defvar user-home-directory) ; Spacemacs variable

(declare-function outshine-narrow-to-subtree "outshine" ())

;; *** main defun

;;;###autoload
(defun trs-throw (&optional count)
  "Throw text/file to target in next window COUNT times."
  (interactive "p")

  (dotimes (var count)

    (unwind-protect
        (if (eq major-mode 'dired-mode)
            (trs-throw-file)
          (trs-throw-text))
      (other-window -1) ; save-selected-window fails for throw-text
      (message "Threw %s times." var)
      )
    )
  )
;; *** flow control dispatcher

;; **** main defun

(defun trs-throw-text ()
  "Throw text to either Dired or an outline."

  (select-window (next-window))
  (let ((trs-in-dired-p (string-equal major-mode 'dired-mode)))
    (select-window (previous-window))

    (if trs-in-dired-p
        (trs-throw-text-to-dired)
      (call-interactively 'trs-throw-text-to-outline)
      )
    )
  )
;; **** throw file

(defun trs-throw-file ()
  "Throw file(s) from Dired to searched target in next window."

  (select-window (next-window))
  (trs-search-dired-open)
  (mkdir (concat default-directory "0-Inbox/") 1)
  (find-file (concat default-directory "0-Inbox/"))
  (select-window (previous-window))
  (dired-do-rename)

  (select-window (next-window))
  (dired-up-directory) ; restores original dired buffer.
  (dired-up-directory) ; necessary because save-current-buffer won't restore
                       ; after dired-do-rename.
  (forward-char 1)
  )
;; **** throw text
;; ***** destination = dired

;; ****** main defun

(defun trs-throw-text-to-dired ()
  "Throw text to a searched target in an adjacent Dired buffer."

  (select-window (next-window))
  (let ((trs-dired-starting-buffer (current-buffer))
        )
    (trs-search-dired-open)
    (select-window (previous-window))
    (trs-snort-text)
    (select-window (next-window))
    (if buffer-file-name
        (trs-insert-text-to-file-blind)
      (trs-insert-text-to-directory)
      )
    (switch-to-buffer trs-dired-starting-buffer) ; Save-current-buffer bugged,
                                        ; must use instead.
    (forward-char 1)
    )
  )
;; ****** destination = dir

(defun trs-insert-text-to-directory ()
  "Insert `trs-object-text' to Inbox.org."

    (trs-create-open-inbox-org)
    (trs-insert-to-end-of-buffer)
    (trs-text-inserted-to-buffer-path-message)
  )
;; ****** destination = file

(defun trs-insert-text-to-file-blind ()
  "Put point either before first level-1 heading or at end of buffer.
Normally one wants to yank to the end of the buffer.
But if it's a polished document rather than an inbox,
then one wants the new text at the top, where its more noticeable.
Function assumes a polished document will have a level-1 near the top."

  (goto-char (point-min))
  (condition-case nil
      (progn
        (re-search-forward "^* ") ; Search for a level-1 headline.
        (goto-char (point-at-bol))
        (insert trs-object-text)
        )
    (error (trs-insert-to-end-of-buffer))
    )
  (trs-text-inserted-to-buffer-path-message)
  )
;; ***** destination = text
;; ****** main defun

(defun trs-throw-text-to-outline (PREFIX)
  "Append text to next window's heading beginning with PREFIX.
Assumes parent heading is at the top of the visible region.

Prompts user for input. Asks for enough letters from the
beginning of the target child heading to distinguish it from the
other immediate children of the parent heading. Searches for a
simple string. Takes the first match.

If no match found, fails with an error, and does not delete the line."

  (interactive "sEnter target heading's unique prefix: ")

  (save-selected-window
    (select-window (next-window))

    ;; find searched heading
    (goto-char (point-min))
    (search-forward
     (concat "\n"
             (make-string (1+ (skip-chars-forward "*")) ?*)
             " "
             PREFIX)
     )

    (unless (outline-on-heading-p)
      (user-error "%s" "Search did not find a valid heading"))

    (org-save-outline-visibility 1 ; argument necessary, else heading after body
                                   ; text unfolds body text
      (save-restriction
        (org-narrow-to-subtree)
        (trs-region-ends-n-newlines 1)

        (save-selected-window (select-window (previous-window))
                              (trs-snort-text))

        (insert trs-object-text)
        )
      )
    )
  )
;; *** throw up
;; **** main defun

;;;###autoload
(defun trs-throw-up (&optional count)
  "Throw file or text one directory upwards, COUNT times."
  (interactive "p")

  (dotimes (var count)

    (if (eq major-mode 'dired-mode)
        (trs-throw-up-file)
      (trs-throw-up-text))
    (message "Threw up %s times" var)
    )
  )
;; **** jump height

(defun trs-jump-destination ()
  "Return directory 1-2 above current, depending on ../0-Inbox."

  (concat default-directory

          ;; "Returns ../ unless parent dir is 0-inbox, then ../../"
          (if (trs-parent-dir-inbox-p)
              "../../"
            "../")
          )
  )
;; **** object = text

(defun trs-throw-up-text ()
  "Throw text upwards in the directory tree to the next /0-Inbox."

  (let ((trs-buffer-home (current-buffer))
        (trs-text-object (trs-snort-text))
        (default-directory (trs-jump-destination))
        )
    (trs-create-open-inbox-org)
    (trs-insert-text-to-file-blind)
    (switch-to-buffer trs-buffer-home) ; because save-current-buffer failed here
    )
  )
;; **** target = file

(defun trs-throw-up-file ()
  "Throw file upwards in the directory tree to the next /0-Inbox."

  (let* ((trs-jump-destination (trs-jump-destination))
         (trs-inbox-dir (concat trs-jump-destination "0-Inbox/"))
         )
    (if (file-exists-p trs-inbox-dir)
        ()
      (mkdir trs-inbox-dir)
      )
    (rename-file (dired-get-filename "no-dir") trs-inbox-dir)
    (message "File thrown to %s" trs-jump-destination)
    )
  (revert-buffer) ; refreshes screen significantly faster than otherwise.
  )
;; *** library
;; **** snort type
;; ***** text mode?

(defun trs-snort-text ()
  "If heading or line of text to trs-snort-line variable."
  (cond ((eq major-mode 'org-mode) (trs-snort-text-org))
        ((-contains-p minor-mode-list 'outshine-mode) (trs-snort-text-outshine))
        ((-contains-p minor-mode-list 'outline-minor-mode) (trs-snort-text-outline))
        (t (trs-snort-line))
        )
  )
;; ***** at heading?

(defun trs-snort-text-org ()
     (if (org-at-heading-p) (trs-snort-org-heading)
            (trs-snort-line))
     )
(defun trs-snort-text-outshine ()
     (if (outline-on-heading-p) (trs-snort-outshine-heading)
            (trs-snort-line))
     )
(defun trs-snort-text-outline ()
     (if (outline-on-heading-p) (trs-snort-outline-heading)
            (trs-snort-line))
     )
;; ***** heading type?

(defun trs-snort-org-heading ()
  (save-restriction
    (org-narrow-to-subtree)
    (trs-snort-visible)
    )
  )
(defun trs-snort-outshine-heading ()
  (save-restriction
    (outshine-narrow-to-subtree)
    (trs-snort-visible)
    )
  )
(defun trs-snort-outline-heading ()
  (save-restriction
    (org-narrow-to-subtree)
    (trs-snort-visible)
    )
  )
;; ***** line

(defun trs-snort-line ()
  "Move a line of text to variable `trs-object-text'."

  (if (eq (point-min) (point-max))
      (user-error "%s" "Selected line is empty")
    (setq trs-object-text
          (concat (delete-and-extract-region (line-beginning-position) (line-end-position))
                  "\n"
                  )
          )
    (trs-delete-leftover-empty-line)
    )
  )
;; **** files
;; ***** Find the searched dired entry

(defun trs-search-dired-open ()
  "Opens the isearched Dired entry."

  (if (string-equal major-mode "dired-mode")
      nil
      (user-error "%s" "Mode must be Dired"))

  (goto-char (point-min))
  (forward-line)
  (dired-hide-details-mode)
  (isearch-forward)
  (dired-find-file)
  )
;; ***** check whether immediate parent dir is "0-Inbox"

(defun trs-parent-dir-inbox-p ()
  "Return t if parent dir is 0-Inbox."

  (equal
   (file-name-nondirectory (directory-file-name default-directory)) ; Return parent directory.
   "0-Inbox")
  )
;; ***** Inbox.org creation
;; ****** Create open Inbox.org

(defun trs-create-open-inbox-org ()
  "If no Inbox.org, make it and insert *** offset."

  (let* ((trs-inbox-org-path (concat default-directory "Inbox.org"))
         (trs-inbox-org-buffer (find-buffer-visiting trs-inbox-org-path)))

    (cond (trs-inbox-org-buffer (set-buffer trs-inbox-org-buffer)) ; select buffer if exists
          ((file-exists-p trs-inbox-org-path) (find-file trs-inbox-org-path)) ; open file if exists
          ;; else create and open file
          (t (progn (f-touch "Inbox.org")
                    (find-file trs-inbox-org-path)
                    (insert "*** offset\n\n")
                    )
             )
          )
    )
  )
;; ** utilities
;; *** trs-delete-this-buffer-and-file

;;;###autoload
(defun trs-delete-this-buffer-and-file ()
  "Delete file visited by current buffer and kill buffer."
  (interactive)

  (let ((filename (buffer-file-name))
        )
    (if (buffer-narrowed-p)
        (user-error "%s" "Buffer is narrowed")
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (kill-buffer (current-buffer))
        (delete-file filename)
        (message "File `%s' successfully removed" filename)
        )
      )
    )
  )
;; *** org links
;; **** Store link and fold the PROPERTIES drawer

;;;###autoload
(defun trs-store-link-fold-drawer ()
  "Store an org link to a heading, and fold the drawer."
  (interactive)

  (save-excursion
    (save-restriction
      (org-store-link nil t) ; Without interactive=t arg, no org link gets created.
      (org-narrow-to-subtree)
      (org-previous-visible-heading 1)
      (widen)
      (forward-line)
      )
    (org-cycle-hide-drawers 1)
    )
  )
;; **** create Zinks.org

;;;###autoload
(defun trs-dired-zinks ()
  "Make Zinks.org.  Insert org-id link.

Link title's path is relative to `vc-root-dir' if present,
else `user-home-directory'."
  (interactive)

  (let ((zinks-filename (concat default-directory "Zinks.org"))
        )
    (if (file-exists-p zinks-filename)
        (user-error "%s" "Zinks.org already exists")
      (find-file zinks-filename)
      (insert (concat "*** "
                      (file-relative-name (file-name-directory buffer-file-name)
                                        (cond ((vc-root-dir) (vc-root-dir))
                                              (user-home-directory user-home-directory) ; Spacemacs variable. If missing, no problem.
                                              )
                                        )
                      "\n\n\n"
                      )
              )
      (trs-store-link-fold-drawer)
      (goto-char (point-max))
      )
    )
  )
;; *** duplicate heading to other window

;;;###autoload
(defun trs-duplicate-heading-to-other-window ()
  "Append heading at point to end of next window's buffer."
  (interactive)

  (save-restriction
    (org-narrow-to-subtree)
    (trs-region-ends-n-newlines 1)
    (let ((home-buffer (current-buffer))
          )
      (save-selected-window
        (select-window (next-window))
        (trs-region-ends-n-newlines 2)
        (insert-buffer-substring home-buffer)
        )
      )
    )
  )
;; ** library

;; *** heading ends n newlines

(defun trs-region-ends-n-newlines (n)
  "Make region end in N newlines. Set point to end of region."

  (if (>= n 0)
      ()
    (user-error "N is too small: %s" n)
    )

  (let ((m (- n 1)))
    (goto-char (point-max))
    (if (bolp)
        (if (= n 0)
            (progn (org-N-empty-lines-before-current n)
                   (delete-char -1))
          (org-N-empty-lines-before-current m)
          )
      (insert (make-string n ?\n))
      )
    )
  (goto-char (point-max))
  )
;; *** snort visible region

(defun trs-snort-visible ()
  "Move region to `trs-object-text'.  Widen.  Delete empty line."

  (trs-region-ends-n-newlines 1)
  (setq trs-object-text (delete-and-extract-region (point-min) (point-max)))
  (widen)
  (trs-delete-leftover-empty-line)
  )
;; *** safely delete empty line

(defun trs-delete-leftover-empty-line ()
  "Deletes empty line at point, if there is one."

  (unless (and (bobp) (eobp))
    (if (bobp)
        (delete-char 1)
      (when
          (org--line-empty-p 1) ; (not my) bug: This wrongly returns nil when point is on an empty line at top of buffer.  Hence the workaround.
        (delete-char -1)
        (unless (eobp) (forward-char 1))
        )
      )
    )
  )
;; *** insert at bottom of buffer

(defun trs-insert-to-end-of-buffer ()
  "Add `trs-object-text' text to bottom of target buffer."

  (widen)
  (trs-region-ends-n-newlines 2)
  (insert trs-object-text)
  )
;; *** text inserted confirmation message

(defun trs-text-inserted-to-buffer-path-message ()
  "Report filename that text was inserted to.

Reported path is relative to vd-root-dir or ~/."

  (message "Inserted text into `%s'" (if (vc-root-dir)
                                         (expand-file-name buffer-file-name (vc-root-dir))
                                       (expand-file-name buffer-file-name user-home-directory)
                                       )
           )
  )
;; ** provide

(provide 'trs)
;;; trs.el ends here
