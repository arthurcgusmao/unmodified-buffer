;;; unmodified-buffer --- Auto revert modified buffer state -*- lexical-binding: t -*-

;; Copyright (C) 2020  Arthur Colombini Gusmao

;; Author: Arthur Colombini Gusmao
;; Maintainer: Arthur Colombini Gusmao

;; URL: https://github.com/arthurcgusmao/unmodified-buffer
;; Version: 0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an Emacs hook to automatically revert a buffer's
;; modified state, in case the buffer has been changed back to match the
;; original content of the file it concerns.

;;; Code:

(defvar unmodified-buffer-size-limit 50000
  "Size limit (in bytes) to try to check if buffer differs from
saved file. Applies to both the saved file and the buffer: if
either is larger than the specified amount, no check will be
performed.")

(defvar unmodified-buffer-timer nil
  "Stores the latest scheduled timer to be run to check if the
buffer is in modified state.")
(make-variable-buffer-local 'unmodified-buffer-timer)

;;;###autoload
(defvar unmodified-buffer-hook nil
  "List of functions to be called when a buffer is set to unmodified.")

(defvar unmodified-buffer-ignore-remote t
  "If non-nil, `unmodified-buffer-mode' will not be activated for
remote files, for better performance.

WARNING: currently setting this variable to nil has no effect, as
the implemented solution doesn't work with remote file names (as
used by Tramp). It has to be further investigated; it might have
to do with the way we call `diff' in
`unmodified-buffer-update-flag'.")

;; ;; Useful for debugging, uncomment if necessary
;; (defvar count-run-times 0 "")
;; (setq count-run-times 0)
;; (message (concat "Times run: " (number-to-string count-run-times)))

(defun unmodified-buffer-update-flag (buffer)
  "Update the buffer modified flag if content does not differ
from respective file in disk. Requires diff to be installed on
your system. Adapted from
https://stackoverflow.com/a/11452885/5103881"
  (if (buffer-live-p buffer) ; check that buffer has not been killed
      (let ((basefile (buffer-file-name buffer)))
        (unless (not basefile) ; buffer must be associated to a file
          (let ((b-size (buffer-size buffer))
                (f-size (nth 7 (file-attributes basefile)))) ; 7th attribute of a file is its size, in bytes
            ;; (setq count-run-times (+ count-run-times 1)) ;; Debugging purposes -- Uncomment if necessary
            (unless (or (not f-size) ; sometimes the buffer can be associated to a file but the file does not exist on disk. This line covers this case.
                        (/= b-size f-size) ; buffer size should be equal to file size (much faster comparison than diffing)
                        (> b-size unmodified-buffer-size-limit) ; buffer size must be smaller than limit
                        (> f-size unmodified-buffer-size-limit)) ; file size must be smaller than limit
              (let ((tempfile (make-temp-file "buffer-content-")))
                (with-current-buffer buffer
                  (save-restriction
                    (widen)
                    (write-region (point-min) (point-max) tempfile nil 'silent))
                  ;; unless buffer diffs from file
                  (unless
                      (cond
                       ((string-equal system-type "windows-nt")
                        (/= (call-process "FC" nil nil nil "/B"
                                          (replace-regexp-in-string "/" "\\" basefile t t)
                                          (replace-regexp-in-string "/" "\\" tempfile t t)) 0))
                       ((or (string-equal system-type "darwin")
                            (string-equal system-type "gnu/linux"))
                        (/= (call-process "diff" nil nil nil "-q" basefile tempfile) 0)) ; returns 0 if files are equal, 1 if different, and 2 if invalid file paths
                       (t
                        (message "OS not supported. File a bug report or pull request.")))
                    (progn
                      (set-buffer-modified-p nil) ; set unmodified state (important emacs native flag)
                      (run-hooks 'unmodified-buffer-hook))))
                (delete-file tempfile))))))))

(defun unmodified-buffer-schedule-update (beg end len)
  "Schedules a check of the actual buffer state (if it is really
different than its corresponding file in disk). Arguments are not
used but must comply with `after-change-functions' call."
  (save-match-data ; necessary; otherwise running `replace-string' was raising error "Match data clobbered by buffer modification hooks."
    (with-current-buffer (current-buffer)
      (unmodified-buffer-cancel-scheduled-update) ; delete previously scheduled timer
      (setq unmodified-buffer-timer ; schedule new timer and save requested action to variable
            (run-at-time "0.5 sec" nil #'unmodified-buffer-update-flag (current-buffer)))))) ;; use 0.5 sec 'buffer' time

(defun unmodified-buffer-cancel-scheduled-update ()
  "Cancels the last scheduling of a check of the actual buffer
state (if it is really different than its corresponding file in
disk)."
  (if (bound-and-true-p unmodified-buffer-timer)
      (cancel-timer unmodified-buffer-timer)))

(defun unmodified-buffer-add-after-change-hook (&optional buffer)
  "Adds `unmodified-buffer-schedule-update' function to the
`after-change-functions' hook for buffers that visit a file."
  (let* ((buffer (or buffer (current-buffer)))
         (filename (buffer-file-name buffer)))
    (when filename ; Ensure buffer visits file
      (unless (and unmodified-buffer-ignore-remote ; Ensure not ignoring remote or
                   (file-remote-p filename))       ; file is not remote
        (with-current-buffer buffer
          (add-hook 'after-change-functions
                    'unmodified-buffer-schedule-update t t))))))


;;;###autoload
(define-minor-mode unmodified-buffer-mode
  "Minor mode for automatically restoring a buffer state to
unmodified if its current content matches that of the file it
visits."
  :global t
  (if unmodified-buffer-mode
      (progn
        ;; Add after-change-hook locally for buffers who visit a file
        (add-hook 'find-file-hook 'unmodified-buffer-add-after-change-hook)
        ;; No need to check status when file has been saved
        (add-hook 'after-save-hook 'unmodified-buffer-cancel-scheduled-update)
        ;; Add hook to existing buffers that visit a file
        (dolist (buffer (buffer-list))
          (unmodified-buffer-add-after-change-hook buffer))
        ;; Add hook to buffer after it was saved to a file (in case of new buffer)
        (add-hook 'after-save-hook 'unmodified-buffer-add-after-change-hook))
    (progn
      ;; Remove hooks when disabling this minor mode
      (remove-hook 'find-file-hook 'unmodified-buffer-add-after-change-hook)
      (remove-hook 'after-save-hook 'unmodified-buffer-cancel-scheduled-update)
      (remove-hook 'after-save-hook 'unmodified-buffer-add-after-change-hook)
      ;; Remove all buffer-local hooks that were posisbly created (in case the
      ;; buffer visits a file) by `unmodified-buffer-add-after-change-hook'
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (remove-hook 'after-change-functions
                       'unmodified-buffer-schedule-update t))))))


(provide 'unmodified-buffer)
;;; unmodified-buffer ends here
