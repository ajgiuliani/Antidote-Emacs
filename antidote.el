;;; antidote.el --- Use Druide Antidote 9 as a corrector from Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ajgiuliani

;; Author: ajgiuliani
;; URL: https://github.com/ajgiuliani/antidote9-emacs
;; Version: 0.3
;; Package-Requires: ((emacs "26.1"))
;; Keywords: wp, languages, tools

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A connector that drives Druide's Antidote 9 corrector (French/English
;; grammar & spell checker) from Emacs — the way the Word/LibreOffice/browser
;; connectors do, but via a simple, robust *file round-trip* instead of
;; Antidote's internal protocol:
;;
;;   region/buffer -> temp file
;;                 -> `Antidote9 --outil Correcteur --fichier <file>'  (its GUI)
;;                 -> you review & apply corrections, then save (Ctrl+S)
;;                 -> Emacs reads the file back and replaces the text.
;;
;; Antidote runs asynchronously, so Emacs stays responsive while its window is
;; open, and the text is updated the moment you save in Antidote — you do not
;; have to close it.  French and English both work (Antidote auto-detects the
;; language of the text).
;;
;; This is NOT Antidote: you need your own, legally-purchased and activated
;; Antidote 9 for Linux.  See the project README for how to get it running on a
;; modern distribution.
;;
;; Commands:
;;   `antidote-correct'         correct the selected region, else the whole buffer
;;   `antidote-correct-buffer'  correct the whole buffer
;;   `antidote-dictionaries'    open the dictionaries on the word at point
;;   `antidote-guides'          open the guides on the word at point
;;
;; Setup:
;;   (add-to-list 'load-path "/path/to/antidote9-emacs")
;;   (require 'antidote)
;;   ;; if Antidote9 is not on PATH, point this at your launcher:
;;   ;; (setq antidote-program "/opt/Druide/Antidote9/Application/Bin64/Antidote9")
;;   (keymap-global-set "C-c a" antidote-command-map)   ; C-c a c, C-c a d, ...

;;; Code:

(require 'seq)

(defgroup antidote nil
  "Drive the Druide Antidote 9 corrector from Emacs."
  :group 'tools
  :prefix "antidote-")

(defcustom antidote-program
  (or (executable-find "Antidote9")
      (seq-find #'file-executable-p
                '("/usr/local/bin/Antidote9"
                  "/opt/Druide/Antidote9/Application/Bin64/Antidote9")))
  "Path to the Antidote 9 launcher (the `Antidote9' wrapper script).
The default looks for `Antidote9' on `exec-path' and in the locations the
official installer uses.  Set it explicitly for a non-standard install."
  :type '(choice (const :tag "Auto-detect" nil) file))

(defcustom antidote-extra-environment
  '("QT_IM_MODULE=simple"
    "GTK_IM_MODULE=gtk-im-context-simple"
    "XMODIFIERS=@im=none"
    "IBUS_DISABLE_SNOOPER=1")
  "Extra environment variables to set when launching Antidote.
The defaults disable IBus, which otherwise freezes text fields in
Antidote's Qt 5.6 GUI on modern desktops."
  :type '(repeat string))

(defcustom antidote-coding-system 'utf-8-unix
  "Coding system used for the temp file exchanged with Antidote."
  :type 'symbol)

(defcustom antidote-poll-interval 0.7
  "Seconds between checks for Antidote having saved the exchange file.
Lower = snappier buffer updates while Antidote is open."
  :type 'number)

(defun antidote--program ()
  "Return the Antidote launcher, or signal a clear error if unset/missing."
  (or (and antidote-program (file-executable-p antidote-program) antidote-program)
      (user-error
       "Antidote launcher not found; set `antidote-program' to your Antidote9 path")))

(defun antidote--env ()
  "Return `process-environment' with `antidote-extra-environment' prepended."
  (append antidote-extra-environment process-environment))

(defun antidote--correct-region (beg end)
  "Send BEG..END to Antidote's corrector and update it in place as you save.
This is the engine behind the interactive correction commands.  Antidote
opens asynchronously and Emacs stays responsive; whenever you save in
Antidote (Ctrl+S, or by closing and accepting the save prompt) the corrected
text replaces BEG..END.  You do not have to close Antidote for the update."
  (let* ((program (antidote--program))
         (buf     (current-buffer))
         (mbeg    (copy-marker beg))
         (mend    (copy-marker end t))          ; advances with inserted text
         (tmp     (make-temp-file "antidote-" nil ".txt"))
         (coding  antidote-coding-system)
         (mtime   nil)
         (timer   nil)
         (pull    nil)
         (cleanup nil))
    (let ((coding-system-for-write coding))
      (write-region beg end tmp nil 'silent))
    (setq mtime (file-attribute-modification-time (file-attributes tmp)))
    (setq pull
          (lambda ()
            (when (and (buffer-live-p buf) (file-exists-p tmp))
              (let ((mt (file-attribute-modification-time (file-attributes tmp))))
                (unless (equal mt mtime)          ; only when Antidote has saved
                  (setq mtime mt)
                  (let* ((cur (with-current-buffer buf
                                (buffer-substring-no-properties mbeg mend)))
                         (raw (with-temp-buffer
                                (let ((coding-system-for-read coding))
                                  (insert-file-contents tmp))
                                (buffer-string)))
                         ;; Antidote tends to append a trailing newline on save;
                         ;; drop one if the text did not have it.
                         (new (if (and (string-suffix-p "\n" raw)
                                       (not (string-suffix-p "\n" cur)))
                                  (substring raw 0 -1) raw)))
                    (unless (string= new cur)
                      (with-current-buffer buf
                        (save-excursion
                          (goto-char mbeg)
                          (delete-region mbeg mend)
                          (insert new)))
                      (message "Antidote: text updated"))))))))
    (setq cleanup
          (lambda ()
            (when (timerp timer) (cancel-timer timer) (setq timer nil))
            (ignore-errors (delete-file tmp))
            (when (markerp mbeg) (set-marker mbeg nil))
            (when (markerp mend) (set-marker mend nil))))
    (let* ((process-environment (antidote--env))
           (proc (start-process "antidote" nil program
                                "--outil" "Correcteur" "--fichier" tmp)))
      (setq timer (run-with-timer antidote-poll-interval antidote-poll-interval pull))
      (set-process-sentinel
       proc
       (lambda (_proc event)
         (when (string-match-p "\\`\\(finished\\|exited\\|killed\\|deleted\\)" event)
           (funcall pull)                          ; catch a final save-on-close
           (funcall cleanup)))))
    (message "Antidote: review & apply, then save (Ctrl+S) — Emacs updates automatically")))

;;;###autoload
(defun antidote-correct ()
  "Correct text with Antidote.
If a region is active, correct the region; otherwise correct the whole
buffer.  The corrected text is written back in place when you save in
Antidote."
  (interactive)
  (if (use-region-p)
      (antidote--correct-region (region-beginning) (region-end))
    (antidote--correct-region (point-min) (point-max))))

;;;###autoload
(defun antidote-correct-buffer ()
  "Correct the whole buffer with Antidote."
  (interactive)
  (antidote--correct-region (point-min) (point-max)))

(defun antidote--open-tool (tool)
  "Open Antidote TOOL (one of Antidote's `--outil' values) on the word at point."
  (let* ((program (antidote--program))
         (word (or (thing-at-point 'word t) ""))
         (process-environment (antidote--env)))
    (apply #'start-process "antidote" nil program
           "--outil" tool
           (if (string-empty-p word) nil (list "--chaine" word)))))

;;;###autoload
(defun antidote-dictionaries ()
  "Open Antidote's dictionaries on the word at point."
  (interactive)
  (antidote--open-tool "Dictionnaires"))

;;;###autoload
(defun antidote-guides ()
  "Open Antidote's guides on the word at point."
  (interactive)
  (antidote--open-tool "Guides"))

;;;###autoload (autoload 'antidote-command-map "antidote" nil t 'keymap)
(defvar antidote-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'antidote-correct)
    (define-key map "b" #'antidote-correct-buffer)
    (define-key map "d" #'antidote-dictionaries)
    (define-key map "g" #'antidote-guides)
    map)
  "Keymap of Antidote commands, meant to be bound to a prefix key.

Bind it once, e.g.:

    (keymap-global-set \"C-c a\" antidote-command-map)

then \\`C-c a c\\' corrects (region or buffer), \\`C-c a b\\' corrects the
whole buffer, \\`C-c a d\\' opens the dictionaries and \\`C-c a g\\' the
guides on the word at point.")
(fset 'antidote-command-map antidote-command-map)

(provide 'antidote)
;;; antidote.el ends here
