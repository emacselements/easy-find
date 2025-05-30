;;; easy-find.el --- Simple file searching like Nemo -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Created: 2025-05-22
;; URL: https://gitlab.com/gnarledgrip/easy-find
;; Package-Requires: ((emacs "24.1"))
;; Keywords: files, convenience, tools

;; This file is part of GNU Emacs.
;; 
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

;; Easy-find provides simple file searching functionality similar to Nemo file manager.
;; It allows searching for files using pipe-delimited patterns with case-sensitive
;; or case-insensitive matching. Results are displayed in Dired with details hidden.
;; 
;; Main functions:
;; - easy-find: Interactive file search with custom patterns
;; - Predefined searches for common file types (videos, images, documents, etc.)

;;; Code:

(defun easy-find-convert-pattern (pattern &optional case-sensitive)
  "Convert a pipe-delimited file PATTERN to find arguments.
If CASE-SENSITIVE is nil, use case-insensitive matching."
  (let* ((patterns (split-string pattern "|" t "[ \t\n]+"))
         (patterns (mapcar 'string-trim patterns))
         (name-command (if case-sensitive "-name" "-iname"))
         (find-args (concat "-type f \\( "
                           (mapconcat (lambda (pat)
                                       (format "%s \"%s\"" name-command pat))
                                     patterns
                                     " -o ")
                           " \\)")))
    find-args))

(defun easy-find-hide-details ()
  "Force hide details in the current Dired buffer."
  (when (derived-mode-p 'dired-mode)
    (if (fboundp 'dired-hide-details-mode)
        (dired-hide-details-mode 1)
      (message "dired-hide-details-mode not available"))))

(defun easy-find (directory pattern &optional case-sensitive)
  "Search for files using pipe-delimited PATTERN in DIRECTORY.
Only matches files, not directories.
If CASE-SENSITIVE is nil, perform case-insensitive search."
  (interactive
   (list (read-directory-name "Directory: " nil default-directory t)
         (read-string "File pattern (use | as separator): ")
         (y-or-n-p "Case sensitive? ")))
  (let ((find-arguments (easy-find-convert-pattern pattern case-sensitive)))
    ;; Run the find command
    (find-dired directory find-arguments)
    ;; Setup to hide details after find finishes
    (with-current-buffer "*Find*"
      ;; Add a one-time hook to hide details when this specific find process finishes
      (let ((proc (get-buffer-process (current-buffer))))
        (when proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (easy-find-hide-details))))))))))

(defun easy-find-videos (directory)
  "Find video files in DIRECTORY."
  (interactive "DFind videos in directory: ")
  (easy-find directory "*.mp4|*.mkv|*.m4v|*.avi|*.mov|*.flv|*.wmv|*.webm|*.vid|*.mpg|*.mpeg|*.asf|*.f4v"))

(defun easy-find-images (directory)
  "Find image files in DIRECTORY."
  (interactive "DFind images in directory: ")
  (easy-find directory "*.gif|*.jpeg|*.jpg|*.png|*.tif|*.tiff|*.webp|*.svg"))

(defun easy-find-documents (directory)
  "Find document files in DIRECTORY."
  (interactive "DFind documents in directory: ")
  (easy-find directory "*.doc|*.docx|*.el|*.md|*.odp|*.odt|*.ods|*.org|*.pdf|*.ppt|*.pptx|*.xlsx"))

(defun easy-find-text (directory)
  "Find text files in DIRECTORY."
  (interactive "DFind text files in directory: ")
  (easy-find directory "*.md|*.org|*.txt"))

(defun easy-find-org (directory)
  "Find org files in DIRECTORY."
  (interactive "DFind org files in directory: ")
  (easy-find directory "*.org"))

(defun easy-find-pdf (directory)
  "Find PDF files in DIRECTORY."
  (interactive "DFind pdf files in directory: ")
  (easy-find directory "*.pdf"))

(defun easy-find-txt (directory)
  "Find txt files in DIRECTORY."
  (interactive "DFind txt files in directory: ")
  (easy-find directory "*.txt"))

(defun easy-find-md (directory)
  "Find markdown files in DIRECTORY."
  (interactive "DFind markdown files in directory: ")
  (easy-find directory "*.md"))

(defun easy-find-elc (directory)
  "Find compiled Emacs Lisp files in DIRECTORY."
  (interactive "DFind compiled elisp files in directory: ")
  (easy-find directory "*.elc"))

(defun easy-find-backups (directory)
  "Find Emacs backup and autosave files in DIRECTORY."
  (interactive "DFind backup files in directory: ")
  (easy-find directory "*~|#*#"))

(defun easy-find-tilde (directory)
  "Find Emacs backup files in DIRECTORY."
  (interactive "DFind tilde files in directory: ")
  (easy-find directory "*~"))

(defun easy-find-hash (directory)
  "Find Emacs autosave files in DIRECTORY."
  (interactive "DFind hash files in directory: ")
  (easy-find directory "#*#"))

(defun easy-find-audio (directory)
  "Find audio files in DIRECTORY."
  (interactive "DFind audio files in directory: ")
  (easy-find directory "*.aac|*.flac|*.m4a|*.mp3|*.ogg|*.wav|*.wma"))

(defun easy-find-compressed (directory)
  "Find compressed files in DIRECTORY."
  (interactive "DFind compressed files in directory: ")
  (easy-find directory "*.7z|*.bz2|*.gz|*.par2|*.rar|*.tar|*.tbz2|*.tgz|*.xz|*.zip"))

(defun easy-find-cleanup (directory)
  "Find files to clean up in DIRECTORY."
  (interactive "DFind cleanup files in directory: ")
  (easy-find directory "*.deb|*.run|*.docx|*.zip|*.diz|*.webp|*.url|*.gif|*.encr|*.rm|*.mp3|*.html|*.htm|*.pdf|*.png|*.jpe?g|*.m3u|*.nfo|*.exe|*.log|*.doc|*.rar|*.nzb|*.par|*.par2|*.sfv|*.srr|*.txt|*.dat|*.xml|*.r[0-9][0-9]" nil))

(provide 'easy-find)

;;; easy-find.el ends here
