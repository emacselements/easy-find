;;; easy-find.el --- Simple file searching like Nemo -*- lexical-binding: t -*-

;; Author: Raoul Comninos
;; Version: 1.0
;; Created: 2025-05-22
;; URL: https://github.com/emacselements/easy-find
;; Package-Requires: ((emacs "24.4"))
;; Keywords: files, convenience, tools

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Easy-find provides simple file searching functionality similar to Nemo file manager.
;; It allows searching for files using pipe-delimited patterns with case-insensitive
;; matching by default. Results are displayed in Dired with details hidden.
;;
;; Main functions:
;; - easy-find: Interactive file search with custom patterns (case-insensitive by default)
;; - easy-find-big: Find files larger than a specified size (default 100MB)
;; - easy-find-huge: Find exceptionally large files (default 1GB)
;; - Predefined searches for common file types (videos, images, documents, etc.)

;;; Code:

(eval-when-compile (require 'subr-x))

;;;###autoload
(defun easy-find-convert-pattern (pattern &optional case-sensitive)
  "Convert a pipe-delimited file PATTERN to find arguments.
If CASE-SENSITIVE is nil, use case-insensitive matching."
  (let* ((patterns (split-string pattern "|" t "[ \t\n]+"))
         (patterns (mapcar #'string-trim patterns))
         (name-command (if case-sensitive "-name" "-iname"))
         (find-args (concat "-type f \\( "
                           (mapconcat (lambda (pat)
                                       (format "%s \"%s\"" name-command pat))
                                     patterns
                                     " -o ")
                           " \\)")))
    find-args))

;;;###autoload
(defun easy-find-hide-details ()
  "Force hide details in the current Dired buffer."
  (when (derived-mode-p 'dired-mode)
    (if (fboundp 'dired-hide-details-mode)
        (dired-hide-details-mode 1)
      (message "dired-hide-details-mode not available"))))

;;;###autoload
(defun easy-find (directory pattern &optional case-sensitive)
  "Search for files using pipe-delimited PATTERN in DIRECTORY.
Only matches files, not directories.
By default, search is case-insensitive.
With prefix argument (C-u), perform case-sensitive search."
  (interactive
   (list (read-directory-name "Directory: " nil default-directory t)
         (read-string "File pattern (use | as separator): ")
         current-prefix-arg))
  (let ((find-arguments (easy-find-convert-pattern pattern case-sensitive)))
    ;; Show initial message
    (message "Searching for files in %s..." directory)
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
                 (easy-find-hide-details)
                 (let ((file-count (- (count-lines (point-min) (point-max)) 2)))
                   (when (< file-count 0) (setq file-count 0))
                   (message "Search complete. Found %d file%s."
                            file-count
                            (if (= file-count 1) "" "s"))))))))))))

;;;###autoload
(defun easy-find-videos (directory)
  "Find video files in DIRECTORY."
  (interactive "DFind videos in directory: ")
  (easy-find directory "*.mp4|*.mkv|*.m4v|*.avi|*.mov|*.flv|*.wmv|*.webm|*.vid|*.mpg|*.mpeg|*.asf|*.f4v"))

;;;###autoload
(defun easy-find-images (directory)
  "Find image files in DIRECTORY."
  (interactive "DFind images in directory: ")
  (easy-find directory "*.gif|*.jpeg|*.jpg|*.png|*.tif|*.tiff|*.webp|*.svg"))

;;;###autoload
(defun easy-find-thumbnails (directory)
  "Find actual thumbnail cache files in DIRECTORY.
Searches .thumbnails cache directories and common thumbnail locations."
  (interactive "DFind thumbnails in directory: ")
  (let* ((find-arguments "-type f \\( -path '*/.thumbnails/*' -o -path '*/.cache/thumbnails/*' -o -path '*/thumbs.db' -o -path '*/Thumbs.db' \\)"))
    (message "Searching for thumbnail files in %s..." directory)
    (find-dired directory find-arguments)
    (with-current-buffer "*Find*"
      (let ((proc (get-buffer-process (current-buffer))))
        (when proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (easy-find-hide-details)
                 (let ((file-count (- (count-lines (point-min) (point-max)) 2)))
                   (when (< file-count 0) (setq file-count 0))
                   (message "Search complete. Found %d thumbnail file%s."
                            file-count
                            (if (= file-count 1) "" "s"))))))))))))

;;;###autoload
(defun easy-find-documents (directory)
  "Find document files in DIRECTORY."
  (interactive "DFind documents in directory: ")
  (easy-find directory "*.doc|*.docx|*.el|*.md|*.odp|*.odt|*.ods|*.org|*.pdf|*.ppt|*.pptx|*.xlsx"))

;;;###autoload
(defun easy-find-text (directory)
  "Find text files in DIRECTORY."
  (interactive "DFind text files in directory: ")
  (easy-find directory "*.md|*.org|*.txt"))

;;;###autoload
(defun easy-find-scripts (directory)
  "Find executable script files in DIRECTORY."
  (interactive "DFind script files in directory: ")
  (let* ((find-arguments "-type f -executable \\( -iname '*.sh' -o -iname '*.bash' -o -iname '*.zsh' -o -iname '*.py' -o -iname '*.pl' -o -iname '*.rb' -o -iname '*.lua' -o -iname '*.js' -o -iname '*.php' \\)"))
    (message "Searching for executable script files in %s..." directory)
    (find-dired directory find-arguments)
    (with-current-buffer "*Find*"
      (let ((proc (get-buffer-process (current-buffer))))
        (when proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (easy-find-hide-details)
                 (let ((file-count (- (count-lines (point-min) (point-max)) 2)))
                   (when (< file-count 0) (setq file-count 0))
                   (message "Search complete. Found %d executable script%s."
                            file-count
                            (if (= file-count 1) "" "s"))))))))))))

;;;###autoload
(defun easy-find-org (directory)
  "Find org files in DIRECTORY."
  (interactive "DFind org files in directory: ")
  (easy-find directory "*.org"))

;;;###autoload
(defun easy-find-pdf (directory)
  "Find PDF files in DIRECTORY."
  (interactive "DFind pdf files in directory: ")
  (easy-find directory "*.pdf"))

;;;###autoload
(defun easy-find-txt (directory)
  "Find txt files in DIRECTORY."
  (interactive "DFind txt files in directory: ")
  (easy-find directory "*.txt"))

;;;###autoload
(defun easy-find-md (directory)
  "Find markdown files in DIRECTORY."
  (interactive "DFind markdown files in directory: ")
  (easy-find directory "*.md"))

;;;###autoload
(defun easy-find-elc (directory)
  "Find compiled Emacs Lisp files in DIRECTORY."
  (interactive "DFind compiled elisp files in directory: ")
  (easy-find directory "*.elc"))

;;;###autoload
(defun easy-find-backups (directory)
  "Find Emacs backup and autosave files in DIRECTORY."
  (interactive "DFind backup files in directory: ")
  (easy-find directory "*~|#*#"))

;;;###autoload
(defun easy-find-tilde (directory)
  "Find Emacs backup files in DIRECTORY."
  (interactive "DFind tilde files in directory: ")
  (easy-find directory "*~"))

;;;###autoload
(defun easy-find-hash (directory)
  "Find Emacs autosave files in DIRECTORY."
  (interactive "DFind hash files in directory: ")
  (easy-find directory "#*#"))

;;;###autoload
(defun easy-find-audio (directory)
  "Find audio files in DIRECTORY."
  (interactive "DFind audio files in directory: ")
  (easy-find directory "*.aac|*.flac|*.m4a|*.mp3|*.ogg|*.wav|*.wma"))

;;;###autoload
(defun easy-find-compressed (directory)
  "Find compressed files in DIRECTORY."
  (interactive "DFind compressed files in directory: ")
  (easy-find directory "*.7z|*.bz2|*.gz|*.par2|*.rar|*.tar|*.tbz2|*.tgz|*.xz|*.zip"))

;;;###autoload
(defun easy-find-big (directory &optional size-mb)
  "Find files larger than SIZE-MB megabytes in DIRECTORY and subdirectories.
If SIZE-MB is not provided, defaults to 100 megabytes."
  (interactive
   (list (read-directory-name "Find large files in directory: " nil default-directory t)
         (let ((input (read-string "Minimum size in MB (default 100): " nil nil "100")))
           (string-to-number input))))
  (let* ((size-mb (or size-mb 100))
         ;; Convert MB to bytes: 1 MB = 1024 * 1024 bytes
         (bytes (* size-mb 1024 1024))
         (find-arguments (format "-type f -size +%dc" bytes)))
    (message "Searching for files larger than %d MB in %s..." size-mb directory)
    (find-dired directory find-arguments)
    (with-current-buffer "*Find*"
      (let ((proc (get-buffer-process (current-buffer))))
        (when proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (easy-find-hide-details)
                 (let ((file-count (- (count-lines (point-min) (point-max)) 2)))
                   (when (< file-count 0) (setq file-count 0))
                   (message "Search complete. Found %d file%s larger than %d MB."
                            file-count
                            (if (= file-count 1) "" "s")
                            size-mb)))))))))))

;;;###autoload
(defun easy-find-huge (directory &optional size-gb)
  "Find exceptionally large files in DIRECTORY and subdirectories.
SIZE-GB is in gigabytes. If not provided, defaults to 1 gigabyte."
  (interactive
   (list (read-directory-name "Find huge files in directory: " nil default-directory t)
         (let ((input (read-string "Minimum size in GB (default 1): " nil nil "1")))
           (string-to-number input))))
  (let* ((size-gb (or size-gb 1))
         ;; Convert GB to bytes: 1 GB = 1024 * 1024 * 1024 bytes
         (bytes (* size-gb 1024 1024 1024))
         (find-arguments (format "-type f -size +%dc" bytes)))
    (message "Searching for files larger than %d GB in %s..." size-gb directory)
    (find-dired directory find-arguments)
    (with-current-buffer "*Find*"
      (let ((proc (get-buffer-process (current-buffer))))
        (when proc
          (set-process-sentinel
           proc
           (lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (easy-find-hide-details)
                 (let ((file-count (- (count-lines (point-min) (point-max)) 2)))
                   (when (< file-count 0) (setq file-count 0))
                   (message "Search complete. Found %d file%s larger than %d GB."
                            file-count
                            (if (= file-count 1) "" "s")
                            size-gb)))))))))))

;;;###autoload
(defun easy-find-cleanup (directory)
  "Find files to clean up in DIRECTORY."
  (interactive "DFind cleanup files in directory: ")
  (easy-find directory "*.deb|*.run|*.docx|*.zip|*.diz|*.webp|*.url|*.gif|*.encr|*.rm|*.mp3|*.html|*.htm|*.pdf|*.png|*.jpe?g|*.m3u|*.nfo|*.exe|*.log|*.doc|*.rar|*.nzb|*.par|*.par2|*.sfv|*.srr|*.txt|*.dat|*.xml|*.r[0-9][0-9]" nil))

(provide 'easy-find)

;;; easy-find.el ends here
