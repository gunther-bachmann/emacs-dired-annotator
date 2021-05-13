;;; org-roam-stack.el --- organize org roam in stack -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020  Free Software Foundation, Inc.

;; Author: Gunther Bachmann <gunther.bachmann@web.de>
;; Maintainer: gunther.bachmann@web.de
;; Package: dired-annotator
;; Homepage: https://github.com/gunther-bachmann/dired-annotator
;; Version: 0.0.1
;; Package-Requires: ((emacs "27"))
;; Keywords: internal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; https://github.com/gunther-bachmann/dired-annotator

;;; Code:
(eval-when-compile
  (require 'cl))

(require 'simple)
(require 'uuid)
(require 'posframe) ;; TODO: make optional (open in separate buffer)
(require 'dired)
(require 'dash) ;; TODO: check how to replace this

(defgroup dired-annotator nil
  "annotate files integrated with dired"
  :group 'dired)

;; -------------------------------------------------------------------------------- customizations
(defcustom dired-annotator-annotations-folder (expand-file-name "~/documents/annotations")
  "location for annotation files"
  :type 'string
  :group 'dired-annotator)

(defcustom dired-annotator-note-icon (propertize "⊜")
  "propertized text (icon) to use for note indication in dired buffer"
  :type '(plist)
  :group 'dired-annotator)

(defcustom dired-annotator-note-popup-width 56
  "width of the note that pops up"
  :type 'integer
  :group 'dired-annotator)

(defcustom dired-annotator-note-popup-height 40
  "max height of the note that pops up"
  :type 'integer
  :group 'dired-annotator)

;; internal only
(defvar dired-annotator--pinning-modes '(immutable-file immutable-location) "list of symbols used as pinning-mode")
(defvar dired-annotator--md5-2-annotation (make-hash-table :test 'equal) "hash from md5 to annotation file")
(defvar dired-annotator--filepath-2-annotation (make-hash-table :test 'equal) "hash from complete absolute file path to annotation file")

(defvar dired-annotator--hash-mode 'head16kmd5 "currently configured hash mode")

(defvar-local dired-annotator--icons-shown-p nil "are icons currently shown in the dired buffer")

;; --------------------------------------------------------------------------------
(unless (file-directory-p dired-annotator-annotations-folder)
  (warn (format "dired-annotator: folder for notes does not exist (%s)" dired-annotator-annotations-folder)))

(defun dired-annotator--os-prerequisites-met? ()
  "are all os prerequisites met?"
  (--all-p (executable-find it)
           '("find" "head" "md5sum" "sed" "tr" "grep")))

(unless (dired-annotator--os-prerequisites-met?)
  (warn "dired-annotator: os prerequisites not met."))

;; --------------------------------------------------------------------------------


(defun dired-annotator--hash (file-name _hash-type)
  "get hash of the given type for the file"
  (cond (t ;; (eq hash-type 'head16kmd5)
           (dired-annotator--head16kmd5 file-name))))

(defun dired-annotator--head16kmd5 (file-name)
  "get md5sum of the given FILE-NAME, taking the first SIZE bytes of the file"
  (let ((dumpsize 16384))
    (shell-command-to-string (format "head -c %d \"%s\" | md5sum | sed -e 's/ *-//g' | tr -d '\n'" dumpsize file-name))))

(defun dired-annotator--head16kmd5s (directory-name)
  "get a list of pairs of file-name, md5sums of the files in DIRECTORY-NAME, taking the first SIZE bytes of each file"
  (let* ((dumpsize 16384)
         (dir-result (shell-command-to-string (format "find %s -type f -maxdepth 1 -exec sh -c \"head -c %d \"{}\" 2>/dev/null | md5sum | sed -e 's/ *-//g' | { tr -d '\n'; echo ' \"{}\"' ; } \" \\;" directory-name dumpsize))))
    (--map (list (substring it 33)
                 (substring it 0 32)
                 'head16kmd5)
           (--filter (< 33 (length it)) (split-string dir-result "\n")))))

; keep a list with ('hash 'last-file-location 'last-file-name 'last-file-size 'org-file-id 'pinning-mode)
; hash               : hash of the file (see hash-type)
; last-file-location : last known location of the file
; last-file-name     : last known file name
; last-file-size     : last knonw file size
; org-file-id        : the id of the annotation file used
; pinning-mode       : is the (list) of attributes by which to identify the file
;                      e.g. non changing file: md5hash + filesize
;                      e.g. changing file: file-name and file-location
; hash-type          : is the method of how the hash was created (e.g. head16kmd5 = md5 of the 16k first bytes of the file)
; all data except the org-file-id can be refreshed by the user (e.g. new pinning, new hash, new name etc.)

(defun dired-annotator--collect-file-information (file-name)
  "collect all information for file identification"
  (let* ((abs-file (file-truename file-name))
         (attributes (file-attributes abs-file)))
    (list (dired-annotator--hash file-name dired-annotator--hash-mode)
          (file-name-directory abs-file)
          (file-name-nondirectory abs-file)
          (file-attribute-size attributes)
          (format-time-string "%Y-%m-%dT%H:%M:%S" (file-attribute-modification-time attributes)))))

(defun dired-annotator--fi-hash (file-information)
  "get hash hash from file-information"
  (or (nth 0 file-information) nil))

(defun dired-annotator--fi-dir (file-information)
  "get directory from file-information"
  (or (nth 1 file-information) nil))

(defun dired-annotator--fi-name (file-information)
  "get file-name from file-information"
  (or (nth 2 file-information) nil))

(defun dired-annotator--fi-size (file-information)
  "get size from file-information"
  (or (nth 3 file-information) 0))

(defun dired-annotator--fi-cdate (file-information)
  "get change date from file-information"
  (or (nth 4 file-information) nil))

(defun dired-annotator--fi-pinning (file-information)
  "get pinning mode from file-information"
  (or (nth 5 file-information) nil))

(defun dired-annotator--fi-hash-type (file-information)
  "get hash type from file-information"
  (or (nth 6 file-information) 'head16kmd5)) ;; data that was created without, will have been hashed with this method

;; (defun dired-annotator--match-p (file-information-a file-information-b pinning-mode)
;;   "check whether FILE-INFORMATION matches"
;;   (cond ((eq pinning-mode 'immutable-file)
;;          (let ((a-size (dired-annotator--fi-size file-information-a))
;;                (b-size (dired-annotator--fi-size file-information-b))
;;                (a-hash (dired-annotator--fi-hash file-information-a))
;;                (b-hash (dired-annotator--fi-hash file-information-b)))
;;            (and (= a-size b-size) (string-equal a-hash b-hash))))
;;         ((eq pinning-mode 'immutable-location)
;;          (let* ((a-name (dired-annotator--fi-name file-information-a))
;;                 (b-name (dired-annotator--fi-name file-information-b))
;;                 (a-dir (dired-annotator--fi-dir file-information-a))
;;                 (b-dir (dired-annotator--fi-dir file-information-b)))
;;            (and (string-equal a-name b-name) (string-equal a-dir b-dir))))
;;         (t nil)))

(defun dired-annotator--load-annotation-info-from-folder ()
  "read all annotation files from the configured folder and put them in a hash along with the file-information stored with it"
  (setq dired-annotator--hash-2-annotation (make-hash-table :test 'equal))
  (setq dired-annotator--fiepath-2-annotation (make-hash-table :test 'equal))
  (-each (directory-files dired-annotator-annotations-folder nil ".*\.org")
    (lambda (annotation-file-name)
      (ignore-errors
        (--> (format "head -c 512 \"%s/%s\" | grep '^#+property: file-information ' | sed -e 's/#+property: file-information \\(.*\\)/\\1/g'" dired-annotator-annotations-folder annotation-file-name)
          (shell-command-to-string it)
          (string-trim it)
          (read-from-string it)
          (car it)
          (dired-annotator--hash-file-information it annotation-file-name))))))

(defun dired-annotator--unhash-file (hash-key absolute-file-name)
  "remove given keys from hash

HASH-KEY is the hash of the annotated file
ABSOLUTE-FILE-NAME is the absolute file name of the annotated file"
  (puthash hash-key nil dired-annotator--hash-2-annotation)
  (puthash absolute-file-name nil dired-annotator--filepath-2-annotation))

(defun dired-annotator--hash-file-information (file-information annotation-file-name)
  "put the given file-information along with the annotation-file into the hash"
  (let ((pinning (dired-annotator--fi-pinning file-information)))
    (cond ((eq pinning 'immutable-location)
           (puthash (format "%s%s"
                            (dired-annotator--fi-dir file-information)
                            (dired-annotator--fi-name file-information))
                    annotation-file-name dired-annotator--filepath-2-annotation))
          (t ;; (eq pinning 'immutable-file)
           (puthash (dired-annotator--fi-hash file-information) annotation-file-name dired-annotator--hash-2-annotation)))))

(defun dired-annotator--create-annotation (absolute-file-name pinning-mode)
  "create a blank annotation for ABSOLUTE-FILE-NAME"
  (let ((annotation-file-name (format "%s.org" (uuid-string))))
    (with-temp-file (dired-annotator--to-abs-file annotation-file-name)
      (insert (format "#+title: %s\n" (file-name-nondirectory absolute-file-name)))
      (insert (format "#+property: file-information %S\n"
                      (-snoc
                       (dired-annotator--collect-file-information absolute-file-name)
                       pinning-mode)))
      (insert "* Notes\n  some notes\n"))
    (dired-annotator--hash-file-information
     (dired-annotator--collect-file-information absolute-file-name)
     annotation-file-name)
    annotation-file-name))

(defun dired-annotator--get-annotation-for (absolute-file-name)
  "get the annotation of the given file (if existent) from the hash"
  (let ((file-information (dired-annotator--collect-file-information absolute-file-name)))
    (or (gethash (dired-annotator--fi-hash file-information) dired-annotator--hash-2-annotation)
       (gethash absolute-file-name dired-annotator--filepath-2-annotation))))

(defun dired-annotator--remove-all-posframes ()
  "remove all posframes and remove precommand hook to do so"
  (ignore-errors
    (posframe-hide-all))
  (remove-hook 'pre-command-hook #'dired-annotator--remove-all-posframes))

(defun dired-annotator--add-note-icon-to-line ()
  "add the note icon to the file of the current line"
  (save-excursion
    (end-of-line)
    (dired-annotator--add-overlay (point) (concat " " dired-annotator-note-icon))))

(defun dired-annotator--to-abs-file (annotation-file)
  "get absolute filename for this annotation file"
  (format "%s/%s" dired-annotator-annotations-folder annotation-file))

(defun dired-annotator--remove-note-icon-from-line ()
  "remove note icon from the file of the current line"
  (save-excursion
    (let (beg end)
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))
      (dired-annotator--delete-overlay-between beg end))))

(defun dired-annotator--overlays-in (beg end)
  "Get all dired-annotator-overlays between BEG to END."
  (cl-remove-if-not
   (lambda (ov)
     (overlay-get ov 'dired-annotator-overlay))
   (overlays-in beg end)))

(defun dired-annotator--delete-overlay-between (beg end)
  "delete dired annotator overlay between start and end"
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (dired-annotator--overlays-in beg end))))

(defun dired-annotator--add-overlay (pos string)
  "Add dired annotator overlay to display STRING at POS."
  (let ((ov (make-overlay (1- pos) pos)))
    (overlay-put ov 'dired-annotator-overlay t)
    (overlay-put ov 'after-string string)))

(defun dired-annotator--show-icons-in-region (min max)
  "Display the note icon on files with notes in dired buffer."
  (let* ((annotation-count 0)
         (file-count 0)
         (time
          (measure-time
           (save-excursion
             (goto-char min)
             (loop-until (>= (point) max)
               (when (dired-move-to-filename nil)
                 (incf file-count)
                 (when-let* ((file (dired-get-filename))
                             (not-directory (not (file-directory-p file)))
                             (annotation (dired-annotator--get-annotation-for file)))
                   (dired-annotator--add-note-icon-to-line)
                   (incf annotation-count)
                   (beginning-of-line)))
               (forward-line 1))))))
    (list annotation-count file-count time)))

(defun dired-annotator--hide-icons-in-region (start end)
  "hide all dired annotator icons in the given region"
  (save-restriction
    (widen)
    (mapc #'delete-overlay
          (dired-annotator--overlays-in start end))))

(defun dired-annotator--get-note-icon-position ()
  "get the note icon position within the given line (or nil if not found)"
  (when-let ((ov (save-excursion
                   (let ((start (point))
                         (end (progn (end-of-line) (point))))
                     (dired-annotator--overlays-in start end)))))
    (+ 1 (overlay-start (car ov)))))

;; -------------------------------------------------------------------------------- API
(defun dired-annotator-show-icons ()
  "Display the note icon on files with notes in dired buffer."
  (interactive)
  (dired-annotator-hide-icons)
  (setq dired-annotator--icons-shown-p t)
  (let ((res (dired-annotator--show-icons-in-region (point-min) (point-max))))
    (message (format "found %d notes looking at %d files in %f seconds" (nth 0 res) (nth 1 res) (nth 2 res)))))

(defun dired-annotator-hide-icons ()
  "Remove all `dired-annotator' overlays."
  (interactive)
  (setq dired-annotator--icons-shown-p nil)
  (dired-annotator--hide-icons-in-region (point-min) (point-max)))

(defun dired-annotator-edit-note ()
  "create a new annotation and open it, or open an existing"
  (interactive)
  (let* ((absolute-file-name (dired-get-filename))
         (annotation (or (dired-annotator--get-annotation-for absolute-file-name)
                        (when-let* ((new-annotation
                                     (dired-annotator--create-annotation
                                      absolute-file-name
                                      (intern (completing-read "pinning-mode: " dired-annotator--pinning-modes)))))
                          (when dired-annotator--icons-shown-p (dired-annotator--add-note-icon-to-line))
                          new-annotation)))
         (annotation-file-name (format "%s/%s" dired-annotator-annotations-folder annotation)))
    (when-let ((fb (get-file-buffer annotation-file-name)))
      (kill-buffer fb))
    (find-file annotation-file-name)
    (when (= (point-min) (point))
      (search-forward "some notes" nil t))))

(defun dired-annotator-delete-note ()
  "delete an existing annotation (if present)"
  (interactive)
  (when-let* ((annotated-file-name (dired-get-filename))
              (annotation (dired-annotator--get-annotation-for annotated-file-name))
              (annotation-file-name (dired-annotator--to-abs-file annotation)))
    (when (yes-or-no-p "Really delete this annotation? ")
      (when-let ((fb (get-file-buffer annotation-file-name)))
        (kill-buffer fb))
      (dired-annotator--remove-note-icon-from-line)
      (dired-annotator--unhash-file (dired-annotator--hash annotated-file-name dired-annotator--hash-mode) annotated-file-name)
      (delete-file annotation-file-name))))

(defun dired-annotator-show-note ()
  "display the annotation in a posframe (if existent)"
  (interactive)
  (when-let* ((absolute-file-name (dired-get-filename))
              (annotation (dired-annotator--get-annotation-for absolute-file-name))
              (annotation-abs-file-name (format "%s/%s" dired-annotator-annotations-folder annotation)))
    (posframe-show (find-file-noselect annotation-abs-file-name) :position (or (dired-annotator--get-note-icon-position) (point)) :lines-truncate t
                      :width dired-annotator-note-popup-width :height dired-annotator-note-popup-height
                      :border-color "plum4" :border-width 2 :left-fringe 3 :right-fringe 3 )
    (add-hook 'pre-command-hook #'dired-annotator--remove-all-posframes)))

;; -------------------------------------------------------------------------------- dired-subtree integration
(when (package-installed-p 'dired-subtree)
  (eval-after-load 'dired-subtree
    (progn
      (defun dired-annotator--subtree--possibly-show-for-inserted ()
        "if currently showing icons, try to collect information for inserted subtree"
        (when dired-annotator--icons-shown-p
          (let ((ov (dired-subtree--get-ov)))
            (dired-annotator--show-icons-in-region (overlay-start ov) (overlay-end ov)))))

      (defun dired-annotator--subtree--cleanup-icons-after-fold ()
        "remove any remaining icons after dired subtree is folded"
        (save-restriction
          (let ((line (point)))
            (forward-line)
            (beginning-of-line)
            (let ((end (1+ (point))))
              (message (format "hide from %d to %d" line end))
              (dired-annotator--hide-icons-in-region line end)))))

      (when (boundp 'dired-subtree-after-insert-hook)
        (add-hook 'dired-subtree-after-insert-hook #'dired-annotator--subtree--possibly-show-for-inserted))

      (when (boundp 'dired-subtree-after-remove-hook)
        (add-hook 'dired-subtree-after-remove-hook #'dired-annotator--subtree--cleanup-icons-after-fold)))))

;; --------------------------------------------------------------------------------
(when (file-directory-p dired-annotator-annotations-folder)
  (dired-annotator--load-annotation-info-from-folder))

(provide 'dired-annotator)
