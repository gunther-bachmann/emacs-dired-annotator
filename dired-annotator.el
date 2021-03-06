;;; org-roam-stack.el --- organize org roam in stack -*- lexical-binding:t -*-

;; Copyright (C) 2019-2020  Free Software Foundation, Inc.

;; Author: Gunther Bachmann <gunther.bachmann@web.de>
;; Maintainer: gunther.bachmann@web.de
;; Package: dired-annotator
;; Homepage: https://github.com/gunther-bachmann/dired-annotator
;; Version: 0.0.3
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
(require 'dired)
(require 'dash)

(defgroup dired-annotator nil
  "annotate files integrated with dired"
  :group 'dired)

;; -------------------------------------------------------------------------------- customizations
(defcustom dired-annotator-annotations-folder (expand-file-name "~/documents/annotations")
  "location for annotation files"
  :type 'string
  :group 'dired-annotator)

(defcustom dired-annotator-note-icon (propertize "⏵")
  "propertized text (icon) to use for note indication in dired buffer"
  :type '(plist)
  :group 'dired-annotator)

(defcustom dired-annotator-modeline
  '(:eval (dired-annotator-modeline-function))
  "element that is added to the modeline"
  :type 'list
  :group 'dired-annotator)

(defcustom dired-annotator-after-icon-shown-hook '()
  "list of hooks called whenever the icon for a file is shown.
each hook is called with two parameters, the absolute file name of the file that has the note
and the annotation information itself."
  :type 'hook
  :group 'dired-annotator)

(defcustom dired-annotator-after-icons-shown-hook #'dired-annotator--report-collection-stats
  "list of hooks called after new icons are shown.
each hook is called with three parameters:
the number of annotations found
the number of files checked
the time it took to collect the annotations"
  :type 'hook
  :group 'dired-annotator)

(defcustom dired-annotator-after-icons-removed-hook '()
  "list of hooks called after icons are removed"
  :type 'hook
  :group 'dired-annotator)

(defcustom dired-annotator-note-popup-hook #'dired-annotator--popup-note
  "called to open an annotation.
it gets passed: pos of note (icon), absolute file name of annotation"
  :type 'hook
  :group 'dired-annotator)

(defcustom dired-annotator-note-popup-remove-hook nil
  "called to remove an annotation."
  :type 'hook
  :group 'dired-annotator)

(defcustom dired-annotator-note-fill-column 54
  "set fill column of the annotation to this value"
  :type 'number
  :group 'dired-annotator)

(defcustom dired-annotator-integrate-with-dired-narrow
  (and (package-installed-p 'dired-narrow)
         (package-installed-p 'org-ql))
  "should annotator integrate with dired narrow?

make sure to have all necessary packages installed"
  :type 'boolean
  :group 'dired-annotator)

(defcustom dired-annotator-show-non-tagged-on-narrow t
  "should files without tag be shown on narrowing?

only valid if integration with dired narrow is activated"
  :type 'boolean
  :group 'dired-annotator)

(defcustom dired-annotator-seconds-to-note-buffer-removal (* 5 60)
  "seconds until annotation buffers are removed"
  :type 'number
  :group 'dired-annotator)

;; -------------------------------------------------------------------------------- internal only
(defvar dired-annotator--pinning-modes '(immutable-file immutable-location)
  "list of symbols used as pinning-mode")
(defvar dired-annotator--md5-2-annotation (make-hash-table :test 'equal)
  "hash from md5 to annotation file")
(defvar dired-annotator--filepath-2-annotation (make-hash-table :test 'equal)
  "hash from complete absolute file path to annotation file")

(defvar dired-annotator--hash-mode 'head16kmd5 "currently configured hash mode")

(defvar dired-annotator--hash-2-annotation nil "hashmap mapping file hash to annotation")
(defvar dired-annotator--filepath-2-annotation nil "hashmap mapping filepath to annotation")
(defvar dired-annotator-buffer-cleanup-timer nil "timer currently running to cleanup annotation buffers")

(defvar-local dired-annotator--icons-shown-p nil "are icons currently shown in the dired buffer")
(defvar-local dired-annotator--note-should-not-popup nil
  "note should not popup again if show note is triggered.
this allows for trigger show/hide behaviour if the same command is repeated.")

(defvar-local dired-annotator-show nil "variable to be set via .dir-locals.el")

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

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY and return this in ms."
  (let ((time-sym (gensym 'time)))
    `(let (
           (,time-sym (current-time)))
       ,@body
       (float-time (time-since ,time-sym)))))

(defun dired-annotator--hash (file-name _hash-type)
  "get hash of the given type for the file"
  (cond (t ;; (eq hash-type 'head16kmd5)
         (dired-annotator--head16kmd5 file-name))))

(defun dired-annotator--head16kmd5 (file-name)
  "get md5sum of the given FILE-NAME, taking the first SIZE bytes of the file"
  (let ((dumpsize 16384))
    (shell-command-to-string (format "head -c %d \"%s\" | md5sum | sed -e 's/ *-//g' | tr -d '\n'"
                                     dumpsize file-name))))

(defun dired-annotator--head16kmd5s (directory-name)
  "get a list of pairs of file-name, md5sums of the files in DIRECTORY-NAME, taking the first SIZE bytes of each file"
  (let* ((dumpsize 16384)
         (dir-result (shell-command-to-string
                      (format "find %s -type f -maxdepth 1 -exec sh -c \"head -c %d \"{}\" 2>/dev/null | md5sum | sed -e 's/ *-//g' | { tr -d '\n'; echo ' \"{}\"' ; } \" \\;"
                              directory-name dumpsize))))
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
  "read all annotation files from the configured folder
and put them in a hash along with the file-information stored with it"
  (setq dired-annotator--hash-2-annotation (make-hash-table :test 'equal))
  (setq dired-annotator--filepath-2-annotation (make-hash-table :test 'equal))
  (-each (directory-files dired-annotator-annotations-folder nil ".*\.org")
    (lambda (annotation-file-name)
      (ignore-errors
        (dired-annotator--hash-file-information
         (dired-annotator--read-fileinformation annotation-file-name)
         annotation-file-name)))))

(defun dired-annotator--read-fileinformation (annotation-file-name)
  "read and return the file information structure of the given file"
  (ignore-errors
    (--> (format "head -c 512 \"%s/%s\" | grep '^#+property: file-information ' | sed -e 's/#+property: file-information \\(.*\\)/\\1/g'"
                 dired-annotator-annotations-folder annotation-file-name)
         (shell-command-to-string it)
         (string-trim it)
         (read-from-string it)
         (car it))))

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
           (puthash (dired-annotator--fi-hash file-information)
                    annotation-file-name
                    dired-annotator--hash-2-annotation)))))

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
          (dired-annotator--overlays-in beg end))
    (run-hooks 'dired-annotator-after-icons-removed-hook)))

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
             (while (< (point) max)
               (when (dired-move-to-filename nil)
                 (cl-incf file-count)
                 (when-let* ((file (dired-get-filename))
                             (not-directory (not (file-directory-p file)))
                             (visible (not (get-text-property (line-beginning-position) 'invisible)))
                             (annotation (dired-annotator--get-annotation-for file)))
                   (dired-annotator--add-note-icon-to-line)
                   (run-hook-with-args 'dired-annotator-after-icon-shown-hook file annotation)
                   (cl-incf annotation-count)
                   (beginning-of-line)))
               (forward-line 1))))))
    (list annotation-count file-count time)))

(defun dired-annotator--get-note-icon-position ()
  "get the note icon position within the given line (or nil if not found)"
  (when-let ((ov (save-excursion
                   (let ((start (point))
                         (end (progn (end-of-line) (point))))
                     (dired-annotator--overlays-in start end)))))
    (+ 1 (overlay-start (car ov)))))

(defun dired-annotator--wrapped-revert-buffer (orig-func &rest params)
  "make sure that buffer reverts either hide remaining icons, or redisplays them appropriately"
  (apply orig-func params)
  (dired-annotator--update-icon-display))

(defun dired-annotator--after-omit-expunge (&rest _params)
  "update icon display after omit expunged files"
  (dired-annotator--update-icon-display))

(defun dired-annotator--update-icon-display ()
  "either show or hide the icons for the whole buffer,
depending on buffer local variable DIRED-ANNOTATOR--ICONS-SHOWN-P"
  (when (derived-mode-p 'dired-mode)
    (if dired-annotator--icons-shown-p
        (dired-annotator--show-icons)
      (dired-annotator--hide-icons))))

(defun dired-annotator--hide-icons ()
  "hide all icons in the current buffr"
  (dired-annotator--delete-overlay-between (point-min) (point-max)))

(defun dired-annotator--show-icons ()
  "show all icons in the given buffer, if file has a note"
  (dired-annotator--hide-icons)
  (dired-annotator--show-icons-in-region (point-min) (point-max)))

(defun dired-annotator--report-collection-stats (annotation-count file-count time)
  "hook to inform about the stats collected during annotation collection"
  (when (derived-mode-p 'dired-mode)
    (message
     (format "found %d notes looking at %d files in %f seconds in %s (and open subdirs)"
             annotation-count file-count time default-directory))))

(defun dired-annotator--any-buffer-showing-icons? ()
  "does any buffer exist that currently should show icons in dired?"
  (--any? (with-current-buffer it
            (and (derived-mode-p 'dired-mode)
               dired-annotator--icons-shown-p))
          (buffer-list)))

(defun dired-annotator--add-integration-advices ()
  "add all advices for integrating dired-annotator with others"
  (advice-add 'revert-buffer :around #'dired-annotator--wrapped-revert-buffer)
  (when (fboundp #'dired-omit-expunge)
    (advice-add 'dired-omit-expunge :after #'dired-annotator--after-omit-expunge)))

(defun dired-annotator--remove-integration-advices ()
  "remove all integration advices of dired-annotator with others"
  (advice-remove 'revert-buffer #'dired-annotator--wrapped-revert-buffer)
  (when (fboundp #'dired-omit-expunge)
    (advice-remove 'dired-omit-expunge #'dired-annotator--after-omit-expunge)))

(defun dired-annotator--remove-note-popup ()
  "called to remove popup"
  (remove-hook 'pre-command-hook #'dired-annotator--remove-note-popup)
  (run-hooks 'dired-annotator-note-popup-remove-hook))

(defun dired-annotator--popup-note (_position annotation-filename)
  "default open annotation"
  (let ((annotation-buffer (find-file-noselect annotation-filename)))
    (view-buffer-other-window annotation-buffer)))

(defmacro dired-annotator--with-default-directory (dir &rest body)
  (declare (indent 1) (debug (form body)))
  `(when-let ((default-directory ,dir))
     ,@body))

(defun dired-annotator--cleanup-modeline ()
  "cleanup modeline string"
  (setq global-mode-string (remove dired-annotator-modeline global-mode-string))
  (force-mode-line-update t))

(defun dired-annotator--setup-modeline ()
  "setup modeline string if wanted"
  (dired-annotator--cleanup-modeline)
  (when dired-annotator-modeline
    (setq global-mode-string (append global-mode-string (list dired-annotator-modeline)))
    (force-mode-line-update t)))

;; -------------------------------------------------------------------------------- org tags via org-ql
(when (package-installed-p 'org-ql)
  (defun dired-annotator--get-tags-for (absolute-file-name)
    "get tags of the annotation for the given file"
    (when-let* ((annotation-file (dired-annotator--get-annotation-for absolute-file-name)))
      (--map (substring-no-properties it)
             (car (org-ql-select (concat dired-annotator-annotations-folder "/" annotation-file)
                    '(tags-all)
                    :action #'org-get-tags)))))

  (defun dired-annotator--get-all-tags ()
    "get a unique list of tags that are available in annotated files"
    (-as-> (org-ql-select (directory-files dired-annotator-annotations-folder t ".*\\.org$")
                                  '(tags-all)
                                  :action #'org-get-tags)
       tags
       (--mapcat it tags)
       (--map (substring-no-properties it) tags)
       (-uniq tags))))

;; -------------------------------------------------------------------------------- dired-narrow integration
(when dired-annotator-integrate-with-dired-narrow

  (require 'dired-narrow)

  (defun dired-annotator--tag-filter-function (tag)
    "should the file under cursor be filtered given this TAG?"
    (if-let ((tags (dired-annotator--get-tags-for
                    (concat (dired-current-directory)
                            (dired-utils-get-filename 'no-dir))))) ;; TODO cache tags for file
        (--find (string-equal tag it) tags)
      dired-annotator-show-non-tagged-on-narrow))

  (defun dired-annotator--get-tags ()
    "get tags for current file in dired"
    (when-let* ((file-name (dired-utils-get-filename 'no-dir))
                (absolute-file-name (concat (dired-current-directory) file-name)))
      (dired-annotator--get-tags-for absolute-file-name)))

  (defun dired-annotator--minimal-narrow (filter-function initial-filter)
    "integrate with dired-narrow

this contains very specific dired-narrow code that might change over time."
    (setq dired-narrow-filter-function filter-function)
    (dired-narrow-mode 1)
    (add-to-invisibility-spec :dired-narrow)
    (dired-narrow--update initial-filter)
    (let ((inhibit-read-only t))
      (dired-narrow--remove-text-with-property :dired-narrow))
    (funcall dired-narrow-exit-action)))

;; -------------------------------------------------------------------------------- API
(defun dired-annotator-modeline-function ()
  "indicator, whether notes should be displayed or not"
  (if (derived-mode-p 'dired-mode)
    (concat " " (if dired-annotator--icons-shown-p
                    dired-annotator-note-icon
                  "-"))
    ""))

(defun dired-annotator-check-dir-local-show ()
  (when (bound-and-true-p dired-annotator-show)
    (dired-annotator-show-icons)))

(defun dired-annotator-show-icons ()
  "Display the note icon on files with notes in dired buffer."
  (interactive)
  (if (not (derived-mode-p 'dired-mode))
      (message "only available in dired buffers")
    (-let [(annotation-count file-count time) (dired-annotator--show-icons)]
      (run-hook-with-args 'dired-annotator-after-icons-shown-hook annotation-count file-count time))
    (unless (dired-annotator--any-buffer-showing-icons?)
      (dired-annotator--setup-modeline)
      (dired-annotator--add-integration-advices))
    (setq dired-annotator--icons-shown-p t)))

(defun dired-annotator-hide-icons ()
  "Remove all `dired-annotator' overlays."
  (interactive)
  (if (not (derived-mode-p 'dired-mode))
      (message "only available in dired buffers")
    (setq dired-annotator--icons-shown-p nil)
    (dired-annotator--hide-icons)
    (unless (dired-annotator--any-buffer-showing-icons?)
      (dired-annotator--cleanup-modeline)
      (dired-annotator--remove-integration-advices))))

(defun dired-annotator-edit-note ()
  "create a new annotation and open it, or open an existing"
  (interactive)
  (if (not (derived-mode-p 'dired-mode))
      (message "only available in dired buffers")
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
      (when (integerp dired-annotator-note-fill-column)
        (setq fill-column dired-annotator-note-fill-column))
      (org-show-all)
      (when (= (point-min) (point))
        (search-forward "some notes" nil t)))))

(defun dired-annotator-delete-note ()
  "delete an existing annotation (if present)"
  (interactive)
  (if (not (derived-mode-p 'dired-mode))
      (message "only available in dired buffers")
    (when-let* ((annotated-file-name (dired-get-filename))
                (annotation (dired-annotator--get-annotation-for annotated-file-name))
                (annotation-file-name (dired-annotator--to-abs-file annotation)))
      (when (yes-or-no-p "Really delete this annotation? ")
        (when-let ((fb (get-file-buffer annotation-file-name)))
          (kill-buffer fb))
        (dired-annotator--remove-note-icon-from-line)
        (dired-annotator--unhash-file (dired-annotator--hash annotated-file-name dired-annotator--hash-mode) annotated-file-name)
        (delete-file annotation-file-name)))))

(defun dired-annotator-show-note ()
  "display the annotation (if existent)"
  (interactive)
  (if (not (derived-mode-p 'dired-mode))
      (message "only available in dired buffers")
    (if dired-annotator--note-should-not-popup
        (setq dired-annotator--note-should-not-popup nil)
      (when-let* ((absolute-file-name (dired-get-filename))
                  (annotation (dired-annotator--get-annotation-for absolute-file-name))
                  (annotation-abs-file-name (format "%s/%s" dired-annotator-annotations-folder annotation)))
        (run-hook-with-args 'dired-annotator-note-popup-hook (or (dired-annotator--get-note-icon-position) (point)) annotation-abs-file-name)
        (add-hook 'pre-command-hook #'dired-annotator--remove-note-popup)))))

(defun dired-annotator-reload-annotation-info ()
  "reload annotation information from disk and adjust visible icons accordingly"
  (interactive)
  (if (file-directory-p dired-annotator-annotations-folder)
      (progn
        (dired-annotator--load-annotation-info-from-folder)
        (when dired-annotator--icons-shown-p
          (dired-annotator--show-icons)))
    (message (format "folder for annotations unknown (%s)" dired-annotator-annotations-folder))))

(defun dired-annotator-dired-on-annotation-folder ()
  "open annotation folder via dired"
  (interactive)
  (dired dired-annotator-annotations-folder))

(when dired-annotator-integrate-with-dired-narrow

  (defun dired-annotator-narrow-on-tag ()
    "Narrow a dired buffer to the files having annotations with the given tag."
    (interactive)
    ;; (dired-annotator--dired-narrow--internal 'dired-annotator--tag-filter-function (completing-read  "tag: " (dired-annotator--get-all-tags) nil t))
    (dired-annotator--minimal-narrow 'dired-annotator--tag-filter-function (completing-read  "tag: " (dired-annotator--get-all-tags) nil t))
    (dired-annotator--show-icons)))

;; -------------------------------------------------------------------------------- remove open notes after some time
(defun dired-annotator-register-buffer-cleanup ()
  (when dired-annotator-buffer-cleanup-timer
    (cancel-timer dired-annotator-buffer-cleanup-timer))
  (setq dired-annotator-buffer-cleanup-timer
        (run-at-time dired-annotator-seconds-to-note-buffer-removal nil #'dired-annotator-clean-unused-note-buffers)))

(defun dired-annotator-clean-unused-note-buffers ()
  (let ((ts (format-time-string "%Y-%m-%d %T")))
    (dolist (buf (buffer-list))
      (let ((bn (buffer-name buf)))
        (when (buffer-live-p buf)
          (message "[%s] `%s'" ts bn)
          (unless (or (get-buffer-process buf)
                     (and (buffer-file-name buf) (buffer-modified-p buf))
                     (not (dired-annotator-buffer-is-note-p buf))
                     (get-buffer-window buf 'visible))
            (message "[%s] killing `%s'" ts bn)
            (kill-buffer buf))))))
  (setq dired-annotator-buffer-cleanup-timer nil))

(defun dired-annotator-buffer-is-note-p (buffer)
  "is the given buffer an annotation file?" 
  (when-let ((bn (buffer-file-name buffer)))
    (string-prefix-p dired-annotator-annotations-folder bn)))

;; -------------------------------------------------------------------------------- dired-subtree integration
(when (package-installed-p 'dired-subtree)
  (eval-after-load 'dired-subtree
    (progn
      (defun dired-annotator--subtree--possibly-show-for-inserted ()
        "if currently showing icons, try to collect information for inserted subtree"
        (when-let* ((filename (ignore-errors (dired-get-filename)))
                    (foldername (if (file-directory-p filename)
                                    (format "%s/../" filename)
                                  (file-name-directory filename)))
                    (folder (expand-file-name foldername)))
          (dired-annotator--with-default-directory
              folder
            (hack-dir-local-variables) ;; don't apply, just collect
            (when-let ((found (--find (eq 'dired-annotator-show (car it)) dir-local-variables-alist)))
              (setq dired-annotator-show (cdr found))))
          (when (or (bound-and-true-p dired-annotator-show)
                   (and (not (boundp dired-annotator-show))
                      dired-annotator--icons-shown-p))
            (let ((ov (dired-subtree--get-ov)))
              (dired-annotator--show-icons-in-region (overlay-start ov) (overlay-end ov))))))

      (defun dired-annotator--subtree--cleanup-icons-after-fold ()
        "remove any remaining icons after dired subtree is folded"
        (save-restriction
          (setq dir-local-variables-alist
		(assq-delete-all 'dired-annotator-show dir-local-variables-alist))
          (hack-dir-local-variables) ;; don't apply, just collect
          (if-let ((found (--find (eq 'dired-annotator-show (car it)) dir-local-variables-alist)))
              (setq dired-annotator-show (cdr found))
            (setq dired-annotator-show nil))
          (let ((line (point)))
            (forward-line)
            (beginning-of-line)
            (let ((end (1+ (point))))
              (dired-annotator--delete-overlay-between line end)))))

      (when (boundp 'dired-subtree-after-insert-hook)
        (add-hook 'dired-subtree-after-insert-hook #'dired-annotator--subtree--possibly-show-for-inserted))

      (when (boundp 'dired-subtree-after-remove-hook)
        (add-hook 'dired-subtree-after-remove-hook #'dired-annotator--subtree--cleanup-icons-after-fold)))))

;; --------------------------------------------------------------------------------
(when (file-directory-p dired-annotator-annotations-folder)
  (dired-annotator--load-annotation-info-from-folder))

(provide 'dired-annotator)
