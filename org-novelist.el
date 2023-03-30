;;; org-novelist.el --- An Org mode system for writing and exporting fiction novels -*- lexical-binding: t; -*-

;; JUF's methodology for keeping novel writing nice and tidy.
;; Copyright (C) 2023 John Urquhart Ferguson
;;
;; Author: John Urquhart Ferguson <mail@johnurquhartferguson.info>
;; Maintainer: John Urquhart Ferguson <mail@johnurquhartferguson.info>
;; URL: https://johnurquhartferguson.info
;; Keywords: fiction, writing, outlines
;; Prefix: org-novelist
;; Package-Requires: ((org "9.5.5"))

;; Version: 0.0.2

;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Org Novelist is a methodology for writing novel-length fiction using
;; Org mode within Emacs. It involves creating and laying out Org mode
;; files such that notes and plans can be easily created and quickly
;; accessed while writing the main text of a story. Org Novelist's
;; secondary function is the ability to use this known structure to
;; easily export and publish stories to other formats. This package
;; supplies a collection of support functions which make it easier to
;; use this methodology.
;;
;; Creating, linking, and laying out files in the Org Novelist
;; methodology can be done without the use of Emacs or the Org Novelist
;; package, but using the package within Emacs will provide helper
;; functions that make the methodology much easier to use; allowing the
;; following of links, programmatic updating of crossreferences, and
;; the ability to programatically export to other formats.
;;
;; This is the first public release of Org Novelist.
;;
;; Installation, Activation, and Documentation
;; -------------------------------------------
;; See the corresponding section of the website at
;;
;;   https://johnurquhartferguson.info

;;; Code:


;;;; Require other packages

(require 'org)  ; Org Novelist is built upon the incredible work of Org mode


;;;; Global Variables

(defvar orgn--autoref-p nil "Temporary store for last known value of org-novelist-automatic-referencing-p.")


;;;; Global Constants

;; This constant allows the system to work on Linux and Windows (and
;; any other system, though I've only tested those two). I think it's
;; faster for Emacs than calling the `file-name-as-directory' function
;; every time I need it, and using the alias keeps the code cleaner.
(defvaralias 'orgn--folder-separator '/ "Add in an alias to make code cleaner when constructing file locations.")
(defconst orgn--folder-separator (file-name-as-directory "/") "Assign the current system's folder separator to a global variable.")

(defconst orgn--config-filename "org-novelist-config.org" "Filename of where Org Novelist will store configuration data.")
(defconst orgn--file-ending ".org" "The ending of the filenames used in Org Novelist.")
(defconst orgn--mode-identifier "; -*-Org-Novelist-*-" "The Emacs mode identifier for Org Novelist.")


;;;; Language Packs

;;;; British English (en-GB)
;; User Variable Fallback Strings
(defconst orgn--author-not-set-en-GB "Author Not Set" "Author not specified by user.")
(defconst orgn--author-email-not-set-en-GB "Author Email Not Set" "Author email not specified by user.")
;; File Instructions
(defconst orgn--main-file-instructions-en-GB "Write a brief summary of the story here" "Instructions for the main entry-point file.")
(defconst orgn--notes-file-instructions-en-GB "Write any general notes for the story here" "Instructions for the general notes file.")
(defconst orgn--research-file-instructions-en-GB "Write any research notes for the story here" "Instructions for the general research file.")
(defconst orgn--characters-file-instructions-en-GB "This is an index of the characters in the story. Do not edit manually. Use only Org mode or Org Novelist functions." "Instructions for the character index file.")
(defconst orgn--places-file-instructions-en-GB "This is an index of the places in the story. Do not edit manually. Use only Org mode or Org Novelist functions." "Instructions for the location index file.")
(defconst orgn--props-file-instructions-en-GB "This is an index of the props in the story. Do not edit manually. Use only Org mode or Org Novelist functions." "Instructions for the prop index file.")
(defconst orgn--chapters-file-instructions-en-GB "This is an index of the chapters in the story. Do not edit manually. Use only Org mode or Org Novelist functions." "Instructions for the chapter index file.")
;; Folder Names
(defconst orgn--notes-folder-en-GB "Notes" "The folder name for storing note files.")
(defconst orgn--indices-folder-en-GB "Indices" "The folder name for storing index files.")
(defconst orgn--chapters-folder-en-GB "Chapters" "The folder for storing the chapter files.")
(defconst orgn--exports-folder-en-GB "Exports" "The folder for storing export files.")
;; File Names
(defconst orgn--main-file-en-GB "main" "Name for the story's main entry-point file.")
(defconst orgn--notes-file-en-GB "notes" "Name for the story's general notes file.")
(defconst orgn--research-file-en-GB "research" "Name for the story's general research file.")
(defconst orgn--characters-file-en-GB "characters" "Name for the story's character index file.")
(defconst orgn--places-file-en-GB "places" "Name for the story's location index file.")
(defconst orgn--props-file-en-GB "props" "Name for the story's prop index file.")
(defconst orgn--chapters-file-en-GB "chapters" "Name for the story's chapter index file.")
(defconst orgn--chapter-file-prefix-en-GB "chapter-" "Prefix for the story's chapter files.")
(defconst orgn--notes-suffix-en-GB "-notes" "Suffix for a file's associated notes file.")
(defconst orgn--character-file-prefix-en-GB "character-" "Prefix for the story's character files.")
(defconst orgn--prop-file-prefix-en-GB "prop-" "Prefix for the story's prop files.")
(defconst orgn--place-file-prefix-en-GB "place-" "Prefix for the story's place files.")
;; File Titles
(defconst orgn--notes-title-en-GB "Notes" "Name for the story's general notes title.")
(defconst orgn--research-title-en-GB "Research" "Name for the story's general research title.")
(defconst orgn--characters-title-en-GB "Characters" "Name for the story's character index title.")
(defconst orgn--places-title-en-GB "Places" "Name for the story's location index title.")
(defconst orgn--props-title-en-GB "Props" "Name for the story's prop index title.")
(defconst orgn--chapters-title-en-GB "Chapters" "Name for the story's chapter index title.")
(defconst orgn--config-name-en-GB "Export Settings" "Display name for a link to the story's configuration file.")
;; File Preambles
(defconst orgn--notes-for-en-GB "Notes for" "Part of the preamble for the general notes file.")
(defconst orgn--research-for-en-GB "Research for" "Part of the preamble for the general research file.")
(defconst orgn--character-index-for-en-GB "Character Index for" "Part of the preamble for the character index file.")
(defconst orgn--place-index-for-en-GB "Place Index for" "Part of the preamble for the location index file.")
(defconst orgn--prop-index-for-en-GB "Prop Index for" "Part of the preamble for the prop index file.")
(defconst orgn--chapter-index-for-en-GB "Chapter Index for" "Part of the premable for the chapter index file.")
(defconst orgn--front-matter-heading-en-GB "Front Matter" "Name for the Front Matter of the book chapters, used as a heading.")
(defconst orgn--main-matter-heading-en-GB "Main Matter" "Name for the Main Matter of the book chapters, used as a heading.")
(defconst orgn--back-matter-heading-en-GB "Back Matter" "Name for the Back Matter of the book chapters, used as a heading.")
(defconst orgn--file-by-file-en-GB "Select individually for each file" "Offer to the user to make selections on a file by file basis.")
(defconst orgn--notes-en-GB "Notes" "Part of the preamble for a chapter file.")
(defconst orgn--are-available-for-this-en-GB "are available for this" "Sentence fragment describing availablility.")
(defconst orgn--chapter-en-GB "chapter" "Part of the preamble for a chapter file.")
(defconst orgn--from-en-GB "from" "Part of the preamble for a chapter file.")
(defconst orgn--content-header-en-GB "Content" "Part of the preamble for a chapter file.")
(defconst orgn--scene-name-here-en-GB "Scene Name Here" "Part of the preamble for a chapter file.")
(defconst orgn--indefinite-article-simple-en-GB "a" "Simple form of the indefinite article.")
(defconst orgn--character-en-GB "character" "Part of the preamble for a character file.")
(defconst orgn--prop-en-GB "prop" "Part of the preamble for a prop file.")
(defconst orgn--place-en-GB "place" "Part of the preamble for a place file.")
(defconst orgn--glossary-header-en-GB "Glossary" "Part of the writing glossary in chapter files.")
(defconst orgn--view-notes-en-GB "View Notes" "A link text to let the user view related notes.")
(defconst orgn--alias-for-en-GB "is an alias for" "Text to let the user know something is an alias.")
(defconst orgn--appearances-in-chapters-header-en-GB "Appearances in Chapters" "Part of the references section in notes files.")
(defconst orgn--line-en-GB "Line" "The word for the line of a chapter. Used at the start of a sentence.")
(defconst orgn--not-yet-referenced-en-GB "Not yet referenced in story." "Display that an object has not yet been mentioned in any of the chapter files.")
(defconst orgn--exports-header-en-GB "Exports" "Heading for configuration file to use to list export tempaltes.")
;; File Content
(defconst orgn--chapter-notes-content-en-GB
  (concat
   "Show how this chapter contributes to:\n"
   "** Character Development\n"
   "** Moving the Plot Forward\n"
   "** Enriching the Setting\n")
  "Starter content for the chapter notes files.")
(defconst orgn--character-notes-content-en-GB
  (concat
   "** Role in Story\n"
   "** What Does This Character Want?\n"
   "** What Would Most Motivate This Character Into Taking Action?\n"
   "** What Would Most Prevent This Character From Taking Action?\n"
   "** What Is The Worst Thing That Could Happen To This Character?\n"
   "** What Is The Best Thing That Could Happen To This Character?\n"
   "** Who or What Is Stopping This Character From Getting What They Want?\n"
   "** What Does This Character Need To Learn In Order To Be Happy?\n"
   "** Occupation\n"
   "** Physical Description\n"
   "** Personality\n"
   "** Habits/Mannerisms\n"
   "** Background\n"
   "** Internal Conflicts\n"
   "** External Conflicts\n"
   "** Notes\n")
  "Starter content for the character notes files.")
(defconst orgn--prop-notes-content-en-GB
  (concat
   "** Role in Story\n"
   "** Description\n"
   "** Background\n"
   "** Notes\n")
  "Starter content for the prop notes files.")
(defconst orgn--place-notes-content-en-GB
  (concat
   "** Role in Story\n"
   "** Description\n"
   "** Background\n"
   "** Related Characters\n"
   "** Season\n"
   "** Unique Features\n"
   "** Sights\n"
   "** Sounds\n"
   "** Smells\n"
   "** Notes\n")
  "Starter content for the place notes files.")
(defconst orgn--matter-type-property-en-GB "ORG-NOVELIST-MATTER-TYPE" "Property key for the chapter matter type in the export property drawer.")
;; User Queries
(defconst orgn--story-name-query-en-GB "Story Name?" "A query to the user for what to name their story.")
(defconst orgn--story-save-location-query-en-GB "Story Save Location?" "A query to the user for where to save their story.")
(defconst orgn--chapter-name-query-en-GB "Chapter Name?" "A query to the user for the name of a chapter.")
(defconst orgn--chapter-location-query-en-GB "Choose Chapter Location From Available Options for \"%s\" (%s/%s/%s):" "A query to the user for what section in which to place a new chapter.")
(defconst orgn--rebuild-chapter-index-location-query-en-GB "Rebuilding index: Where should chapters go?" "When rebuilding chapter index, ask user where to place chapters.")
(defconst orgn--delete-file-query-en-GB "Delete file?" "A query to show the user to see if they want to delete a file.")
(defconst orgn--name-already-in-use-en-GB "That name is already in use. Please try again" "Tell user the chosen name is already in use.")
(defconst orgn--okay-en-GB "Okay" "Positive acknowledgement to the user.")
(defconst orgn--new-chapter-name-query-en-GB "New Chapter Name?" "A query to the user for the new name of a chapter.")
(defconst orgn--character-name-query-en-GB "Character Name?" "A query to the user for what to name a character.")
(defconst orgn--prop-name-query-en-GB "Prop Name?" "A query to the user for what to name a prop.")
(defconst orgn--place-name-query-en-GB "Place Name?" "A query to the user for what to name a place.")
(defconst orgn--new-character-name-query-en-GB "New Character Name?" "A query to the user for the new name of a character.")
(defconst orgn--new-prop-name-query-en-GB "New Prop Name?" "A query to the user for the new name of a prop.")
(defconst orgn--new-place-name-query-en-GB "New Place Name?" "A query to the user for the new name of a place.")
(defconst orgn--new-story-name-query-en-GB "New Story Name?" "A query to the user for the new name for the story.")
(defconst orgn--rename-story-folder-query-en-GB "Rename story folder as well? " "A query to the user as to whether to also rename the story folder.")
;; Error/Throw Messages
(defconst orgn--no-localised-function-en-GB "No localised function found for" "The local language version of the function is missing.")
(defconst orgn--is-not-writable-en-GB "is not writable" "File is not writable.")
(defconst orgn--story-folder-already-in-use-en-GB "That story folder is already in use" "Tell user the selected folder already contains an Org Novelist story.")
(defconst orgn--not-part-of-a-story-folder-en-GB "is not part of an Org Novelist story folder" "Function run from location not appearing to be part of an Org Novelist story.")
(defconst orgn--no-story-found-en-GB "No story found" "No story found in folder.")
(defconst orgn--is-not-readable-en-GB "is not readable" "File is not readable.")
(defconst orgn--new-chapter-created-en-GB "New chapter created" "Throw out of chapter creation loop once chapter created. Not an error.")
(defconst orgn--no-more-headings-en-GB "No more headings" "Throw out of chapter creation loop as section heading not found. Not an error.")
(defconst orgn--file-malformed-en-GB "File malformed" "Throw out of chapter creation function as no top heading. Recoverable error.")
(defconst orgn--file-not-found-en-GB "File not found" "The requested file could not be found.")
(defconst orgn--no-chapters-found-en-GB "No chapters found" "No chapters found in story.")
(defconst orgn--unsaved-buffer-en-GB "Unsaved buffer" "Description of a buffer that is not saved to disk.")
(defconst orgn--no-characters-found-en-GB "No characters found" "No characters found in story.")
(defconst orgn--no-props-found-en-GB "No props found" "No props found in story.")
(defconst orgn--no-places-found-en-GB "No places found" "No places found in story.")
(defconst orgn--unrecognised-index-en-GB "is not a recognised index" "Index is not of a known type.")
;; Pattern Matches
(defconst orgn--sys-safe-name-en-GB "[-A-Za-z0-9]*" "Regexp to match strings produced by `org-novelist--system-safe-name-en-GB'.")
(defconst orgn--aliases-separators-en-GB "[,\f\t\n\r\v]+" "Regexp to match the separators in a list of aliases.")
(defconst orgn--notes-name-search-en-GB "[[:space:][:punct:]]+%s[[:space:][:punct:]]+" "Regexp to match names of things in chapter files.")
(defconst orgn--notes-name-org-link-search-en-GB "\\[\\[:space:\\]\\[:punct:\\]\\]+%s\\[\\[:space:\\]\\[:punct:\\]\\]+" "Regexp to match, from an Org mode link, names of things in chapter files.")
(defconst orgn--folder-already-exists-en-GB "That folder already exists" "Inform user the folder already exists.")

;;;; Internationalised Functions

(defun orgn--system-safe-name-en-GB (str)
  "Convert STR to a directory safe name.
The resultant string should be suitable for all computer systems using en-GB."
  ;; I'm just converting things to CamelCase at the moment, and
  ;; removing non-Latin alphabet characters.
  ;; Make sure that the constant `org-novelist--sys-safe-name-en-GB' matches this.
  (orgn--camelise str))

(defun orgn--camelise-en-GB (str)
  "Convert STR to CamelCase, using only the Latin alphabet.
The resultant string should be suitable for all computer systems using en-GB."
  (let* ((white-list (list "A" "a" "B" "b" "C" "c" "D" "d" "E" "e" "F" "f" "G" "g" "H" "h" "I" "i" "J" "j" "K" "k" "L" "l" "M" "m" "N" "n" "O" "o" "P" "p" "Q" "q" "R" "r" "S" "s" "T" "t" "U" "u" "V" "v" "W" "w" "X" "x" "Y" "y" "Z" "z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" " "))  ; List of allowed characters in folder names, plus the space character. The space will also be removed later on, but we need it in the white list as it will be used as a word separator.
         (taboo-chars (mapcar 'string (string-to-list (orgn--remove-chars white-list str)))))  ; Add characters to this list that are not in the white list
    (mapconcat 'identity (mapcar
                          (lambda (word) (capitalize (downcase word)))
                          (split-string (orgn--remove-chars taboo-chars str) " ")) nil)))
;;;; British English (en-GB) ends here


;;;; Localisation Functions

(defun orgn--localise-string (str-name &rest str-list)
  "Return the correct language version of a string.
More strings can be included with STR-LIST, and the results will be concatenated
into one string. To change the language, the variable
`org-novelist-language-tag' must be set to a supported language for STR-NAME.
The default is \"en-GB\".

Strings matching the values of `org-novelist--folder-separator' or
`org-novelist--file-ending' will be returned without change."
  (catch 'LOCALISATION-STRING-NOT-FOUND
    (unless (boundp 'orgn-language-tag)
      (defvar orgn-language-tag "en-GB" "The language to use for Org Novelist. Based on https://www.w3.org/International/articles/language-tags/index.en"))
    (cond ((string-equal str-name /)  ; Special case for the folder separator string
           (if (> (length str-list) 0)
               (concat (eval /) (apply 'orgn--localise-string str-list))
             (eval /)))
          ((string-equal str-name orgn--file-ending)  ; Special case for the Org Novelist filename ending
           (if (> (length str-list) 0)
               (concat (eval orgn--file-ending) (apply 'orgn--localise-string str-list))
             (eval orgn--file-ending)))
          ((boundp (intern (concat "org-novelist--" str-name "-" orgn-language-tag)))  ; Do not shorten this string to orgn-- as it will prevent running outwith Org Novelist mode
           (if (> (length str-list) 0)
               (concat (eval (intern (concat "org-novelist--" str-name "-" orgn-language-tag))) (apply 'orgn--localise-string str-list))  ; Do not shorten this string to orgn-- as it will prevent running outwith Org Novelist mode.
             (eval (intern (concat "org-novelist--" str-name "-" orgn-language-tag)))))  ; Do not shorten this string to orgn-- as it will prevent running outwith Org Novelist mode
          (t
           ;; The two lines of code below are the only user-facing ones that can't be translated.
           (error (format "No localised string for '%s' found" str-name))
           (throw 'LOCALISATION-STRING-NOT-FOUND (format "No localised string for '%s' found" str-name))))))
(defalias 'orgn--ls 'orgn--localise-string)  ; Make an alias to keep code a little cleaner

(defun orgn--localise-function (func-name)
  "Return the local language version of a function FUNC-NAME.
To change the language, the variable `org-novelist-language-tag' must be set to
a supported language. The default is \"en-GB\"."
  (catch 'LOCALISATION-FUNCTION-NOT-FOUND
    (unless (boundp 'orgn-language-tag)
      (defconst orgn-language-tag "en-GB" "The language to use for Org Novelist (based on https://www.w3.org/International/articles/language-tags/index.en)."))
    (if (fboundp (intern (concat func-name "-" orgn-language-tag)))
        (intern (concat func-name "-" orgn-language-tag))
      (progn
        (error (concat (orgn--ls "no-localised-function") " " func-name))
        (throw 'LOCALISATION-FUNCTION-NOT-FOUND (concat (orgn--ls "no-localised-function") " " func-name))))))
(defalias 'orgn--lf 'orgn--localise-function)  ; Make an alias to keep code a little cleaner


;;;; Customization variables

(defgroup org-novelist nil
  "Helper functions for novel writing with Org mode."
  :tag "Org-Novelist"
  :prefix "org-novelist-"
  :group 'Text
  :group 'Applications)

(defcustom orgn-language-tag "en-GB"
  "The language to use for Org Novelist.
Based on https://www.w3.org/International/articles/language-tags/index.en
A corresponding language pack must be included with Org Novelist."
  :group 'org-novelist
  :type 'string)

(defcustom orgn-author (orgn--ls "author-not-set")
  "The author name you wish to appear on your stories."
  :group 'org-novelist
  :type 'string)

(defcustom orgn-author-email (orgn--ls "author-email-not-set")
  "The contact email you wish to appear on your stories."
  :group 'org-novelist
  :type 'string)

(defcustom orgn-automatic-referencing-p nil
  "Set to t for Org Novelist to automatically update all cross-references."
  :group 'org-novelist
  :type 'boolean)


;;;; String Manipulation Worker Functions

(defun orgn--replace-string-in-string (old-str new-str content)
  "Given a string, CONTENT, replace any occurrences of OLD-STR with NEW-STR."
  ;; This function was written as a non-regexp version of (replace-regexp-in-string REGEXP REP STRING).
  (unless old-str
    (setq old-str ""))
  (unless new-str
    (setq new-str ""))
  (unless content
    (setq content ""))
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (search-forward old-str nil t)
      (replace-match new-str nil t))
    (buffer-string)))

(defun orgn--remove-chars (char-list str)
  "Remove all instances of characters in CHAR-LIST from STR."
  (let (ch)
    (while char-list
      (setq ch (car char-list))
      (setq char-list (cdr char-list))
      (setq str (orgn--replace-string-in-string ch "" str)))
    str))

(defun orgn--camelise (str)
  "Convert STR to CamelCase, only using characters that are allowed in filenames."
  ;; Do not shorten this string to orgn--camelise as it will prevent running outwith Org Novelist mode.
  (funcall (orgn--lf "org-novelist--camelise") str))

(defun orgn--system-safe-name (str)
  "Convert STR to a directory safe name.
The resultant string should be suitable for the current operating system."
  (funcall (orgn--lf "org-novelist--system-safe-name") str))

(defun orgn--sanitize-string (str)
  "Given a string, STR, remove characters that might cause processing problems."
  (setq str (orgn--replace-string-in-string "\\" "\\\\" str))  ; Escape any backslashs
  (setq str (orgn--replace-string-in-string "\"" "\\\"" str))  ; Escape any quotes (this must be run after escaping backslashes)
  str)

(defun orgn--fold-show-all ()
  "Run the deprecated org-show-all when Org version is less than 9.6.
Otherwise, run org-fold-show-all."
  (if (and (>= (string-to-number (nth 0 (split-string (org-version) "\\."))) 9)
           (>= (string-to-number (nth 1 (split-string (org-version) "\\."))) 6))
      (org-fold-show-all)
    (org-show-all)))

(defun orgn--delete-line ()
  "If Emacs version is less than 29, delete line the old fashioned way."
  (let ((inhibit-field-text-motion t))
    (if (>= (string-to-number (nth 0 (split-string (string-trim-left (emacs-version) "GNU Emacs ") "\\."))) 29)
        (delete-line)
      (delete-region (line-beginning-position) (line-beginning-position 2)))))


;;;; File Manipulation Worker Functions

(defun orgn--string-to-file (str filename)
  "Create/Overwrite FILENAME with the contents of STR."
  (catch 'FILE-NOT-WRITABLE
    (let ((current-buffer (current-buffer)))
      (if (get-file-buffer filename)
          (progn
            ;; Filename already open in a buffer. Update buffer and save.
            (switch-to-buffer (get-file-buffer filename))
            (erase-buffer)
            (insert str)
            (save-buffer)  ; Calling `save-buffer' with an argument of 0 would stop back-up files being created, but it's probably best to respect the user's Emacs setup in this regard
            (switch-to-buffer current-buffer))
        (progn
          ;; Filename not open in a buffer. Just deal with file.
          (with-temp-buffer
            (insert str)
            ;; If directory doesn't exist, create it.
            (unless (file-exists-p (file-name-directory filename))
              (make-directory (file-name-directory filename) t))
            (if (file-writable-p filename)
                (write-region (point-min) (point-max) filename)
              (progn
                (error (concat filename " " (orgn--ls "is-not-writable")))
                (throw 'FILE-NOT-WRITABLE (concat filename " " (orgn--ls "is-not-writable")))))))))))

(defun orgn--generate-file-from-template (substitutions template filename)
  "Generate a new file from TEMPLATE string and SUBSTITUTIONS hash table.
The new file, FILENAME, will be saved to disk."
  (orgn--string-to-file (orgn--generate-string-from-template substitutions template) filename))

(defun orgn--generate-string-from-template (substitutions template)
  "Generate a new string from TEMPLATE string and SUBSTITUTIONS hash table."
  (let ((keys (hash-table-keys substitutions))
        key)
    (while keys
      (setq key (pop keys))
      (setq template (orgn--replace-string-in-string key (gethash key substitutions) template)))
    template))

(defun orgn--replace-true-headline-in-org-heading (new-headline org-heading-components)
  "Given an ORG-HEADING-COMPONENTS, replace the true headline with NEW-HEADLINE."
  (let* ((stars (nth 1 org-heading-components))
         (stars-str "")
         (todo-str (nth 2 org-heading-components))
         (priority-str nil)
         ;; (true-headline-str (nth 4 org-heading-components))
         (tags-str (nth 5 org-heading-components))
         (output-str ""))
    (while (> stars 0)
      (setq stars (- stars 1))
      (setq stars-str (concat stars-str "*")))
    (when (nth 3 org-heading-components)
      (setq priority-str (concat "[#" (char-to-string (nth 3 org-heading-components)) "]")))
    (setq output-str stars-str)
    (when todo-str
      (setq output-str (concat output-str " " todo-str)))
    (when priority-str
      (setq output-str (concat output-str " " priority-str)))
    (setq output-str (concat output-str " " new-headline))
    (when tags-str
      (setq output-str (concat output-str " " tags-str)))
    output-str))

(defun orgn--story-root-folder (&optional folder)
  "Return the Org Novelist story's root folder.
If no FOLDER string is supplied, use the current buffer's file location. If
the given folder or current buffer is not part of an Org Novelist story folder,
throw an error."
  (catch 'NOT-A-STORY-FOLDER
    (let (current-folder)
      (if (string= " *temp*" (buffer-file-name))
          (eval nil)
        (progn
          (unless folder
            (if (or load-file-name buffer-file-name)
                (setq folder (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
              (progn
                (user-error (concat (orgn--ls "unsaved-buffer") " " (orgn--ls "not-part-of-a-story-folder")))
                (throw 'NOT-A-STORY-FOLDER (concat (orgn--ls "unsaved-buffer") " " (orgn--ls "not-part-of-a-story-folder"))))))
          (setq current-folder folder)
          (while (not (file-exists-p (concat current-folder / orgn--config-filename)))
            (when (string= current-folder (setq current-folder (expand-file-name (concat folder / ".." ))))
              (user-error (concat folder " " (orgn--ls "not-part-of-a-story-folder")))
              (throw 'NOT-A-STORY-FOLDER (concat folder " " (orgn--ls "not-part-of-a-story-folder")))))
          (expand-file-name current-folder))))))

(defun orgn--story-name (&optional story-folder)
  "Return the Org Novelist story's name.
If no STORY-FOLDER is supplied, try to determine the name for Org Novelist story
related to the current buffer."
  (catch 'CONFIG-MISSING
    (let (story-name)
      (if (not story-folder)
          (setq story-folder (orgn--story-root-folder))
        (setq story-folder (orgn--story-root-folder story-folder)))
      (with-temp-buffer
        (if (file-exists-p (concat story-folder / (orgn--ls "main-file" orgn--file-ending)))
            (if (file-readable-p (concat story-folder / (orgn--ls "main-file" orgn--file-ending)))
                (progn
                  (insert-file-contents (concat story-folder / (orgn--ls "main-file" orgn--file-ending)))
                  (goto-char (point-min))  ; Move point to start of buffer
                  (org-novelist-mode)  ; If not explicitly in Org mode (or a derivative), then org-heading-components won't work in the temp buffer
                  (orgn--fold-show-all)  ; Belts and braces
                  (if (org-goto-first-child)  ; Get the first heading in buffer
                      (setq story-name (nth 4 (org-heading-components)))  ; Extract just the heading text without other Org stuff
                    (progn
                      (error (orgn--ls "no-story-found"))
                      (throw 'CONFIG-MISSING (orgn--ls "no-story-found")))))
              (progn
                (error (concat (orgn--ls "main-file" orgn--file-ending) " " (orgn--ls "is-not-readable")))
                (throw 'CONFIG-MISSING (concat (orgn--ls "main-file" orgn--file-ending) " " (orgn--ls "is-not-readable")))))
          (progn
            (error (orgn--ls "no-story-found"))
            (throw 'CONFIG-MISSING (orgn--ls "no-story-found")))))
      story-name)))

(defun orgn--set-story-name (new-story-name &optional story-folder)
  "Set the Org Novelist story's name in the main entry point file.
NEW-STORY-NAME will be used as the new story name.
If no STORY-FOLDER is supplied, try to determine the name for Org Novelist story
related to the current buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (catch 'SET-STORY-NAME-FAILURE
    (let (org-heading-components
          beg)
      (with-temp-buffer
        (if (file-exists-p (concat story-folder / (orgn--ls "main-file" orgn--file-ending)))
            (if (file-readable-p (concat story-folder / (orgn--ls "main-file" orgn--file-ending)))
                (progn
                  (insert-file-contents (concat story-folder / (orgn--ls "main-file" orgn--file-ending)))
                  (goto-char (point-min))  ; Move point to start of buffer
                  (org-novelist-mode)  ; If not explicitly in Org mode (or a derivative), then org-heading-components won't work in the temp buffer
                  (orgn--fold-show-all)  ; Belts and braces
                  (if (org-goto-first-child)  ; Get the first heading in buffer
                      (progn
                        (setq org-heading-components (org-heading-components))  ; Extract heading components
                        (beginning-of-line)
                        (setq beg (point))
                        (end-of-line)
                        (delete-region beg (point))
                        (insert (org-novelist--replace-true-headline-in-org-heading new-story-name org-heading-components))
                        (orgn--string-to-file (buffer-string) (concat story-folder / (orgn--ls "main-file" orgn--file-ending))))
                    (progn
                      (error (orgn--ls "no-story-found"))
                      (throw 'SET-STORY-NAME-FAILURE (orgn--ls "no-story-found")))))
              (progn
                (error (concat (orgn--ls "main-file" orgn--file-ending) " " (orgn--ls "is-not-readable")))
                (throw 'SET-STORY-NAME-FAILURE (concat (orgn--ls "main-file" orgn--file-ending) " " (orgn--ls "is-not-readable")))))
          (progn
            (error (orgn--ls "no-story-found"))
            (throw 'SET-STORY-NAME-FAILURE (orgn--ls "no-story-found"))))))))

(defun orgn--delete-current-file (&optional no-prompt)
  "Delete the file associated with the current buffer.
Kill the current buffer too. If no file is associated, just kill buffer without
prompt for save. If NO-PROMPT is non-nil, don't ask user for confirmation."
  (let ((current-file (buffer-file-name)))
    (if no-prompt
        (progn
          (set-buffer-modified-p nil)
          (kill-buffer (current-buffer))
          (when current-file
            (delete-file current-file)))
      (when (yes-or-no-p (concat (orgn--ls "delete-file-query") " " current-file " "))
        (kill-buffer (current-buffer))
        (when current-file
          (delete-file current-file))))))

(defun orgn--rename-current-file (new-file &optional no-prompt)
  "Rename the file associated with the current buffer to NEW-FILE.
If no file is associated, inform the user. If NO-PROMPT is non-nil, don't ask
user for confirmation if new file name already in use."
  (let ((current-file (buffer-file-name)))
    (when current-file
      (save-buffer)
      (if no-prompt
          (progn
            (rename-file current-file new-file t))
        (progn
          (rename-file current-file new-file 1)))
      (kill-buffer (current-buffer))
      (find-file new-file))))

(defun orgn--save-current-file (&optional file)
  "Save the current buffer to its associated file.
If no file associated with current buffer, do nothing.
If passed a FILE, see if their is a matching buffer and save it."
  (let ((current-file (buffer-file-name))
        (current-buffer (current-buffer)))
    (if file
        (progn
          (switch-to-buffer (get-file-buffer file))
          (save-buffer)
          (switch-to-buffer current-buffer))
      (progn
        (when current-file
          (save-buffer))))))

(defun orgn--make-chapter-at-index-point (chapter-name)
  "Create a new chapter file and link to it from the current point.
CHAPTER-NAME should be the name of the chapter. The new chapter file will also
have a dedicated notes file linked from it."
  (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
         (chapter-file (concat (orgn--ls "chapter-file-prefix") (orgn--system-safe-name chapter-name)))
         (story-name (orgn--story-name story-folder)))
    (insert (format "\[\[file:../%s/%s\]\[%s\]\]" (orgn--ls "chapters-folder") (concat chapter-file orgn--file-ending) chapter-name))
    (orgn--populate-chapter-template story-name story-folder chapter-file chapter-name)
    (orgn--populate-chapter-notes-template story-name story-folder chapter-file chapter-name)))

(defun orgn--make-character-at-index-point (character-name)
  "Create a new character file and link to it from the current point.
CHARACTER-NAME should be the name of the character."
  (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
         (character-file (concat (orgn--ls "character-file-prefix") (orgn--system-safe-name character-name)))
         (story-name (orgn--story-name story-folder)))
    (insert (format "\[\[file:../%s/%s\]\[%s\]\]" (orgn--ls "notes-folder") (concat character-file orgn--file-ending) character-name))
    (orgn--populate-character-notes-template story-name story-folder character-file character-name)))

(defun orgn--make-prop-at-index-point (prop-name)
  "Create a new prop file and link to it from the current point.
PROP-NAME should be the name of the prop."
  (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
         (prop-file (concat (orgn--ls "prop-file-prefix") (orgn--system-safe-name prop-name)))
         (story-name (orgn--story-name story-folder)))
    (insert (format "\[\[file:../%s/%s\]\[%s\]\]" (orgn--ls "notes-folder") (concat prop-file orgn--file-ending) prop-name))
    (orgn--populate-prop-notes-template story-name story-folder prop-file prop-name)))

(defun orgn--make-place-at-index-point (place-name)
  "Create a new place file and link to it from the current point.
PLACE-NAME should be the name of the place."
  (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
         (place-file (concat (orgn--ls "place-file-prefix") (orgn--system-safe-name place-name)))
         (story-name (orgn--story-name story-folder)))
    (insert (format "\[\[file:../%s/%s\]\[%s\]\]" (orgn--ls "notes-folder") (concat place-file orgn--file-ending) place-name))
    (orgn--populate-place-notes-template story-name story-folder place-file place-name)))

(defun orgn--make-glossary-string (story-folder)
  "Create a new glossary for STORY-FOLDER."
  (setq story-folder (orgn--story-root-folder story-folder))
  (when story-folder
    (orgn--populate-glossary-string story-folder)))

(defun orgn--make-file-chapter-references-string (file &optional story-folder)
  "Create a new list of references in an Org Novelist story for FILE.
If no STORY-FOLDER is supplied, try to determine the name for Org Novelist story
related to the current buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (catch 'MAKE-REFERENCES-WORKER-FAULT
    (if (file-exists-p file)
        (let* ((chapters-folder (orgn--ls "chapters-folder"))
               (indices-folder (orgn--ls "indices-folder"))
               (chapter-index (concat (orgn--ls "chapters-file") orgn--file-ending))
               (aliases-str (orgn--get-file-property-value file "aliases"))
               names
               curr-names
               name
               (file-chapters (orgn--chapter-hash-table story-folder))
               (chapter-keys (sort (hash-table-keys file-chapters) #'string<))
               chapter-key
               (chapter-index-files '())
               (regexp (orgn--ls "notes-name-search"))
               (regexp-org-link (orgn--ls "notes-name-org-link-search"))
               found-aliases
               found-alias-keys
               found-alias-key
               (chap-ref-content "")
               (output-str "")
               (prepped-file-contents ""))
          (when aliases-str
            (setq names (sort (split-string aliases-str (orgn--ls "aliases-separators") t " ") 'string<)))
          (setq names (cons (orgn--get-file-property-value file "TITLE") names))
          (unless (equal names '(nil))
            ;; Make sure order of chapter-keys matches the chapter index order, with any unknown files at the end in alphabetical order.
            (orgn--reorder-matter-in-chapter-index story-folder)
            (with-temp-buffer
              (insert-file-contents (concat story-folder / indices-folder / chapter-index))
              (goto-char (point-min))
              (insert "\n")
              (goto-char (point-min))
              (org-novelist-mode)
              (orgn--fold-show-all)  ; Belts and braces
              (while (not (org-next-visible-heading 1))
                (when (or (string= (orgn--ls "front-matter-heading") (nth 4 (org-heading-components)))
                          (string= (orgn--ls "main-matter-heading") (nth 4 (org-heading-components)))
                          (string= (orgn--ls "back-matter-heading") (nth 4 (org-heading-components))))
                  (when (org-goto-first-child)
                    (setq chapter-index-files (cons (file-name-nondirectory (orgn--heading-last-link-absolute-link-text)) chapter-index-files))
                    (while (org-goto-sibling)
                      (setq chapter-index-files (cons (file-name-nondirectory (orgn--heading-last-link-absolute-link-text)) chapter-index-files))))))
              (goto-char (point-min))
              (orgn--delete-line)
              (setq chapter-index-files (reverse chapter-index-files)))
            (setq chapter-keys (delq nil (delete-dups (append chapter-index-files chapter-keys))))
            ;; Go through chapters for each name and construct output string.
            (while chapter-keys
              (setq curr-names names)
              (setq chapter-key (pop chapter-keys))
              (setq chap-ref-content "")
              (setq found-aliases (make-hash-table :test 'equal))
              (if (file-exists-p (concat story-folder / chapters-folder / chapter-key))
                  (if (file-readable-p (concat story-folder / chapters-folder / chapter-key))
                      (progn
                        (when (file-exists-p (concat story-folder / chapters-folder / chapter-key))
                          (when (file-readable-p (concat story-folder / chapters-folder / chapter-key))
                            (when (get-file-buffer (concat story-folder / chapters-folder / chapter-key))
                              (switch-to-buffer (get-file-buffer (concat story-folder / chapters-folder / chapter-key)))
                              (save-buffer))))
                        ;; Removing the glossary is necessary to prevent extraneous references.
                        (setq prepped-file-contents (orgn--delete-org-subtrees-from-string
                                                     (orgn--ls "glossary-header")
                                                     (org-file-contents (concat story-folder / chapters-folder / chapter-key))))
                        (with-temp-buffer
                          (insert prepped-file-contents)
                          (org-novelist-mode)
                          (orgn--fold-show-all)  ; Belts and braces
                          (while curr-names
                            (setq name (string-trim (pop curr-names)))
                            (goto-char (point-min))
                            (while (re-search-forward (format regexp name) nil t)
                              (backward-word)
                              (when (thing-at-point 'sentence t)
                                (puthash (line-number-at-pos) (thing-at-point 'sentence t) found-aliases))
                              (forward-word))))
                        ;; We should now have a hash table of found references
                        (with-temp-buffer
                          (setq found-alias-keys (sort (hash-table-keys found-aliases) '<))
                          (while found-alias-keys
                            (setq found-alias-key (pop found-alias-keys))
                            (setq chap-ref-content (concat chap-ref-content "\n- \[\[file:.." / chapters-folder / chapter-key
                                                           "::" (number-to-string found-alias-key) "\]\["
                                                           (orgn--ls "line") " " (number-to-string found-alias-key)
                                                           ": \"" (gethash found-alias-key found-aliases) "\"\]\]")))))
                    (progn
                      (error (concat story-folder / chapters-folder / chapter-key " " (orgn--ls "is-not-readable")))
                      (throw 'MAKE-REFERENCES-WORKER-FAULT (concat story-folder / chapters-folder / chapter-key " " (orgn--ls "is-not-readable")))))
                (progn
                  (error (concat (orgn--ls "file-not-found") ": " story-folder / chapters-folder / chapter-key))
                  (throw 'MAKE-REFERENCES-WORKER-FAULT (concat (orgn--ls "file-not-found") ": " story-folder / chapters-folder / chapter-key))))
              (unless (string= "" chap-ref-content)
                ;; If hash entries exist, add chapter header and content for this chapter to output string
                (setq output-str (concat output-str "\n** \[\[file:.." / chapters-folder / chapter-key
                                         "::/" (format regexp-org-link name) "/\]\["
                                         (gethash chapter-key file-chapters) "\]\]"
                                         chap-ref-content)))))
          ;; If output string is nil, setup Appearances header with 'not yet used'
          ;; ;; otherwise, setup appearance header with contents
          (if (string= "" output-str)
              (setq output-str (concat "* " (orgn--ls "appearances-in-chapters-header") "\n" (orgn--ls "not-yet-referenced")))
            (setq output-str (concat "* " (orgn--ls "appearances-in-chapters-header") output-str)))
          (eval output-str))
      (progn
        (error (concat (orgn--ls "file-not-found") ": " file))
        (throw 'MAKE-REFERENCES-WORKER-FAULT (concat (orgn--ls "file-not-found") ": " file))))))

(defun orgn--object-hash-table (file-prefix file-folder &optional story-folder)
  "Return a hash table of known objects in FILE-FOLDER, based on FILE-PREFIX.
Function will try to use story that current file is a part of, unless called
with STORY-FOLDER to override that behaviour. FILE-FOLDER should be the
relative location of either the chapters or notes folder within an Org Novelist
story folder.
This function is based on files, not indices.
The returned hash table will use filenames as keys, and object titles as
values."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  ;; If org-novelist--story-root-folder didn't throw any errors, we should be good to go.
  (unless (file-directory-p (concat story-folder / file-folder))
    (make-directory (concat story-folder / file-folder) t))
  (let ((files (directory-files-recursively (concat story-folder / file-folder) (format "^%s%s%s\\'" file-prefix (orgn--ls "sys-safe-name") orgn--file-ending)))
        curr-file
        (names (make-hash-table :test 'equal))
        curr-name)
    ;; Open each file in list and discover object title. Return all titles as a new list to finish function.
    (while files
      (setq curr-file (car files))
      (setq files (cdr files))
      (if (setq curr-name (orgn--get-file-property-value curr-file "TITLE"))
          (puthash (file-name-nondirectory curr-file) curr-name names)
        (puthash (file-name-nondirectory curr-file) (file-name-nondirectory curr-file) names)))
    (eval names)))

(defun orgn--chapter-hash-table (&optional story-folder)
  "Return a hash table of known chapters in a story.
Function will try to use story that current file is a part of, unless called
with STORY-FOLDER to override that behaviour.
This function is based on files, not indices.
The returned hash table will use filenames as keys, and chapter titles as
values."
  (orgn--object-hash-table (orgn--ls "chapter-file-prefix") (orgn--ls "chapters-folder") story-folder))

(defun orgn--character-hash-table (&optional story-folder)
  "Return a hash table of known characters in a story.
Function will try to use story that current file is a part of, unless called
with STORY-FOLDER to override that behaviour.
This function is based on files, not indices.
The returned hash table will use filenames as keys, and character titles as
values."
  (orgn--object-hash-table (orgn--ls "character-file-prefix") (orgn--ls "notes-folder") story-folder))

(defun orgn--prop-hash-table (&optional story-folder)
  "Return a hash table of known props in a story.
Function will try to use story that current file is a part of, unless called
with STORY-FOLDER to override that behaviour.
This function is based on files, not indices.
The returned hash table will use filenames as keys, and prop titles as
values."
  (orgn--object-hash-table (orgn--ls "prop-file-prefix") (orgn--ls "notes-folder") story-folder))

(defun orgn--place-hash-table (&optional story-folder)
  "Return a hash table of known places in a story.
Function will try to use story that current file is a part of, unless called
with STORY-FOLDER to override that behaviour.
This function is based on files, not indices.
The returned hash table will use filenames as keys, and place titles as
values."
  (orgn--object-hash-table (orgn--ls "place-file-prefix") (orgn--ls "notes-folder") story-folder))

(defun orgn--set-file-property-value (property value file)
  "Given a FILE and VALUE, change PROPERTY value of that file.
If property not found, add it."
  (when (file-exists-p file)
    (when (file-readable-p file)
      (find-file file)
      (let* ((regexp (format "^[ \t]*#\\+%s:" (regexp-quote property)))
             (case-fold-search t)
             (property-found-p nil))
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (setq property-found-p t)
          (insert " ")
          (delete-region (point) (line-end-position))
          (insert value))
        (unless property-found-p
          (goto-char (point-min))
          (end-of-line)
          (insert (format "\n\#\+%s\: %s" property value)))))))

(defun orgn--get-file-property-value (file property)
  "Given a FILE, return the value of PROPERTY."
  (let ((value "")
        (regexp (format "^[ \t]*#\\+%s:" (regexp-quote property)))
        (case-fold-search t)
        beg)
    (with-temp-buffer
      (when (file-exists-p file)
        (when (file-readable-p file)
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (when (looking-at-p " ")
              (forward-char))
            (setq beg (point))
            (end-of-line)
            (setq value (org-trim (buffer-substring beg (point))))))))
    value))

(defun orgn--delete-file-property-value (file property)
  "Given a FILE, delete the entry for PROPERTY."
  ;; This function is currently not used (as of version 0.0.1). I coded it up thinking I'd need it, but so far it's not come up.
  (let ((regexp (format "^[ \t]*#\\+%s:" (regexp-quote property)))
        (case-fold-search t))
    (with-temp-buffer
      (when (file-exists-p file)
        (when (file-readable-p file)
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (beginning-of-line)
            (orgn--delete-line))))
      (orgn--string-to-file (buffer-string) file))))

(defun orgn--get-file-properties (file)
  "Given a FILE, return the properties."
  (let ((property-list '())
        (regexp-start "^[ \t]*#\\+")
        (regexp-end ": ")
        (case-fold-search t)
        beg
        beg-line-num)
    (with-temp-buffer
      (when (file-exists-p file)
        (when (file-readable-p file)
          (insert-file-contents file)
          (goto-char (point-min))
          (while (re-search-forward regexp-start nil t)
            (setq beg (point))
            (setq beg-line-num (line-number-at-pos))
            (when (re-search-forward regexp-end nil t)
              (when (= beg-line-num (line-number-at-pos))
                (forward-char -2)
                (setq property-list (cons (org-trim (buffer-substring beg (point))) property-list))))))))
    property-list))

(defun orgn--get-file-subtree (file header &optional no-header)
  "Given a FILE, and HEADER, return the contents of the header's subtree.
If NO-HEADER is non-nil, don't include header line in output."
  (let (beg
        (output-str ""))
    (with-temp-buffer
      (when (file-exists-p file)
        (when (file-readable-p file)
          (insert-file-contents file)
          (goto-char (point-min))
          (insert "\n")
          (goto-char (point-min))
          (org-novelist-mode)
          (orgn--fold-show-all)  ; Belts and braces
          (while (not (org-next-visible-heading 1))
            (when (string= header (nth 4 (org-heading-components)))
              (when no-header
                (forward-line))
              (beginning-of-line)
              (setq beg (point))
              (when no-header
                (forward-line -1))
              (org-end-of-subtree t t)
              ;; Include the end of an inlinetask
              (when (and (featurep 'org-inlinetask)
                         (looking-at-p (concat (org-inlinetask-outline-regexp) "END[ \t]*$")))
                (end-of-line))
              (setq output-str (buffer-substring beg (point)))
              (goto-char (point-max)))))))
    output-str))

(defun orgn--heading-last-link-headline-text ()
  "Return the headline text of the heading link at point.
If no link, return the full headline text.
Assumes you are in Org mode, or a mode derived from it.
Whitespace is trimmed from results."
  (car (last (split-string (nth 4 (org-heading-components)) "[\]\[]+" t "[ ]+"))))

(defun orgn--heading-last-link-absolute-link-text ()
  "Return the link text of the heading link at point.
If no link, return the full headline text.
Assumes you are in Org mode, or a mode derived from it.
Whitespace is trimmed from results."
  (expand-file-name (concat ".." (car (split-string (nth 4 (org-heading-components)) "[\]\[]+" t "file\:..")))))

(defun orgn--get-all-story-chapters-headlines (&optional story-folder)
  "Return a list of all chapters' headlines in the story.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (catch 'CHAPTER-HEADLINES-FAULT
    (let* ((chapters-file (orgn--ls "chapters-file" orgn--file-ending))
           (indices-folder (orgn--ls "indices-folder"))
           (file-chapters (orgn--chapter-hash-table story-folder))
           (keys (hash-table-keys file-chapters))
           key
           (chapters-headlines '()))
      ;; We have the chapter list from looking at the files.
      ;; Now add missing entries to it from looking at the index.
      (with-temp-buffer
        (if (file-exists-p (concat story-folder / indices-folder / chapters-file))
            (if (file-readable-p (concat story-folder / indices-folder / chapters-file))
                (progn
                  (insert-file-contents (concat story-folder / indices-folder / chapters-file))
                  (goto-char (point-min))
                  (org-novelist-mode)
                  (orgn--fold-show-all)  ; Belts and braces
                  (org-next-visible-heading 1)
                  (while (not (org-next-visible-heading 1))
                    (setq chapters-headlines (cons (orgn--heading-last-link-headline-text) chapters-headlines)))
                  ;; Top level heading already removed, just need to remove the three matter types, and any duplicates or nils,
                  ;; then compare the lists and add missing.
                  (setq chapters-headlines (delete (org-novelist--ls "front-matter-heading") chapters-headlines))
                  (setq chapters-headlines (delete (org-novelist--ls "main-matter-heading") chapters-headlines))
                  (setq chapters-headlines (delete (org-novelist--ls "back-matter-heading") chapters-headlines))
                  (while keys
                    (setq key (pop keys))
                    (setq chapters-headlines (cons (gethash key file-chapters) chapters-headlines)))
                  (setq chapters-headlines (delq nil (delete-dups chapters-headlines))))
              (progn
                (error (concat story-folder / indices-folder / chapters-file " " (orgn--ls "is-not-readable")))
                (throw 'CHAPTER-HEADLINES-FAULT (concat story-folder / indices-folder / chapters-file " " (orgn--ls "is-not-readable")))))
          (progn
            (error (concat (orgn--ls "file-not-found") ": " story-folder / indices-folder / chapters-file))
            (throw 'CHAPTER-HEADLINES-FAULT (concat (orgn--ls "file-not-found") ": " story-folder / indices-folder / chapters-file))))))))

(defun orgn--get-all-story-headlines-for-object-type (index-filename hash-table-func &optional story-folder)
  "Return a list of all headlines in the story for a particular object.
INDEX-FILENAME should be the name of the object index.
HASH-TABLE-FUNC should be the name of the function to get this object's
headlines as a hash table linked to the files.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (catch 'OBJECT-HEADLINES-FAULT
    (let* ((index-file (orgn--ls index-filename orgn--file-ending))
           (indices-folder (orgn--ls "indices-folder"))
           (file-objects (funcall (intern hash-table-func) story-folder))
           (keys (hash-table-keys file-objects))
           key
           (objects-headlines '()))
      ;; We have the character list from looking at the files.
      ;; Now add missing entries to it from looking at the index.
      (with-temp-buffer
        (if (file-exists-p (concat story-folder / indices-folder / index-file))
            (if (file-readable-p (concat story-folder / indices-folder / index-file))
                (progn
                  (insert-file-contents (concat story-folder / indices-folder / index-file))
                  (goto-char (point-min))
                  (org-novelist-mode)
                  (orgn--fold-show-all)  ; Belts and braces
                  (org-next-visible-heading 1)
                  (while (not (org-next-visible-heading 1))
                    (setq objects-headlines (cons (orgn--heading-last-link-headline-text) objects-headlines)))
                  ;; Top level heading already removed, just need to remove any duplicates or nils,
                  ;; then compare the lists and add missing.
                  (while keys
                    (setq key (pop keys))
                    (setq objects-headlines (cons (gethash key file-objects) objects-headlines)))
                  (setq objects-headlines (delq nil (delete-dups objects-headlines))))
              (progn
                (error (concat story-folder / indices-folder / index-file " " (orgn--ls "is-not-readable")))
                (throw 'OBJECT-HEADLINES-FAULT (concat story-folder / indices-folder / index-file " " (orgn--ls "is-not-readable")))))
          (progn
            (error (concat (orgn--ls "file-not-found") ": " story-folder / indices-folder / index-file))
            (throw 'OBJECT-HEADLINES-FAULT (concat (orgn--ls "file-not-found") ": " story-folder / indices-folder / index-file))))))))

(defun orgn--get-all-story-characters-headlines (&optional story-folder)
  "Return a list of all characters' headlines in the story.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--get-all-story-headlines-for-object-type "characters-file" "org-novelist--character-hash-table" story-folder))

(defun orgn--get-all-story-props-headlines (&optional story-folder)
  "Return a list of all props' headlines in the story.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--get-all-story-headlines-for-object-type "props-file" "org-novelist--prop-hash-table" story-folder))

(defun orgn--get-all-story-places-headlines (&optional story-folder)
  "Return a list of all places' headlines in the story.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--get-all-story-headlines-for-object-type "places-file" "org-novelist--place-hash-table" story-folder))

(defun orgn--delete-chapter-from-index (chapter-name &optional story-folder)
  "Remove CHAPTER-NAME and its subtree from the chapter index file.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--delete-org-subtrees-from-file
   (concat "\[\[file:.." (orgn--ls / "chapters-folder" / "chapter-file-prefix")
           (orgn--system-safe-name (orgn--sanitize-string chapter-name)) orgn--file-ending
           "\]\[" chapter-name "\]\]")
   (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls "chapters-file" orgn--file-ending))))

(defun orgn--delete-character-from-index (character-name &optional story-folder)
  "Remove CHARACTER-NAME and its subtree from the character index file.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--delete-org-subtrees-from-file
   (concat "\[\[file:.." (orgn--ls / "notes-folder" / "character-file-prefix")
           (orgn--system-safe-name (orgn--sanitize-string character-name)) orgn--file-ending
           "\]\[" character-name "\]\]")
   (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls "characters-file" orgn--file-ending))))

(defun orgn--delete-prop-from-index (prop-name &optional story-folder)
  "Remove PROP-NAME and its subtree from the prop index file.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--delete-org-subtrees-from-file
   (concat "\[\[file:.." (orgn--ls / "notes-folder" / "prop-file-prefix")
           (orgn--system-safe-name (orgn--sanitize-string prop-name)) orgn--file-ending
           "\]\[" prop-name "\]\]")
   (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls "props-file" orgn--file-ending))))

(defun orgn--delete-place-from-index (place-name &optional story-folder)
  "Remove PLACE-NAME and its subtree from the place index file.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--delete-org-subtrees-from-file
   (concat "\[\[file:.." (orgn--ls / "notes-folder" / "place-file-prefix")
           (orgn--system-safe-name (orgn--sanitize-string place-name)) orgn--file-ending
           "\]\[" place-name "\]\]")
   (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls "places-file" orgn--file-ending))))

(defun orgn--delete-org-subtrees-from-string (subtree-heading org-str)
  "Remove all subtrees with SUBTREE-HEADING from string ORG-STR."
  (with-temp-buffer
    (insert org-str)
    (goto-char (point-min))
    (insert "\n")
    (goto-char (point-min))
    (org-novelist-mode)
    (orgn--fold-show-all)  ; Belts and braces
    (while (not (org-next-visible-heading 1))
      (when (string= subtree-heading (nth 4 (org-heading-components)))
        (org-back-to-heading t)
        (org-mark-subtree)
        (delete-region (point) (mark))
        (goto-char (point-min))))
    (goto-char (point-min))
    (orgn--delete-line)
    (buffer-string)))

(defun orgn--delete-org-subtrees-from-file (subtree-heading file)
  "Remove all subtrees with SUBTREE-HEADING from an Org FILE."
  (catch 'SUBTREE-DELETION-FROM-FILE-FAULT
    (let ((story-folder (orgn--story-root-folder (file-name-directory file))))
      (when story-folder
        (if (file-exists-p file)
            (if (file-readable-p file)
                (orgn--string-to-file (orgn--delete-org-subtrees-from-string subtree-heading (org-file-contents file)) file)
              (progn
                (error (concat file " " (orgn--ls "is-not-readable")))
                (throw 'SUBTREE-DELETION-FROM-FILE-FAULT (concat file " " (orgn--ls "is-not-readable")))))
          (progn
            (error (concat (orgn--ls "file-not-found") ": " file))
            (throw 'SUBTREE-DELETION-FROM-FILE-FAULT (concat (orgn--ls "file-not-found") ": " file))))))))

(defun orgn--delete-chapter-files-for (chapter-name &optional story-folder)
  "Delete chapter files associated with CHAPTER-NAME, including its notes.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (catch 'CHAPTER-FILE-DELETION-FAULT
    (let* ((file-chapters (orgn--chapter-hash-table story-folder))
           (keys (hash-table-keys file-chapters))
           key
           chapter-file
           chapter-notes-file)
      (while keys
        (setq key (pop keys))
        (when (string= chapter-name (gethash key file-chapters))
          (setq chapter-file (concat story-folder / (orgn--ls "chapters-folder") / key))
          (setq chapter-notes-file (concat story-folder / (orgn--ls "notes-folder") / (orgn--ls "chapter-file-prefix") (orgn--system-safe-name chapter-name) (orgn--ls "notes-suffix") orgn--file-ending))
          (if (file-exists-p chapter-file)
              (progn
                (find-file chapter-file)
                (orgn--delete-current-file))
            (progn
              (error (concat (orgn--ls "file-not-found") ": " chapter-file))
              (throw 'CHAPTER-FILE-DELETION-FAULT (concat (orgn--ls "file-not-found") ": " chapter-file))))
          ;; Still need to delete notes file
          (if (file-exists-p chapter-notes-file)
              (progn
                (find-file chapter-notes-file)
                (orgn--delete-current-file))
            (progn
              ;; If no notes file found, don't show error; just continue.
              (throw 'CHAPTER-FILE-DELETION-FAULT (concat (orgn--ls "file-not-found") ": " chapter-notes-file)))))))))

(defun orgn--delete-object-files-for (object-name hash-table-func &optional story-folder)
  "Delete object files associated with OBJECT-NAME.
HASH-TABLE-FUNC should be the name of the function to get this object's
headlines as a hash table linked to the files.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (catch 'OBJECT-FILE-DELETION-FAULT
    (let* ((file-objects (funcall (intern hash-table-func) story-folder))
           (keys (hash-table-keys file-objects))
           key
           object-file)
      (while keys
        (setq key (pop keys))
        (when (string= object-name (gethash key file-objects))
          (setq object-file (concat story-folder / (orgn--ls "notes-folder") / key))
          (if (file-exists-p object-file)
              (progn
                (find-file object-file)
                (orgn--delete-current-file))
            (progn
              (error (concat (orgn--ls "file-not-found") ": " object-file))
              (throw 'OBJECT-FILE-DELETION-FAULT (concat (orgn--ls "file-not-found") ": " object-file)))))))))

(defun orgn--delete-character-files-for (character-name &optional story-folder)
  "Delete character files associated with CHARACTER-NAME.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--delete-object-files-for character-name "org-novelist--character-hash-table" story-folder))

(defun orgn--delete-prop-files-for (prop-name &optional story-folder)
  "Delete prop files associated with PROP-NAME.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--delete-object-files-for prop-name "org-novelist--prop-hash-table" story-folder))

(defun orgn--delete-place-files-for (place-name &optional story-folder)
  "Delete place files associated with PLACE-NAME.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--delete-object-files-for place-name "org-novelist--place-hash-table" story-folder))

(defun orgn--rename-object-in-index (chosen-object new-object-name index-file-name objects-folder object-file-prefix &optional story-folder)
  "Rename CHOSEN-OBJECT to NEW-OBJECT-NAME in INDEX-FILE-NAME, and update link.
OBJECT-FILE-PREFIX must be supplied to identify files correctly.
OBJECTS-FOLDER must be supplied to identify files correctly.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (catch 'OBJECT-RENAME-IN-INDEX-FAULT
    (let* ((indices-folder (orgn--ls "indices-folder"))
           (index-file (concat story-folder / indices-folder / (orgn--ls index-file-name orgn--file-ending)))
           index-file-contents
           beg)
      (if (file-exists-p index-file)
          (if (file-readable-p index-file)
              (progn
                (find-file index-file)
                (save-buffer)
                (goto-char (point-min))
                (insert "\n")
                (goto-char (point-min))
                (org-novelist-mode)
                (orgn--fold-show-all)  ; Belts and braces
                (while (not (org-next-visible-heading 1))
                  (when (string= chosen-object (orgn--heading-last-link-headline-text))
                    (setq beg (point))
                    (goto-char (point-min))
                    (orgn--delete-line)
                    (goto-char beg)
                    (setq index-file-contents (orgn--replace-string-in-string
                                               (format "\[\[file:..%s%s\]\[%s\]\]"
                                                       (orgn--ls / objects-folder /)
                                                       (concat (orgn--ls object-file-prefix) (orgn--system-safe-name chosen-object) orgn--file-ending)
                                                       chosen-object)
                                               (format "\[\[file:..%s%s\]\[%s\]\]"
                                                       (orgn--ls / objects-folder /)
                                                       (concat (orgn--ls object-file-prefix) (orgn--system-safe-name new-object-name) orgn--file-ending)
                                                       new-object-name)
                                               (buffer-string)))
                    (orgn--string-to-file index-file-contents index-file)
                    (revert-buffer t t t)))
                ;; Place point at start of renamed object
                (goto-char (point-max))
                (re-search-backward
                 (regexp-quote (format "\[\[file:..%s%s\]\[%s\]\]"
                                       (orgn--ls / objects-folder /)
                                       (concat (orgn--ls object-file-prefix) (orgn--system-safe-name new-object-name) orgn--file-ending)
                                       new-object-name))
                 nil t)
                )
            (progn
              (error (concat index-file " " (orgn--ls "is-not-readable")))
              (throw 'OBJECT-RENAME-IN-INDEX-FAULT (concat index-file " " (orgn--ls "is-not-readable")))))
        (progn
          (error (concat (orgn--ls "file-not-found") ": " index-file))
          (throw 'OBJECT-RENAME-IN-INDEX-FAULT (concat (orgn--ls "file-not-found") ": " index-file))))
      (save-buffer))))

(defun orgn--rename-chapter-in-index (chosen-chapter new-chapter-name &optional story-folder)
  "Rename CHOSEN-CHAPTER to NEW-CHAPTER-NAME in the index file, and update link.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rename-object-in-index chosen-chapter new-chapter-name "chapters-file" "chapters-folder" "chapter-file-prefix" story-folder))

(defun orgn--rename-character-in-index (chosen-character new-character-name &optional story-folder)
  "Rename CHOSEN-CHARACTER to NEW-CHARACTER-NAME in index file, and update link.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rename-object-in-index chosen-character new-character-name "characters-file" "notes-folder" "character-file-prefix" story-folder))

(defun orgn--rename-prop-in-index (chosen-prop new-prop-name &optional story-folder)
  "Rename CHOSEN-PROP to NEW-PROP-NAME in index file, and update link.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rename-object-in-index chosen-prop new-prop-name "props-file" "notes-folder" "prop-file-prefix" story-folder))

(defun orgn--rename-place-in-index (chosen-place new-place-name &optional story-folder)
  "Rename CHOSEN-PLACE to NEW-PLACE-NAME in index file, and update link.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rename-object-in-index chosen-place new-place-name "places-file" "notes-folder" "place-file-prefix" story-folder))

(defun orgn--rename-chapter-files-for (chosen-chapter new-chapter-name &optional story-folder)
  "Given a CHOSEN-CHAPTER in a story, rename it to NEW-CHAPTER-NAME.
Chapter index contents, chapter file contents, and chapter file notes contents
will be updated to reflect the change.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (catch 'CHAPTER-FILE-RENAME-FAULT
    (let* ((file-chapters (orgn--chapter-hash-table story-folder))
           (keys (hash-table-keys file-chapters))
           key
           chapter-file
           chapter-notes-file
           new-chapter-file
           new-chapter-notes-file
           new-chapter-contents
           new-chapter-notes-contents)
      (while keys
        (setq key (pop keys))
        (when (string= chosen-chapter (gethash key file-chapters))
          (setq chapter-file (concat story-folder / (orgn--ls "chapters-folder") / key))
          (setq chapter-notes-file (concat story-folder / (orgn--ls "notes-folder") / (orgn--ls "chapter-file-prefix") (orgn--system-safe-name chosen-chapter) (orgn--ls "notes-suffix") orgn--file-ending))
          (setq new-chapter-file (concat story-folder / (orgn--ls "chapters-folder") / (orgn--ls "chapter-file-prefix") (orgn--system-safe-name new-chapter-name) orgn--file-ending))
          (setq new-chapter-notes-file (concat story-folder / (orgn--ls "notes-folder") / (orgn--ls "chapter-file-prefix") (orgn--system-safe-name new-chapter-name) (orgn--ls "notes-suffix") orgn--file-ending))
          (if (file-exists-p chapter-file)
              (progn
                (find-file chapter-file)
                (org-novelist-mode)
                (orgn--fold-show-all)  ; Belts and braces
                (setq new-chapter-contents (orgn--replace-string-in-string
                                            (format "\[\[file:..%s%s\]\[%s\]\]"
                                                    (orgn--ls / "notes-folder" /)
                                                    (concat (orgn--ls "chapter-file-prefix") (orgn--system-safe-name chosen-chapter) (orgn--ls "notes-suffix") orgn--file-ending)
                                                    (orgn--ls "notes"))
                                            (format "\[\[file:..%s%s\]\[%s\]\]"
                                                    (orgn--ls / "notes-folder" /)
                                                    (concat (orgn--ls "chapter-file-prefix") (orgn--system-safe-name (orgn--sanitize-string new-chapter-name)) (orgn--ls "notes-suffix") orgn--file-ending)
                                                    (orgn--ls "notes"))
                                            (buffer-string)))
                (orgn--string-to-file new-chapter-contents chapter-file)
                (revert-buffer t t t)
                (orgn--set-file-property-value "TITLE" new-chapter-name chapter-file)
                (orgn--rename-current-file new-chapter-file))
            (progn
              (error (concat (orgn--ls "file-not-found") ": " chapter-file))
              (throw 'CHAPTER-FILE-RENAME-FAULT (concat (orgn--ls "file-not-found") ": " chapter-file))))
          ;; Still need to delete notes file
          (if (file-exists-p chapter-notes-file)
              (progn
                (find-file chapter-notes-file)
                (org-novelist-mode)
                (orgn--fold-show-all)  ; Belts and braces
                (setq new-chapter-notes-contents (orgn--replace-string-in-string
                                                  (format "\[\[file:..%s%s\]\[%s\]\]"
                                                          (orgn--ls / "chapters-folder" /)
                                                          key
                                                          chosen-chapter)
                                                  (format "\[\[file:..%s%s\]\[%s\]\]"
                                                          (orgn--ls / "chapters-folder" /)
                                                          (concat (orgn--ls "chapter-file-prefix") (orgn--system-safe-name (orgn--sanitize-string new-chapter-name)) orgn--file-ending)
                                                          new-chapter-name)
                                                  (buffer-string)))
                (orgn--string-to-file new-chapter-notes-contents chapter-notes-file)
                (revert-buffer t t t)
                (orgn--set-file-property-value "TITLE" (concat (orgn--ls "notes-for") " " new-chapter-name) chapter-notes-file)
                (orgn--rename-current-file new-chapter-notes-file))
            (progn
              ;; If no notes file found, don't show error; just continue.
              (throw 'CHAPTER-FILE-RENAME-FAULT (concat (orgn--ls "file-not-found") ": " chapter-notes-file))))
          (find-file new-chapter-file))))))

(defun orgn--rename-object-files-for (chosen-object new-object-name hash-table-func object-file-prefix &optional story-folder)
  "Given a CHOSEN-OBJECT in a story, rename it to NEW-OBJECT-NAME.
Object file contents will be updated to reflect the change.
HASH-TABLE-FUNC should be the name of the function to get this object's
headlines as a hash table linked to the files.
OBJECT-FILE-PREFIX must be supplied to identify files correctly.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (setq new-object-name (orgn--sanitize-string new-object-name))
  (catch 'OBJECT-FILE-RENAME-FAULT
    (let* ((file-objects (funcall (intern hash-table-func) story-folder))
           (keys (hash-table-keys file-objects))
           key
           object-file
           new-object-file
           (appearances-subtree "")
           (old-object-name "")
           (old-aliases "")
           aliases-list
           alias-curr
           (new-aliases-str ""))
      (while keys
        (setq key (pop keys))
        (when (string= chosen-object (gethash key file-objects))
          (setq object-file (concat story-folder / (orgn--ls "notes-folder") / key))
          (setq new-object-file (concat story-folder / (orgn--ls "notes-folder") / (orgn--ls object-file-prefix) (orgn--system-safe-name new-object-name) orgn--file-ending))
          (if (file-exists-p object-file)
              (progn
                (setq appearances-subtree (orgn--get-file-subtree object-file (orgn--ls "appearances-in-chapters-header")))
                (orgn--delete-org-subtrees-from-file (orgn--ls "appearances-in-chapters-header") object-file)
                (find-file object-file)
                (org-novelist-mode)
                (orgn--fold-show-all)  ; Belts and braces
                ;; Get current title and add to aliases.
                (setq old-object-name (orgn--get-file-property-value object-file "TITLE"))
                (setq old-aliases (orgn--get-file-property-value object-file "ALIASES"))
                ;; Prevent repeating names in aliases.
                (when old-aliases
                  (setq aliases-list (delete new-object-name (delq nil (delete-dups (sort (cons old-object-name (split-string old-aliases (orgn--ls "aliases-separators") t " ")) 'string<))))))
                (unless (equal aliases-list '(nil))
                  (while aliases-list
                    (setq alias-curr (pop aliases-list))
                    (setq new-aliases-str (concat new-aliases-str ", " alias-curr)))
                  (setq new-aliases-str (substring new-aliases-str 2)))
                ;; Regenerate file contents and rename it.
                (erase-buffer)
                (insert (orgn--replace-string-in-string chosen-object new-object-name (org-file-contents object-file)))
                (goto-char (point-max))
                (insert appearances-subtree)
                (orgn--set-file-property-value "TITLE" new-object-name object-file)
                (orgn--set-file-property-value "ALIASES" new-aliases-str object-file)
                (orgn--save-current-file)
                (orgn--rename-current-file new-object-file))
            (progn
              (error (concat (orgn--ls "file-not-found") ": " object-file))
              (throw 'OBJECT-FILE-RENAME-FAULT (concat (orgn--ls "file-not-found") ": " object-file))))
          (find-file new-object-file))))))

(defun orgn--rename-character-files-for (chosen-character new-character-name &optional story-folder)
  "Given a CHOSEN-CHARACTER in a story, rename it to NEW-CHARACTER-NAME.
Character index contents, and character file contents will be updated to
reflect change.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rename-object-files-for chosen-character new-character-name "org-novelist--character-hash-table" "character-file-prefix" story-folder))

(defun orgn--rename-prop-files-for (chosen-prop new-prop-name &optional story-folder)
  "Given a CHOSEN-PROP in a story, rename it to NEW-PROP-NAME.
Prop index contents, and prop file contents will be updated to reflect change.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rename-object-files-for chosen-prop new-prop-name "org-novelist--prop-hash-table" "prop-file-prefix" story-folder))

(defun orgn--rename-place-files-for (chosen-place new-place-name &optional story-folder)
  "Given a CHOSEN-PLACE in a story, rename it to NEW-PLACE-NAME.
Place index contents, and place file contents will be updated to reflect change.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rename-object-files-for chosen-place new-place-name "org-novelist--place-hash-table" "place-file-prefix" story-folder))

(defun orgn--update-glossaries (&optional story-folder)
  "Update all the glossaries in the story chapter files.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (unless (file-directory-p (concat story-folder / (orgn--ls "chapters-folder")))
    (make-directory (concat story-folder / (orgn--ls "chapters-folder")) t))
  (unless (file-directory-p (concat story-folder / (orgn--ls "notes-folder")))
    (make-directory (concat story-folder / (orgn--ls "notes-folder")) t))
  (catch 'UPDATE-GLOSSARIES-FAULT
    (let* ((chapters-folder (orgn--ls "chapters-folder"))
           (file-chapters (orgn--chapter-hash-table story-folder))
           (keys (hash-table-keys file-chapters))
           key
           (glossary-str (orgn--make-glossary-string story-folder)))
      (while keys
        (setq key (pop keys))
        (if (file-exists-p (concat story-folder / chapters-folder / key))
            (if (file-readable-p (concat story-folder / chapters-folder / key))
                (progn
                  (when (get-file-buffer (concat story-folder / chapters-folder / key))
                    (switch-to-buffer (get-file-buffer (concat story-folder / chapters-folder / key)))
                    (save-buffer))
                  (orgn--delete-org-subtrees-from-file (orgn--ls "glossary-header") (concat story-folder / chapters-folder / key))
                  (with-temp-buffer
                    (insert-file-contents (concat story-folder / chapters-folder / key))
                    (goto-char (buffer-size))
                    (insert "\n" glossary-str)
                    (orgn--string-to-file (buffer-string) (concat story-folder / chapters-folder / key))
                    (org-novelist-mode)
                    (orgn--fold-show-all)  ; Belts and braces
                    (org-update-radio-target-regexp)))
              (progn
                (error (concat story-folder / chapters-folder / key " " (orgn--ls "is-not-readable")))
                (throw 'UPDATE-GLOSSARIES-FAULT (concat story-folder / chapters-folder / key " " (orgn--ls "is-not-readable")))))
          (progn
            (error (concat (orgn--ls "file-not-found") ": " story-folder / chapters-folder / key))
            (throw 'UPDATE-GLOSSARIES-FAULT (concat (orgn--ls "file-not-found") ": " story-folder / chapters-folder / key)))))
      (org-update-radio-target-regexp))))

(defun orgn--update-object-references (&optional story-folder)
  "Update all the story appearances in the object notes files.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (unless (file-directory-p (concat story-folder / (orgn--ls "chapters-folder")))
    (make-directory (concat story-folder / (orgn--ls "chapters-folder")) t))
  (unless (file-directory-p (concat story-folder / (orgn--ls "notes-folder")))
    (make-directory (concat story-folder / (orgn--ls "notes-folder")) t))
  (catch 'UPDATE-OBJECT-REFERENCES-FAULT
    (let* ((notes-folder (orgn--ls "notes-folder"))
           (file-characters (orgn--character-hash-table story-folder))
           (file-places (orgn--place-hash-table story-folder))
           (file-props (orgn--prop-hash-table story-folder))
           (keys (append (hash-table-keys file-characters)
                         (hash-table-keys file-places)
                         (hash-table-keys file-props)))
           key
           (appearances ""))
      (while keys
        (setq key (pop keys))
        (if (file-exists-p (concat story-folder / notes-folder / key))
            (if (file-readable-p (concat story-folder / notes-folder / key))
                (progn
                  (when (get-file-buffer (concat story-folder / notes-folder / key))
                    (switch-to-buffer (get-file-buffer (concat story-folder / notes-folder / key)))
                    (save-buffer))
                  (setq appearances (orgn--make-file-chapter-references-string (concat story-folder / notes-folder / key) story-folder))
                  (orgn--delete-org-subtrees-from-file (orgn--ls "appearances-in-chapters-header") (concat story-folder / notes-folder / key))
                  (with-temp-buffer
                    (insert-file-contents (concat story-folder / notes-folder / key))
                    (goto-char (point-max))
                    (insert appearances)
                    (orgn--string-to-file (buffer-string) (concat story-folder / notes-folder / key))))
              (progn
                (error (concat story-folder / notes-folder / key " " (orgn--ls "is-not-readable")))
                (throw 'UPDATE-OBJECT-REFERENCES-FAULT (concat story-folder / notes-folder / key " " (orgn--ls "is-not-readable")))))
          (progn
            (error (concat (orgn--ls "file-not-found") ": " story-folder / notes-folder / key))
            (throw 'UPDATE-OBJECT-REFERENCES-FAULT (concat (orgn--ls "file-not-found") ": " story-folder / notes-folder / key))))))))

(defun orgn--rebuild-objects-index (index-filename hash-table-func populate-template-func &optional story-folder)
  "Rebuild the story's index for the given INDEX-FILENAME.
HASH-TABLE-FUNC should be the name of the function to get this object's
headlines as a hash table linked to the files.
POPULATE-TEMPLATE-FUNC should be the name of the function to populate the base
index file.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (unless (file-directory-p (concat story-folder / (orgn--ls "indices-folder")))
    (make-directory (concat story-folder / (orgn--ls "indices-folder")) t))
  (let* ((file-objects (funcall (intern hash-table-func) story-folder))
         (keys (hash-table-keys file-objects))
         key
         (object-list-str ""))
    ;; Overwrite file to base template
    (funcall (intern populate-template-func) (orgn--story-name story-folder) story-folder)  ; Create the index file for the object type
    (setq keys (sort keys 'string<))
    (while keys
      (setq key (pop keys))
      (setq object-list-str (concat object-list-str "** TODO " (format "\[\[file:../%s/%s\]\[%s\]\]\n" (orgn--ls "notes-folder") key (gethash key file-objects)))))
    (orgn--string-to-file (concat (org-file-contents (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls index-filename) orgn--file-ending)) object-list-str)
                          (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls index-filename) orgn--file-ending))))

(defun orgn--rebuild-characters-index (&optional story-folder)
  "Rebuild the story's characters index.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rebuild-objects-index "characters-file" "org-novelist--character-hash-table" "org-novelist--populate-characters-template" story-folder))

(defun orgn--rebuild-places-index (&optional story-folder)
  "Rebuild the story's places index.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rebuild-objects-index "places-file" "org-novelist--place-hash-table" "org-novelist--populate-places-template" story-folder))

(defun orgn--rebuild-props-index (&optional story-folder)
  "Rebuild the story's props index.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (orgn--rebuild-objects-index "props-file" "org-novelist--prop-hash-table" "org-novelist--populate-props-template" story-folder))

(defun orgn--rebuild-chapters-index (&optional story-folder)
  "Rebuild the story's chapters index.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (unless (file-directory-p (concat story-folder / (orgn--ls "indices-folder")))
    (make-directory (concat story-folder / (orgn--ls "indices-folder")) t))
  (let* ((file-chapters (orgn--chapter-hash-table story-folder))
         (keys (hash-table-keys file-chapters))
         key
         (object-list-str "")
         (chapter-matter-type nil)
         (fm-object-list-str "")
         (mm-object-list-str "")
         (bm-object-list-str ""))
    ;; Overwrite file to base template
    (orgn--populate-chapters-template (orgn--story-name story-folder) story-folder)  ; Create the chapter index file for the story
    (unless chapter-matter-type
      (while (not (or (string= chapter-matter-type (orgn--ls "front-matter-heading"))
                      (string= chapter-matter-type (orgn--ls "main-matter-heading"))
                      (string= chapter-matter-type (orgn--ls "back-matter-heading"))
                      (string= chapter-matter-type (orgn--ls "file-by-file"))))
        (setq chapter-matter-type (completing-read (concat (orgn--ls "rebuild-chapter-index-location-query") " ")
                                                   (list (orgn--ls "front-matter-heading")
                                                         (orgn--ls "main-matter-heading")
                                                         (orgn--ls "back-matter-heading")
                                                         (orgn--ls "file-by-file"))))))
    (setq keys (sort keys 'string<))
    (while keys
      (setq key (pop keys))
      ;; Put all chapters into chosen matter section by adding to correct string
      (cond ((string= chapter-matter-type (orgn--ls "front-matter-heading"))
             (setq fm-object-list-str (concat fm-object-list-str "*** TODO " (format "\[\[file:../%s/%s\]\[%s\]\]\n" (orgn--ls "chapters-folder") key (gethash key file-chapters)))))
            ((string= chapter-matter-type (orgn--ls "main-matter-heading"))
             (setq mm-object-list-str (concat mm-object-list-str "*** TODO " (format "\[\[file:../%s/%s\]\[%s\]\]\n" (orgn--ls "chapters-folder") key (gethash key file-chapters)))))
            ((string= chapter-matter-type (orgn--ls "back-matter-heading"))
             (setq bm-object-list-str (concat bm-object-list-str "*** TODO " (format "\[\[file:../%s/%s\]\[%s\]\]\n" (orgn--ls "chapters-folder") key (gethash key file-chapters)))))
            ((string= (orgn--ls "file-by-file") chapter-matter-type)
             (progn
               ;; Ask user for matter section for each chapter
               (while (not (or (string= chapter-matter-type (orgn--ls "front-matter-heading"))
                               (string= chapter-matter-type (orgn--ls "main-matter-heading"))
                               (string= chapter-matter-type (orgn--ls "back-matter-heading"))))
                 (setq chapter-matter-type (completing-read (concat (format (orgn--ls "chapter-location-query") (gethash key file-chapters))" ")
                                                            (list (orgn--ls "front-matter-heading")
                                                                  (orgn--ls "main-matter-heading")
                                                                  (orgn--ls "back-matter-heading")))))
               ;; User gave us matter type for this chapter. Add to correct string.
               (cond ((string= chapter-matter-type (orgn--ls "front-matter-heading"))
                      (setq fm-object-list-str (concat fm-object-list-str "*** TODO " (format "\[\[file:../%s/%s\]\[%s\]\]\n" (orgn--ls "chapters-folder") key (gethash key file-chapters)))))
                     ((string= chapter-matter-type (orgn--ls "main-matter-heading"))
                      (setq mm-object-list-str (concat mm-object-list-str "*** TODO " (format "\[\[file:../%s/%s\]\[%s\]\]\n" (orgn--ls "chapters-folder") key (gethash key file-chapters)))))
                     ((string= chapter-matter-type (orgn--ls "back-matter-heading"))
                      (setq bm-object-list-str (concat bm-object-list-str "*** TODO " (format "\[\[file:../%s/%s\]\[%s\]\]\n" (orgn--ls "chapters-folder") key (gethash key file-chapters))))))
               (setq chapter-matter-type (orgn--ls "file-by-file"))))))
    ;; Add matter type heading, and join matter strings together. Check each is not empty before adding header.
    (unless (string= "" fm-object-list-str)
      (setq object-list-str (concat "** " (orgn--ls "front-matter-heading") "\n" fm-object-list-str)))
    (unless (string= "" mm-object-list-str)
      (setq object-list-str (concat "** " (orgn--ls "main-matter-heading") "\n" mm-object-list-str)))
    (unless (string= "" bm-object-list-str)
      (setq object-list-str (concat "** " (orgn--ls "back-matter-heading") "\n" bm-object-list-str)))
    (setq object-list-str (string-trim object-list-str))
    ;; Output to end of the newly created (done above) chapters file.
    (orgn--string-to-file (concat (org-file-contents (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls "chapters-file") orgn--file-ending)) object-list-str)
                          (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls "chapters-file") orgn--file-ending))
    ;; Re-order the matter sections to be in the correct order here.
    (orgn--reorder-matter-in-chapter-index story-folder)))

(defun orgn--rebuild-index (index-file &optional story-folder)
  "Attempt to rebuild the INDEX-FILE.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (unless (file-directory-p (concat story-folder / (orgn--ls "indices-folder")))
    (make-directory (concat story-folder / (orgn--ls "indices-folder")) t))
  (catch 'REBUILD-INDEX-FAULT
    (cond ((string-equal (orgn--ls "characters-file" orgn--file-ending) index-file)
           (orgn--rebuild-characters-index story-folder))
          ((string-equal (orgn--ls "places-file" orgn--file-ending) index-file)
           (orgn--rebuild-places-index story-folder))
          ((string-equal (orgn--ls "props-file" orgn--file-ending) index-file)
           (orgn--rebuild-props-index story-folder))
          ((string-equal (orgn--ls "chapters-file" orgn--file-ending) index-file)
           (orgn--rebuild-chapters-index story-folder))
          (t
           (error (concat index-file (orgn--ls "unrecognised-index")))
           (throw 'REBUILD-INDEX-FAULT (concat index-file (orgn--ls "unrecognised-index")))))))

(defun orgn--rebuild-indices (&optional story-folder)
  "Rebuild all story indices if malformed.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (unless (file-directory-p (concat story-folder / (orgn--ls "chapters-folder")))
    (make-directory (concat story-folder / (orgn--ls "chapters-folder")) t))
  (unless (file-directory-p (concat story-folder / (orgn--ls "indices-folder")))
    (make-directory (concat story-folder / (orgn--ls "indices-folder")) t))
  (unless (file-directory-p (concat story-folder / (orgn--ls "notes-folder")))
    (make-directory (concat story-folder / (orgn--ls "notes-folder")) t))
  (catch 'REBUILD-INDICES-FAULT
    (let* ((indices-folder (orgn--ls "indices-folder"))
           (indices (directory-files-recursively (concat story-folder / indices-folder) (format "^%s%s\\'" (orgn--ls "sys-safe-name") orgn--file-ending)))
           index
           file-malformed-p)
      ;; Remove chapters.org from indices, as it is a special case.
      (setq indices (delete (concat story-folder / indices-folder / (orgn--ls "chapters-file") orgn--file-ending) indices))
      (while indices
        (setq index (pop indices))
        ;; Skip file if it starts with a dot.
        (unless (string= (substring (file-name-nondirectory index) 0 1) ".")
          (setq file-malformed-p nil)
          (if (file-exists-p index)
              (if (file-readable-p index)
                  (progn
                    (orgn--save-current-file index)
                    (with-temp-buffer
                      (insert-file-contents index)
                      (goto-char (point-min))
                      (insert "\n")
                      (goto-char (point-min))
                      (org-novelist-mode)
                      (orgn--fold-show-all)  ; Belts and braces
                      (when (not (org-next-visible-heading 1))
                        (unless (and (string= (orgn--heading-last-link-headline-text) (orgn--story-name story-folder))
                                     (= 1 (org-current-level)))
                          (setq file-malformed-p t))))
                    (when file-malformed-p
                      (orgn--rebuild-index (file-name-nondirectory index) story-folder)))
                (progn
                  (error (concat index " " (orgn--ls "is-not-readable")))
                  (throw 'REBUILD-INDICES-FAULT (concat index " " (orgn--ls "is-not-readable")))))
            (progn
              (orgn--string-to-file "" index)
              (orgn--rebuild-indices story-folder)
              (throw 'REBUILD-INDICES-FAULT (concat (orgn--ls "file-not-found") ": " index))))))
      ;; Chapter index is special case.
      (setq index (orgn--ls "chapters-file" orgn--file-ending))
      (setq file-malformed-p nil)
      (if (file-exists-p (concat story-folder / indices-folder / index))
          (if (file-readable-p (concat story-folder / indices-folder / index))
              (progn
                (orgn--save-current-file (concat story-folder / indices-folder / index))
                (with-temp-buffer
                  (insert-file-contents (concat story-folder / indices-folder / index))
                  (goto-char (point-min))
                  (insert "\n")
                  (goto-char (point-min))
                  (org-novelist-mode)
                  (orgn--fold-show-all)  ; Belts and braces
                  (when (not (org-next-visible-heading 1))
                    (unless (and (string= (orgn--heading-last-link-headline-text) (orgn--story-name story-folder))
                                 (= 1 (org-current-level)))
                      (setq file-malformed-p t)))
                  (when (not (org-next-visible-heading 1))
                    (unless (and (or (string= (orgn--ls "front-matter-heading") (nth 4 (org-heading-components)))
                                     (string= (orgn--ls "main-matter-heading") (nth 4 (org-heading-components)))
                                     (string= (orgn--ls "back-matter-heading") (nth 4 (org-heading-components))))
                                 (= 2 (org-current-level)))
                      (setq file-malformed-p t))))
                (when file-malformed-p
                  (orgn--rebuild-index index story-folder)))
            (progn
              (error (concat story-folder / indices-folder / index " " (orgn--ls "is-not-readable")))
              (throw 'REBUILD-INDICES-FAULT (concat story-folder / indices-folder / index " " (orgn--ls "is-not-readable")))))
        (progn
          (orgn--string-to-file "" (concat story-folder / indices-folder / index))
          (orgn--rebuild-indices story-folder)
          (throw 'REBUILD-INDICES-FAULT (concat (orgn--ls "file-not-found") ": " story-folder / indices-folder / index)))))))

(defun orgn--update-references-after-save-hook ()
  "If automatic referencing, update references on save."
  (when (derived-mode-p 'org-novelist-mode)
    (when orgn-automatic-referencing-p
      (setq orgn-automatic-referencing-p nil)
      (orgn-update-references)
      (setq orgn-automatic-referencing-p t))))

(defun orgn--reset-automatic-referencing ()
  "Reset automatic referencing to last known value when user aborts minibuffer."
  (when (string= (format "%s" this-command) "abort-minibuffers")
    (setq orgn-automatic-referencing-p orgn--autoref-p)
    (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing)))

(defun orgn--exports-hash-table (&optional story-folder)
  "Return a hash table of export templates and output files.
This series of key/value pairs will come from the Org Novelist story's
config file.
Function will try to use story that current file is a part of, unless called
with STORY-FOLDER to override that behaviour.
The returned hash table will use template filenames as keys, and output
filenames as values."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  ;; If org-novelist--story-root-folder didn't throw any errors, we should be good to go.
  (let ((exports-hash (make-hash-table :test 'equal))
        beg
        current-template
        current-outfile)
    (when (file-exists-p (concat story-folder / orgn--config-filename))
      (when (file-readable-p (concat story-folder / orgn--config-filename))
        (with-temp-buffer
          (insert-file-contents (concat story-folder / orgn--config-filename))
          (goto-char (point-min))
          (insert "\n")
          (goto-char (point-min))
          (org-novelist-mode)
          (orgn--fold-show-all)  ; Belts and braces
          (while (not (org-next-visible-heading 1))
            (when (string= (orgn--ls "exports-header") (nth 4 (org-heading-components)))
              (when (org-goto-first-child)
                ;; Add first thing to hash
                (setq current-template (expand-file-name (car (split-string (nth 4 (org-heading-components)) "[\]\[]+" t "file\:")) story-folder))
                (forward-line)
                (beginning-of-line)
                (setq beg (point))
                (forward-line -1)
                (org-end-of-subtree t t)
                ;; Include the end of an inlinetask
                (when (and (featurep 'org-inlinetask)
                           (looking-at-p (concat (org-inlinetask-outline-regexp) "END[ \t]*$")))
                  (end-of-line))
                (forward-line -1)
                (end-of-line)
                (setq current-outfile (expand-file-name (string-trim (buffer-substring beg (point))) story-folder))
                (puthash current-template current-outfile exports-hash)
                ;; Loop through siblings and do the same
                (while (org-goto-sibling)
                  (setq current-template (expand-file-name (car (split-string (nth 4 (org-heading-components)) "[\]\[]+" t "file\:")) story-folder))
                  (forward-line)
                  (beginning-of-line)
                  (setq beg (point))
                  (forward-line -1)
                  (org-end-of-subtree t t)
                  ;; Include the end of an inlinetask
                  (when (and (featurep 'org-inlinetask)
                             (looking-at-p (concat (org-inlinetask-outline-regexp) "END[ \t]*$")))
                    (end-of-line))
                  (forward-line -1)
                  (end-of-line)
                  (setq current-outfile (expand-file-name (string-trim (buffer-substring beg (point))) story-folder))
                  (puthash current-template current-outfile exports-hash))))))))
    (eval exports-hash)))

(defun orgn--reorder-matter-in-chapter-index (&optional story-folder)
  "Reorder the matter sections in the chapters index in an Org Novelist story.
Function will try to use story that current file is a part of, unless called
with STORY-FOLDER to override that behaviour."
  (let ((fm-str "")
        (mm-str "")
        (bm-str "")
        beg)
    (if (not story-folder)
        (setq story-folder (orgn--story-root-folder))
      (setq story-folder (orgn--story-root-folder story-folder)))
    (with-temp-buffer
      (insert-file-contents (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls "chapters-file") orgn--file-ending))
      (org-novelist-mode)
      (orgn--fold-show-all)  ; Belts and braces
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-min))
      (while (not (org-next-visible-heading 1))
        (when (string= (orgn--ls "front-matter-heading") (nth 4 (org-heading-components)))
          (beginning-of-line)
          (setq beg (point))
          (org-end-of-subtree t t)
          ;; Include the end of an inlinetask
          (when (and (featurep 'org-inlinetask)
                     (looking-at-p (concat (org-inlinetask-outline-regexp) "END[ \t]*$")))
            (end-of-line))
          (setq fm-str (buffer-substring beg (point)))
          (delete-region beg (point))))
      (goto-char (point-min))
      (while (not (org-next-visible-heading 1))
        (when (string= (orgn--ls "main-matter-heading") (nth 4 (org-heading-components)))
          (beginning-of-line)
          (setq beg (point))
          (org-end-of-subtree t t)
          ;; Include the end of an inlinetask
          (when (and (featurep 'org-inlinetask)
                     (looking-at-p (concat (org-inlinetask-outline-regexp) "END[ \t]*$")))
            (end-of-line))
          (setq mm-str (buffer-substring beg (point)))
          (delete-region beg (point))))
      (goto-char (point-min))
      (while (not (org-next-visible-heading 1))
        (when (string= (orgn--ls "back-matter-heading") (nth 4 (org-heading-components)))
          (beginning-of-line)
          (setq beg (point))
          (org-end-of-subtree t t)
          ;; Include the end of an inlinetask
          (when (and (featurep 'org-inlinetask)
                     (looking-at-p (concat (org-inlinetask-outline-regexp) "END[ \t]*$")))
            (end-of-line))
          (setq bm-str (buffer-substring beg (point)))
          (delete-region beg (point))))
      (goto-char (point-max))
      (insert fm-str)
      (insert mm-str)
      (insert bm-str)
      (goto-char (point-min))
      (orgn--delete-line)
      (orgn--string-to-file (buffer-string) (concat story-folder / (orgn--ls "indices-folder") / (orgn--ls "chapters-file") orgn--file-ending)))))


;;;; File Templates

;; Although the Org Novelist system attempts to be language agnostic, the
;; files used in the system have a rigid structure and should stay the same
;; for all languages.

(defconst orgn--main-template
  (concat
   "<<mode>>\n"
   "* <<story-name>>\n"
   "<<<file-instructions>>>\n"
   "** [[file:<<notes-file>>][<<notes-name>>]]\n"
   "** [[file:<<research-file>>][<<research-name>>]]\n"
   "** [[file:<<characters-file>>][<<characters-name>>]]\n"
   "** [[file:<<places-file>>][<<places-name>>]]\n"
   "** [[file:<<props-file>>][<<props-name>>]]\n"
   "** [[file:<<chapters-file>>][<<chapters-name>>]]\n"
   "** [[file:<<config-file>>][<<config-name>>]]\n")
  "The template for the story's main entry-point file.")

(defconst orgn--notes-template
  (concat
   "<<mode>>\n"
   "\#\+TITLE\: <<title>>\n"
   "* <<notes-for>> \[\[file\:<<main-file>>\]\[<<story-name>>\]\]\n"
   "<<<file-instructions>>>\n")
  "The template for the story's general notes file.")

(defconst orgn--research-template
  (concat
   "<<mode>>\n"
   "\#\+TITLE\: <<title>>\n"
   "* <<research-for>> \[\[file\:<<main-file>>\]\[<<story-name>>\]\]\n"
   "<<<file-instructions>>>\n")
  "The template for the story's general research file.")

(defconst orgn--characters-template
  (concat
   "<<mode>>\n"
   "* <<character-index-for>> \[\[file\:<<main-file>>\]\[<<story-name>>\]\]\n"
   "<<file-instructions>>\n")
  "The template for the story's character index file.")

(defconst orgn--places-template
  (concat
   "<<mode>>\n"
   "* <<place-index-for>> \[\[file\:<<main-file>>\]\[<<story-name>>\]\]\n"
   "<<file-instructions>>\n")
  "The template for the story's location index file.")

(defconst orgn--props-template
  (concat
   "<<mode>>\n"
   "* <<prop-index-for>> \[\[file\:<<main-file>>\]\[<<story-name>>\]\]\n"
   "<<file-instructions>>\n")
  "The template for the story's prop index file.")

(defconst orgn--chapters-template
  (concat
   "<<mode>>\n"
   "* <<chapter-index-for>> \[\[file\:<<main-file>>\]\[<<story-name>>\]\]\n"
   "<<file-instructions>>\n")
  "The template for the story's chapter index file.")

(defconst orgn--chapter-template
  (concat
   "<<mode>>\n"
   "\#\+TITLE\: <<title>>\n"
   "\[\[file:<<chapter-notes-file>>\]\[<<chapter-notes>>\]\] <<are-available-for-this>> "
   "\[\[file:<<chapter-index-file>>\]\[<<chapter>>\]\] <<from>> "
   "\[\[file:<<main-file>>\]\[<<story-name>>\]\].\n"
   "* <<content-header>>\n"
   "\# <<scene-name-here>>\n")
  "The template for the story's chapter files.")

(defconst orgn--chapter-notes-template
  (concat
   "<<mode>>\n"
   "\#\+TITLE\: <<title>>\n"
   "* <<notes-for>> \[\[file:<<chapter-file>>\]\[<<chapter-name>>\]\], <<indefinite-article>> "
   "\[\[file:<<chapter-index-file>>\]\[<<chapter>>\]\] <<from>> "
   "\[\[file:<<main-file>>\]\[<<story-name>>\]\].\n"
   "<<chapter-notes-content>>")
  "The template for the story's chapter notes files.")

(defconst orgn--character-notes-template
  (concat
   "<<mode>>\n"
   "\#\+TITLE\: <<title>>\n"
   "* <<notes-for>> /<<character-name>>/, <<indefinite-article>> "
   "\[\[file:<<character-index-file>>\]\[<<character>>\]\] <<from>> "
   "\[\[file:<<main-file>>\]\[<<story-name>>\]\].\n"
   "<<character-notes-content>>")
  "The template for the story's character notes files.")

(defconst orgn--prop-notes-template
  (concat
   "<<mode>>\n"
   "\#\+TITLE\: <<title>>\n"
   "* <<notes-for>> /<<prop-name>>/, <<indefinite-article>> "
   "\[\[file:<<prop-index-file>>\]\[<<prop>>\]\] <<from>> "
   "\[\[file:<<main-file>>\]\[<<story-name>>\]\].\n"
   "<<prop-notes-content>>")
  "The template for the story's prop notes files.")

(defconst orgn--place-notes-template
  (concat
   "<<mode>>\n"
   "\#\+TITLE\: <<title>>\n"
   "* <<notes-for>> /<<place-name>>/, <<indefinite-article>> "
   "\[\[file:<<place-index-file>>\]\[<<place>>\]\] <<from>> "
   "\[\[file:<<main-file>>\]\[<<story-name>>\]\].\n"
   "<<place-notes-content>>")
  "The template for the story's place notes files.")

(defconst orgn--glossary-template
  (concat
   "* <<glossary-header>>\n"
   "** <<characters-header>>\n"
   "<<characters-content>>"
   "** <<places-header>>\n"
   "<<places-content>>"
   "** <<props-header>>\n"
   "<<props-content>>")
  "The template for the chapter glossaries while writing.")

(defconst orgn--export-org-template
  (concat
   "\#\+TITLE\: <<title>>\n"
   "\#\+AUTHOR\: <<author>>\n"
   "\#\+EMAIL\: <<email>>\n"
   "\#\+DATE\: <<date>>\n"
   "<<content>>\n")
  "The template for the exported Org file of the story.")


;;;; File Template Populators

;; Populate file templates with data, then write to file.

(defun orgn--populate-main-template (story-name story-folder)
  "Populate the main entry-point file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
Once template is populated, it will be written to file."
  (let ((main-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier main-file-substitutions)
    (puthash "<<story-name>>" story-name main-file-substitutions)
    (puthash "<<file-instructions>>" (orgn--ls "main-file-instructions") main-file-substitutions)
    (puthash "<<notes-file>>" (orgn--ls "notes-folder" / "notes-file" orgn--file-ending) main-file-substitutions)
    (puthash "<<notes-name>>" (orgn--ls "notes-title") main-file-substitutions)
    (puthash "<<research-file>>" (orgn--ls "notes-folder" / "research-file" orgn--file-ending) main-file-substitutions)
    (puthash "<<research-name>>" (orgn--ls "research-title") main-file-substitutions)
    (puthash "<<characters-file>>" (orgn--ls "indices-folder" / "characters-file" orgn--file-ending) main-file-substitutions)
    (puthash "<<characters-name>>" (orgn--ls "characters-title") main-file-substitutions)
    (puthash "<<places-file>>" (orgn--ls "indices-folder" / "places-file" orgn--file-ending) main-file-substitutions)
    (puthash "<<places-name>>" (orgn--ls "places-title") main-file-substitutions)
    (puthash "<<props-file>>" (orgn--ls "indices-folder" / "props-file" orgn--file-ending) main-file-substitutions)
    (puthash "<<props-name>>" (orgn--ls "props-title") main-file-substitutions)
    (puthash "<<chapters-file>>" (orgn--ls "indices-folder" / "chapters-file" orgn--file-ending) main-file-substitutions)
    (puthash "<<chapters-name>>" (orgn--ls "chapters-title") main-file-substitutions)
    (puthash "<<config-file>>" orgn--config-filename main-file-substitutions)
    (puthash "<<config-name>>" (orgn--ls "config-name") main-file-substitutions)
    (orgn--generate-file-from-template main-file-substitutions orgn--main-template (concat story-folder / (orgn--ls "main-file" orgn--file-ending)))))

(defun orgn--populate-notes-template (story-name story-folder)
  "Populate the general notes file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
Once template is populated, it will be written to file."
  (let ((notes-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier notes-file-substitutions)
    (puthash "<<title>>" (orgn--ls "notes-title") notes-file-substitutions)
    (puthash "<<notes-for>>" (orgn--ls "notes-for") notes-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) notes-file-substitutions)
    (puthash "<<story-name>>" story-name notes-file-substitutions)
    (puthash "<<file-instructions>>" (orgn--ls "notes-file-instructions") notes-file-substitutions)
    (orgn--generate-file-from-template notes-file-substitutions orgn--notes-template (concat story-folder / (orgn--ls "notes-folder" / "notes-file" orgn--file-ending)))))

(defun orgn--populate-research-template (story-name story-folder)
  "Populate the general research file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
Once template is populated, it will be written to file."
  (let ((research-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier research-file-substitutions)
    (puthash "<<title>>" (orgn--ls "research-title") research-file-substitutions)
    (puthash "<<research-for>>" (orgn--ls "research-for") research-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) research-file-substitutions)
    (puthash "<<story-name>>" story-name research-file-substitutions)
    (puthash "<<file-instructions>>" (orgn--ls "research-file-instructions") research-file-substitutions)
    (orgn--generate-file-from-template research-file-substitutions orgn--research-template (concat story-folder / (orgn--ls "notes-folder" / "research-file" orgn--file-ending)))))

(defun orgn--populate-characters-template (story-name story-folder)
  "Populate the character index file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
Once template is populated, it will be written to file."
  (let ((characters-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier characters-file-substitutions)
    (puthash "<<character-index-for>>" (orgn--ls "character-index-for") characters-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) characters-file-substitutions)
    (puthash "<<story-name>>" story-name characters-file-substitutions)
    (puthash "<<file-instructions>>" (orgn--ls "characters-file-instructions") characters-file-substitutions)
    (orgn--generate-file-from-template characters-file-substitutions orgn--characters-template (concat story-folder / (orgn--ls "indices-folder" / "characters-file" orgn--file-ending)))))

(defun orgn--populate-places-template (story-name story-folder)
  "Populate the location index file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
Once template is populated, it will be written to file."
  (let ((places-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier places-file-substitutions)
    (puthash "<<place-index-for>>" (orgn--ls "place-index-for") places-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) places-file-substitutions)
    (puthash "<<story-name>>" story-name places-file-substitutions)
    (puthash "<<file-instructions>>" (orgn--ls "places-file-instructions") places-file-substitutions)
    (orgn--generate-file-from-template places-file-substitutions orgn--places-template (concat story-folder / (orgn--ls "indices-folder" / "places-file" orgn--file-ending)))))

(defun orgn--populate-props-template (story-name story-folder)
  "Populate the prop index file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
Once template is populated, it will be written to file."
  (let ((props-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier props-file-substitutions)
    (puthash "<<prop-index-for>>" (orgn--ls "prop-index-for") props-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) props-file-substitutions)
    (puthash "<<story-name>>" story-name props-file-substitutions)
    (puthash "<<file-instructions>>" (orgn--ls "props-file-instructions") props-file-substitutions)
    (orgn--generate-file-from-template props-file-substitutions orgn--props-template (concat story-folder / (orgn--ls "indices-folder" / "props-file" orgn--file-ending)))))

(defun orgn--populate-chapters-template (story-name story-folder)
  "Populate the chapter index file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
Once template is populated, it will be written to file."
  (let ((chapters-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier chapters-file-substitutions)
    (puthash "<<chapter-index-for>>" (orgn--ls "chapter-index-for") chapters-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) chapters-file-substitutions)
    (puthash "<<story-name>>" story-name chapters-file-substitutions)
    (puthash "<<file-instructions>>" (orgn--ls "chapters-file-instructions") chapters-file-substitutions)
    (orgn--generate-file-from-template chapters-file-substitutions orgn--chapters-template (concat story-folder / (orgn--ls "indices-folder" / "chapters-file" orgn--file-ending)))))

(defun orgn--populate-chapter-template (story-name story-folder chapter-file chapter-title)
  "Populate a chapter file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
CHAPTER-FILE is the file name for the chapter. CHAPTER-TITLE is the name for
the chapter.
Once template is populated, it will be written to file."
  (let ((chapter-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier chapter-file-substitutions)
    (puthash "<<title>>" chapter-title chapter-file-substitutions)
    (puthash "<<chapter-notes-file>>" (concat ".." / (orgn--ls "notes-folder") / chapter-file (orgn--ls "notes-suffix") orgn--file-ending) chapter-file-substitutions)
    (puthash "<<chapter-notes>>" (orgn--ls "notes") chapter-file-substitutions)
    (puthash "<<are-available-for-this>>" (orgn--ls "are-available-for-this") chapter-file-substitutions)
    (puthash "<<chapter-index-file>>" (concat ".." / (orgn--ls "indices-folder" / "chapters-file" orgn--file-ending)) chapter-file-substitutions)
    (puthash "<<chapter>>" (orgn--ls "chapter") chapter-file-substitutions)
    (puthash "<<from>>" (orgn--ls "from") chapter-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) chapter-file-substitutions)
    (puthash "<<story-name>>" story-name chapter-file-substitutions)
    (puthash "<<content-header>>" (orgn--ls "content-header") chapter-file-substitutions)
    (puthash "<<scene-name-here>>" (orgn--ls "scene-name-here") chapter-file-substitutions)
    (orgn--generate-file-from-template chapter-file-substitutions orgn--chapter-template (concat story-folder / (orgn--ls "chapters-folder") / chapter-file orgn--file-ending))))

(defun orgn--populate-chapter-notes-template (story-name story-folder chapter-file chapter-title)
  "Populate a chapter file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
CHAPTER-FILE is the file name for the chapter. CHAPTER-TITLE is the name for
the chapter.
Once template is populated, it will be written to file."
  (let ((chapter-notes-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier chapter-notes-file-substitutions)
    (puthash "<<title>>" (concat (orgn--ls "notes-for") " " chapter-title) chapter-notes-file-substitutions)
    (puthash "<<notes-for>>" (orgn--ls "notes-for") chapter-notes-file-substitutions)
    (puthash "<<chapter-file>>" (concat ".." / (orgn--ls "chapters-folder") / chapter-file orgn--file-ending) chapter-notes-file-substitutions)
    (puthash "<<chapter-name>>" chapter-title chapter-notes-file-substitutions)
    (puthash"<<indefinite-article>>" (orgn--ls "indefinite-article-simple") chapter-notes-file-substitutions)
    (puthash "<<chapter-index-file>>" (concat ".." / (orgn--ls "indices-folder" / "chapters-file" orgn--file-ending)) chapter-notes-file-substitutions)
    (puthash "<<chapter>>" (orgn--ls "chapter") chapter-notes-file-substitutions)
    (puthash "<<from>>" (orgn--ls "from") chapter-notes-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) chapter-notes-file-substitutions)
    (puthash "<<story-name>>" story-name chapter-notes-file-substitutions)
    (puthash "<<chapter-notes-content>>" (orgn--ls "chapter-notes-content") chapter-notes-file-substitutions)
    (orgn--generate-file-from-template chapter-notes-file-substitutions orgn--chapter-notes-template (concat story-folder / (orgn--ls "notes-folder") / chapter-file (orgn--ls "notes-suffix") orgn--file-ending))))

(defun orgn--populate-character-notes-template (story-name story-folder character-file character-name)
  "Populate a character file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
CHARACTER-FILE is the file name for the character notes. CHARACTER-NAME is the
name for the character.
Once template is populated, it will be written to file."
  (let ((character-notes-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier character-notes-file-substitutions)
    (puthash "<<title>>" character-name character-notes-file-substitutions)
    (puthash "<<notes-for>>" (orgn--ls "notes-for") character-notes-file-substitutions)
    (puthash "<<character-name>>" character-name character-notes-file-substitutions)
    (puthash"<<indefinite-article>>" (orgn--ls "indefinite-article-simple") character-notes-file-substitutions)
    (puthash "<<character-index-file>>" (concat ".." / (orgn--ls "indices-folder" / "characters-file" orgn--file-ending)) character-notes-file-substitutions)
    (puthash "<<character>>" (orgn--ls "character") character-notes-file-substitutions)
    (puthash "<<from>>" (orgn--ls "from") character-notes-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) character-notes-file-substitutions)
    (puthash "<<story-name>>" story-name character-notes-file-substitutions)
    (puthash "<<character-notes-content>>" (orgn--ls "character-notes-content") character-notes-file-substitutions)
    (orgn--generate-file-from-template character-notes-file-substitutions orgn--character-notes-template (concat story-folder / (orgn--ls "notes-folder") / character-file orgn--file-ending))))

(defun orgn--populate-prop-notes-template (story-name story-folder prop-file prop-name)
  "Populate a prop file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
PROP-FILE is the file name for the prop notes. PROP-NAME is the
name for the prop.
Once template is populated, it will be written to file."
  (let ((prop-notes-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier prop-notes-file-substitutions)
    (puthash "<<title>>" prop-name prop-notes-file-substitutions)
    (puthash "<<notes-for>>" (orgn--ls "notes-for") prop-notes-file-substitutions)
    (puthash "<<prop-name>>" prop-name prop-notes-file-substitutions)
    (puthash"<<indefinite-article>>" (orgn--ls "indefinite-article-simple") prop-notes-file-substitutions)
    (puthash "<<prop-index-file>>" (concat ".." / (orgn--ls "indices-folder" / "props-file" orgn--file-ending)) prop-notes-file-substitutions)
    (puthash "<<prop>>" (orgn--ls "prop") prop-notes-file-substitutions)
    (puthash "<<from>>" (orgn--ls "from") prop-notes-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) prop-notes-file-substitutions)
    (puthash "<<story-name>>" story-name prop-notes-file-substitutions)
    (puthash "<<prop-notes-content>>" (orgn--ls "prop-notes-content") prop-notes-file-substitutions)
    (orgn--generate-file-from-template prop-notes-file-substitutions orgn--prop-notes-template (concat story-folder / (orgn--ls "notes-folder") / prop-file orgn--file-ending))))

(defun orgn--populate-place-notes-template (story-name story-folder place-file place-name)
  "Populate a place file template with data.
STORY-NAME is the name for the story, and STORY-FOLDER is its save location.
PLACE-FILE is the file name for the place notes. PLACE-NAME is the
name for the place.
Once template is populated, it will be written to file."
  (let ((place-notes-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<mode>>" orgn--mode-identifier place-notes-file-substitutions)
    (puthash "<<title>>" place-name place-notes-file-substitutions)
    (puthash "<<notes-for>>" (orgn--ls "notes-for") place-notes-file-substitutions)
    (puthash "<<place-name>>" place-name place-notes-file-substitutions)
    (puthash"<<indefinite-article>>" (orgn--ls "indefinite-article-simple") place-notes-file-substitutions)
    (puthash "<<place-index-file>>" (concat ".." / (orgn--ls "indices-folder" / "places-file" orgn--file-ending)) place-notes-file-substitutions)
    (puthash "<<place>>" (orgn--ls "place") place-notes-file-substitutions)
    (puthash "<<from>>" (orgn--ls "from") place-notes-file-substitutions)
    (puthash "<<main-file>>" (concat ".." / (orgn--ls "main-file" orgn--file-ending)) place-notes-file-substitutions)
    (puthash "<<story-name>>" story-name place-notes-file-substitutions)
    (puthash "<<place-notes-content>>" (orgn--ls "place-notes-content") place-notes-file-substitutions)
    (orgn--generate-file-from-template place-notes-file-substitutions orgn--place-notes-template (concat story-folder / (orgn--ls "notes-folder") / place-file orgn--file-ending))))

(defun orgn--populate-glossary-string (&optional story-folder)
  "Populate a glossary string with data.
If STORY-FOLDER is not provided, try to determine story files based on currently
open buffer."
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (let* ((notes-folder (orgn--ls "notes-folder"))
         (glossary-string-substitutions (make-hash-table :test 'equal))
         (existing-characters (orgn--character-hash-table story-folder))
         (existing-props (orgn--prop-hash-table story-folder))
         (existing-places (orgn--place-hash-table story-folder))
         (character-keys (hash-table-keys existing-characters))
         character-key
         characters-content
         character-aliases-str
         character-aliases
         character-alias
         (prop-keys (hash-table-keys existing-props))
         prop-key
         props-content
         prop-aliases-str
         prop-aliases
         prop-alias
         (place-keys (hash-table-keys existing-places))
         place-key
         places-content
         place-aliases-str
         place-aliases
         place-alias)
    (puthash "<<glossary-header>>" (orgn--ls "glossary-header") glossary-string-substitutions)
    (puthash "<<characters-header>>" (orgn--ls "characters-title") glossary-string-substitutions)
    (setq character-keys (sort character-keys 'string<))
    (while character-keys
      (setq character-key (pop character-keys))
      (unless (string= character-key (gethash character-key existing-characters))
        (when characters-content
          (setq characters-content (concat characters-content "\n")))
        (setq characters-content (concat characters-content "*** <<<" (gethash character-key existing-characters) ">>> "
                                         "\[\[file:.." / notes-folder / character-key "\]\[" (orgn--ls "view-notes") "\]\]"))
        (setq character-aliases-str (orgn--get-file-property-value (concat story-folder / notes-folder / character-key) "aliases"))
        (when character-aliases-str
          (setq character-aliases (sort (split-string character-aliases-str (orgn--ls "aliases-separators") t " ") 'string<)))
        (while character-aliases
          (setq character-alias (string-trim (pop character-aliases)))
          (setq characters-content (concat characters-content "\n- <<<" character-alias ">>> " (orgn--ls "alias-for") " " (gethash character-key existing-characters))))))
    (when characters-content
      (setq characters-content (concat characters-content "\n")))
    (puthash "<<characters-content>>" characters-content glossary-string-substitutions)
    (puthash "<<places-header>>" (orgn--ls "places-title") glossary-string-substitutions)
    (setq place-keys (sort place-keys 'string<))
    (while place-keys
      (setq place-key (pop place-keys))
      (unless (string= place-key (gethash place-key existing-places))
        (when places-content
          (setq places-content (concat places-content "\n")))
        (setq places-content (concat places-content "*** <<<" (gethash place-key existing-places) ">>> "
                                     "\[\[file:.." / notes-folder / place-key "\]\[" (orgn--ls "view-notes") "\]\]"))
        (setq place-aliases-str (orgn--get-file-property-value (concat story-folder / notes-folder / place-key) "aliases"))
        (when place-aliases-str
          (setq place-aliases (sort (split-string place-aliases-str (orgn--ls "aliases-separators") t " ") 'string<)))
        (while place-aliases
          (setq place-alias (string-trim (pop place-aliases)))
          (setq places-content (concat places-content "\n- <<<" place-alias ">>> " (orgn--ls "alias-for") " " (gethash place-key existing-places))))))
    (when places-content
      (setq places-content (concat places-content "\n")))
    (puthash "<<places-content>>" places-content glossary-string-substitutions)
    (puthash "<<props-header>>" (orgn--ls "props-title") glossary-string-substitutions)
    (setq prop-keys (sort prop-keys 'string<))
    (while prop-keys
      (setq prop-key (pop prop-keys))
      (unless (string= prop-key (gethash prop-key existing-props))
        (when props-content
          (setq props-content (concat props-content "\n")))
        (setq props-content (concat props-content "*** <<<" (gethash prop-key existing-props) ">>> "
                                    "\[\[file:.." / notes-folder / prop-key "\]\[" (orgn--ls "view-notes") "\]\]"))
        (setq prop-aliases-str (orgn--get-file-property-value (concat story-folder / notes-folder / prop-key) "aliases"))
        (when prop-aliases-str
          (setq prop-aliases (sort (split-string prop-aliases-str (orgn--ls "aliases-separators") t " ") 'string<)))
        (while prop-aliases
          (setq prop-alias (string-trim (pop prop-aliases)))
          (setq props-content (concat props-content "\n- <<<" prop-alias ">>> " (orgn--ls "alias-for") " " (gethash prop-key existing-props))))))
    (puthash "<<props-content>>" props-content glossary-string-substitutions)
    (orgn--generate-string-from-template glossary-string-substitutions orgn--glossary-template)))

(defun orgn--populate-export-org-template (story-name author email date content output-file)
  "Populate the export Org file template with data.
STORY-NAME is the name for the story.
AUTHOR and EMAIL are for the story's writer. DATE is when the export was made.
CONTENT is the story content as a string.
OUTPUT-FILE is where the exported Org file will be saved.
Once template is populated, it will be written to file."
  (let ((export-org-file-substitutions (make-hash-table :test 'equal)))
    (puthash "<<title>>" story-name export-org-file-substitutions)
    (puthash "<<author>>" author export-org-file-substitutions)
    (puthash "<<email>>" email export-org-file-substitutions)
    (puthash "<<date>>" date export-org-file-substitutions)
    (puthash "<<content>>" content export-org-file-substitutions)
    (orgn--generate-file-from-template export-org-file-substitutions orgn--export-org-template output-file)))




;;;; User Acessible Functions

(defun orgn-new-story (story-name story-folder)
  "Create the minimum number of linked files and folders for a new story.
STORY-NAME is the name of the story, and STORY-FOLDER is its save location."
  (interactive (list (read-string (concat (orgn--ls "story-name-query") " "))
                     (read-directory-name (concat (orgn--ls "story-save-location-query") " "))))
  (setq story-name (orgn--sanitize-string story-name))
  (setq story-folder (concat story-folder / (orgn--system-safe-name story-name)))
  (catch 'NON-UNIQUE-STORY
    (setq orgn--autoref-p orgn-automatic-referencing-p)
    (setq orgn-automatic-referencing-p nil)
    ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
    (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
    (if (file-directory-p story-folder)
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (user-error (concat (orgn--ls "story-folder-already-in-use") ": " story-folder))
          (throw 'NON-UNIQUE-STORY (concat (orgn--ls "story-folder-already-in-use") ": " story-folder)))
      (progn
        (unless (file-exists-p (file-name-directory (concat story-folder /)))
          (make-directory (file-name-directory (concat story-folder /)) t))  ; The 't' tells emacs to create any non-existent parents directories that are needed as well
        (orgn--string-to-file "" (concat story-folder / orgn--config-filename))  ; Create an empty configuration file for the story
        (orgn--populate-main-template story-name story-folder)  ; Create the main entry-point file for the story
        (orgn--populate-notes-template story-name story-folder)  ; Create the general notes file for the story
        (orgn--populate-research-template story-name story-folder)  ; Create the general research file for the story
        (orgn--populate-characters-template story-name story-folder)  ; Create the character index file for the story
        (orgn--populate-places-template story-name story-folder)  ; Create the location index file for the story
        (orgn--populate-props-template story-name story-folder)  ; Create the prop index file for the story
        (orgn--populate-chapters-template story-name story-folder)  ; Create the chapter index file for the story
        ;; At this point, all the minimalist files needed for a new story should have been created. All that remains for us to do is to open the story's main.org file in the current buffer.
        (find-file (concat story-folder / (orgn--ls "main-file" orgn--file-ending)))))
    (setq orgn-automatic-referencing-p orgn--autoref-p)
    (when orgn-automatic-referencing-p
      (orgn-update-references story-folder))
    ;; Remove hook to reset automatic referencing since we made it to the end of the function.
    (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing)))

(defun orgn-new-chapter (chapter-name)
  "Create and modify the minimum number of linked files for a new chapter.
CHAPTER-NAME will be the name given to the chapter. Org Novelist requires all
chapters to have a name, even if this will not be used on export."
  (interactive (list (read-string (concat (orgn--ls "chapter-name-query") " "))))
  (setq chapter-name (orgn--sanitize-string chapter-name))
  (catch 'CHAPTER-CREATION-FAULT
    (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
           (chapters-file (orgn--ls "chapters-file" orgn--file-ending))
           (indices-folder (orgn--ls "indices-folder"))
           (story-name (orgn--story-name story-folder))
           (existing-chapters (orgn--chapter-hash-table story-folder))
           (chapter-file (concat (orgn--ls "chapter-file-prefix") (orgn--system-safe-name chapter-name) orgn--file-ending))
           (chapters-folder (orgn--ls "chapters-folder"))
           (keys (hash-table-keys existing-chapters))
           key
           chapter-matter-type
           (file-malformed nil))
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      ;; At this point, we can be fairly certain the function was called while a file from an Org Novelist story was open in the current buffer.
      ;; However, we should still make sure the chapters.org file exists before we start trying to manipulate it. If it doesn't exist, just create one.
      ;; Either way, open the chapter index file when done.
      (unless (file-exists-p (concat story-folder / indices-folder / chapters-file))
        (orgn--populate-chapters-template story-name story-folder))  ; Create the chapter index file for the story
      ;; Get a list of all current chapters in story. Base this on filenames, not the index. Once we have this list, make sure that chosen chapter name is not on it.
      ;; If it is, throw an error to the user. We should generalise a function for getting all chapter names, because we will be using this in the `destroy chapter' function as well.
      (while keys
        (setq key (pop keys))
        (when (string= chapter-file key)
          (progn
            (completing-read (concat (orgn--ls "name-already-in-use") " ") (list (orgn--ls "okay")))
            (setq chapter-name (read-string (concat (orgn--ls "chapter-name-query") " ")))
            (orgn-new-chapter chapter-name)  ; Call this function again
            (setq orgn-automatic-referencing-p orgn--autoref-p)
            (throw 'CHAPTER-CREATION-FAULT (concat (orgn--ls "name-already-in-use") ": " story-folder / chapters-folder / chapter-file)))))
      (find-file (concat story-folder / indices-folder / chapters-file))  ; Open the chapter index file (it will stay open after this, so no need to mess with temp buffers)
      ;; Check if index is malformed.
      (if (file-exists-p (concat story-folder / indices-folder / chapters-file))
          (if (file-readable-p (concat story-folder / indices-folder / chapters-file))
              (progn
                (with-temp-buffer
                  (insert-file-contents (concat story-folder / indices-folder / chapters-file))
                  (goto-char (point-min))
                  (insert "\n")
                  (goto-char (point-min))
                  (org-novelist-mode)
                  (orgn--fold-show-all)  ; Belts and braces
                  (when (not (org-next-visible-heading 1))
                    (unless (and (string= (orgn--heading-last-link-headline-text) (orgn--story-name story-folder))
                                 (= 1 (org-current-level)))
                      (setq file-malformed t)))
                  (when (not (org-next-visible-heading 1))
                    (unless (and (or (string= (orgn--ls "front-matter-heading") (nth 4 (org-heading-components)))
                                     (string= (orgn--ls "main-matter-heading") (nth 4 (org-heading-components)))
                                     (string= (orgn--ls "back-matter-heading") (nth 4 (org-heading-components))))
                                 (= 2 (org-current-level)))
                      (setq file-malformed t))))
                (when file-malformed
                  (orgn--rebuild-index chapters-file story-folder)))
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat story-folder / indices-folder / chapters-file " " (orgn--ls "is-not-readable")))
              (throw 'CHAPTER-CREATION-FAULT (concat story-folder / indices-folder / chapters-file " " (orgn--ls "is-not-readable")))))
        (progn
          (orgn--string-to-file "" (concat story-folder / indices-folder / chapters-file))
          (orgn-new-chapter chapter-name)  ; Call this function again
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (throw 'CHAPTER-CREATION-FAULT (concat (orgn--ls "file-not-found") ": " story-folder / indices-folder / chapters-file))))
      ;; Chapter Index file should be formatted well enough to add new chapter. Find position for addition.
      (org-novelist-mode)
      ;; Check to see if the chosen section already exists.
      (orgn--fold-show-all)  ; Belts and braces
      (goto-char (point-min))  ; Move to start of buffer
      (catch 'MATTER-LOOP-EXIT
        (if (org-goto-first-child)  ; Goto the first heading in buffer if one exists
            (progn  ; Basic malformed check passed
              ;; Ask user what section they want the chapter to be in.
              (while (not (or (string= chapter-matter-type (orgn--ls "front-matter-heading"))
                              (string= chapter-matter-type (orgn--ls "main-matter-heading"))
                              (string= chapter-matter-type (orgn--ls "back-matter-heading"))))
                (setq chapter-matter-type (completing-read (concat
                                                            (format (orgn--ls "chapter-location-query")
                                                                    chapter-name
                                                                    (orgn--ls "front-matter-heading")
                                                                    (orgn--ls "main-matter-heading")
                                                                    (orgn--ls "back-matter-heading"))
                                                            " ")
                                                           (list (orgn--ls "front-matter-heading")
                                                                 (orgn--ls "main-matter-heading")
                                                                 (orgn--ls "back-matter-heading")))))
              (if (org-goto-first-child)
                  ;; Potential Matter Found, is it the right one?
                  (let (found-matter-type)
                    (while t
                      (setq found-matter-type (nth 4 (org-heading-components)))  ; Extract just the heading text without other Org stuff
                      (if (string= found-matter-type chapter-matter-type)
                          (progn
                            ;; Matter Type Found, go to end of section and setup for new chapter, exit loop.
                            (if (org-goto-first-child)
                                ;; Matter type found, there are existing chapters in it.
                                (progn
                                  (re-search-backward org-outline-regexp-bol nil t)  ; Go to parent heading
                                  (org-end-of-subtree)  ; Go to end of last subheading in child tree
                                  (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                                  (org-todo))  ; Turn heading into a TODO item
                              ;; Matter type found, but it is empty.
                              (progn
                                (org-end-of-subtree)  ; Go to end of last subheading
                                (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                                (org-demote)  ; Turn heading into a subheading
                                (org-todo)))  ; Turn heading into a TODO item
                            (throw 'MATTER-LOOP-EXIT (orgn--ls "new-chapter-created")))
                        ;; Matter Type Not Found, advance to next sibling and restart loop, or create new matter type when no siblings left.
                        (unless (org-goto-sibling)
                          ;; Matter type not found and no more siblings; create matter type, setup for new chapter, and exit loop.
                          (goto-char (point-min))
                          (if (org-goto-first-child)
                              ;; Subheadings found
                              (progn
                                (re-search-backward org-outline-regexp-bol nil t)  ; Go to parent heading
                                (org-end-of-subtree)  ; Go to end of last subheading
                                (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                                (insert chapter-matter-type)  ; Create section heading
                                (org-promote)  ; Turn subheading into a heading
                                (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                                (org-demote)  ; Turn heading into a subheading
                                (org-todo))  ; Turn heading into a TODO item
                            ;; Subheadings not found
                            (progn
                              (org-end-of-subtree)  ; Go to end of last subheading
                              (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                              (insert chapter-matter-type)  ; Create section heading
                              (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                              (org-demote)  ; Turn heading into a subheading
                              (org-todo)))  ; Turn heading into a TODO item
                          (throw 'MATTER-LOOP-EXIT (orgn--ls "no-more-headings"))))))
                (progn ;; No matter found, create needed one and add chapter
                  ;; Create the required matter section and setup for new chapter within it.
                  (org-end-of-subtree)  ; Go to end of last subheading
                  (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                  (org-demote)  ; Turn heading into a subheading
                  (insert chapter-matter-type)  ; Create section heading
                  (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                  (org-demote)  ; Turn heading into a subheading
                  (org-todo))))  ; Turn heading into a TODO item
          (progn  ; File malformed
            (setq orgn-automatic-referencing-p orgn--autoref-p)
            (error (concat (orgn--ls "file-malformed") ": " story-folder / indices-folder / chapters-file))
            (throw 'CHAPTER-CREATION-FAULT (concat (orgn--ls "file-malformed") ": " story-folder / indices-folder / chapters-file)))))
      ;; By here, point should be at the correct location to create the new chapter.
      (orgn--make-chapter-at-index-point chapter-name)  ; Create chapter at point
      (save-buffer)
      ;; Re-order the matter sections to be in the correct order here.
      (orgn--reorder-matter-in-chapter-index story-folder)
      ;; Place cursor at the start of the new chapter name.
      (goto-char (point-max))
      (re-search-backward
       (regexp-quote chapter-name)
       nil t)
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-destroy-chapter ()
  "Remove a chapter from the chapter index, and delete associated files."
  (interactive)
  ;; We need to figure out what chapters are available before interacting with the user.
  (catch 'CHAPTER-DELETION-FAULT
    (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
           (indices-folder (orgn--ls "indices-folder"))
           (chapters-file (concat story-folder / indices-folder / (orgn--ls "chapters-file" orgn--file-ending)))
           (chapters-headlines (orgn--get-all-story-chapters-headlines story-folder))
           chosen-chapter)
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      ;; chapters-headlines now contains a list of all chapters to present to user for deletion.
      ;; This includes files without an entry in the index, and entries in the index without a file.
      (if chapters-headlines
          (progn
            (while (not chosen-chapter)
              (setq chosen-chapter (completing-read (concat (orgn--ls "chapter-name-query") " ")
                                                    chapters-headlines)))
            ;; Chapter has been chosen by user for deletion.
            ;; Delete chapter's files and remove it from the index.
            (orgn--delete-chapter-files-for chosen-chapter story-folder)
            (orgn--delete-chapter-from-index chosen-chapter story-folder))
        (progn
          (message (concat chapters-file ": " (orgn--ls "no-chapters-found")))
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (throw 'CHAPTER-DELETION-FAULT (concat chapters-file ": " (orgn--ls "no-chapters-found")))))
      (if (file-exists-p chapters-file)
          (if (file-readable-p chapters-file)
              (find-file chapters-file)
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat chapters-file " " (orgn--ls "is-not-readable")))
              (throw 'CHAPTER-DELETION-FAULT (concat chapters-file " " (orgn--ls "is-not-readable")))))
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (error (concat (orgn--ls "file-not-found") ": " chapters-file))
          (throw 'CHAPTER-DELETION-FAULT (concat (orgn--ls "file-not-found") ": " chapters-file))))
      ;; Re-order the matter sections to be in the correct order here.
      (orgn--reorder-matter-in-chapter-index story-folder)
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-rename-chapter ()
  "Rename a chapter from the chapter index and update associated files."
  (interactive)
  ;; We need to figure out what chapters are available before interacting with the user.
  (catch 'RENAME-CHAPTER-FAULT-AT-INDEX
    (let* ((story-folder (orgn--story-root-folder))
           (indices-folder (orgn--ls "indices-folder"))
           (chapters-headlines (orgn--get-all-story-chapters-headlines story-folder))
           chosen-chapter
           new-chapter-name
           (chapters-file (concat story-folder / indices-folder / (orgn--ls "chapters-file" orgn--file-ending))))
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      ;; chapters-headlines now contains a list of all chapters to present to user for renaming.
      ;; This includes files without an entry in the index, and entries in the index without a file.
      (if chapters-headlines
          (progn
            (while (not chosen-chapter)
              (setq chosen-chapter (completing-read (concat (orgn--ls "chapter-name-query") " ")
                                                    chapters-headlines)))
            ;; Chapter has been chosen for renaming by user. What is the new name?
            (setq new-chapter-name (orgn--sanitize-string (read-string (concat (orgn--ls "new-chapter-name-query") " "))))
            ;; Rename chapter files and internal headings, and rename chapter in the index.
            (orgn--rename-chapter-files-for chosen-chapter new-chapter-name story-folder)
            (orgn--rename-chapter-in-index chosen-chapter new-chapter-name story-folder))
        (progn
          (message (concat chapters-file ": " (orgn--ls "no-chapters-found")))
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (throw 'RENAME-CHAPTER-FAULT-AT-INDEX (concat chapters-file ": " (orgn--ls "no-chapters-found")))))
      (if (file-exists-p chapters-file)
          (if (file-readable-p chapters-file)
              (find-file chapters-file)
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat chapters-file " " (orgn--ls "is-not-readable")))
              (throw 'RENAME-CHAPTER-FAULT-AT-INDEX (concat chapters-file " " (orgn--ls "is-not-readable")))))
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (error (concat (orgn--ls "file-not-found") ": " chapters-file))
          (throw 'RENAME-CHAPTER-FAULT-AT-INDEX (concat (orgn--ls "file-not-found") ": " chapters-file))))
      ;; Re-order the matter sections to be in the correct order here.
      (orgn--reorder-matter-in-chapter-index story-folder)
      ;; Place cursor at the start of the new chapter name.
      (goto-char (point-max))
      (re-search-backward
       (regexp-quote new-chapter-name)
       nil t)
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-new-character (character-name)
  "Create and modify the minimum number of linked files for a new character.
CHARACTER-NAME will be the name given to the character."
  (interactive (list (read-string (concat (orgn--ls "character-name-query") " "))))
  (setq character-name (orgn--sanitize-string character-name))
  (catch 'CHARACTER-CREATION-FAULT
    (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
           (characters-file (orgn--ls "characters-file" orgn--file-ending))
           (indices-folder (orgn--ls "indices-folder"))
           (notes-folder (orgn--ls "notes-folder"))
           (story-name (orgn--story-name story-folder))
           (existing-characters (orgn--character-hash-table story-folder))
           (character-file (concat (orgn--ls "character-file-prefix") (orgn--system-safe-name character-name) orgn--file-ending))
           (keys (hash-table-keys existing-characters))
           key
           insert-point)
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      (unless (file-exists-p (concat story-folder / indices-folder / characters-file))
        (orgn--populate-characters-template story-name story-folder))
      (while keys
        (setq key (pop keys))
        (when (string= character-file key)
          (progn
            (completing-read (concat (orgn--ls "name-already-in-use") " ") (list (orgn--ls "okay")))
            (setq character-name (read-string (concat (orgn--ls "character-name-query") " ")))
            (orgn-new-character character-name)  ; Call this function again
            (setq orgn-automatic-referencing-p orgn--autoref-p)
            (throw 'CHARACTER-CREATION-FAULT (concat (orgn--ls "name-already-in-use") ": " story-folder / notes-folder / character-file)))))
      (find-file (concat story-folder / indices-folder / characters-file))
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-min))
      (org-novelist-mode)
      (orgn--fold-show-all)  ; Belts and braces
      (when (not (org-next-visible-heading 1))
        (if (and (string= (orgn--heading-last-link-headline-text) (orgn--story-name story-folder))
                 (= 1 (org-current-level)))
            (if (org-goto-first-child)
                ;; Existing entries found.
                (progn
                  (re-search-backward org-outline-regexp-bol nil t)  ; Go to parent heading
                  (org-end-of-subtree)  ; Go to end of last subheading in child tree
                  (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                  (org-todo)  ; Turn heading into a TODO item
                  (setq insert-point (point))
                  (goto-char (point-min))
                  (orgn--delete-line)
                  (goto-char (- insert-point 1)))
              ;; First entry.
              (progn
                (org-end-of-subtree)  ; Go to end of last subheading
                (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                (org-demote)  ; Turn heading into a subheading
                (org-todo)  ; Turn heading into a TODO item
                (setq insert-point (point))
                (goto-char (point-min))
                (orgn--delete-line)
                (goto-char (- insert-point 1))))
          ;; File Malformed
          (progn
            (goto-char (point-min))
            (orgn--delete-line)
            (orgn--rebuild-characters-index story-folder)
            (orgn-new-character character-name)  ; Call this function again
            (setq orgn-automatic-referencing-p orgn--autoref-p)
            (throw 'CHARACTER-CREATION-FAULT (concat (orgn--ls "file-malformed") ": " story-folder / indices-folder / characters-file)))))
      ;; By here, point should be at the correct location to create the new character.
      (save-excursion  ; Allow cursor to be placed at the start of the new character name
        (orgn--make-character-at-index-point character-name))  ; Create character at point
      (save-buffer)
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-destroy-character ()
  "Remove a character from the character index, and delete associated files."
  (interactive)
  (catch 'CHARACTER-DELETION-FAULT
    (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
           (indices-folder (orgn--ls "indices-folder"))
           (characters-file (concat story-folder / indices-folder / (orgn--ls "characters-file" orgn--file-ending)))
           (characters-headlines (orgn--get-all-story-characters-headlines story-folder))
           chosen-character)
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      ;; characters-headlines now contains a list of all characters to present to user for deletion.
      ;; This includes files without an entry in the index, and entries in the index without a file.
      (if characters-headlines
          (progn
            (while (not chosen-character)
              (setq chosen-character (completing-read (concat (orgn--ls "character-name-query") " ")
                                                      characters-headlines)))
            ;; Character has been chosen by user for deletion.
            ;; Delete character's file and remove it from the index.
            (orgn--delete-character-files-for chosen-character story-folder)
            (orgn--delete-character-from-index chosen-character story-folder))
        (progn
          (message (concat characters-file ": " (orgn--ls "no-chapters-found")))
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (throw 'CHARACTER-DELETION-FAULT (concat characters-file ": " (orgn--ls "no-chapters-found")))))
      (if (file-exists-p characters-file)
          (if (file-readable-p characters-file)
              (find-file characters-file)
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat characters-file " " (orgn--ls "is-not-readable")))
              (throw 'CHARACTER-DELETION-FAULT (concat characters-file " " (orgn--ls "is-not-readable")))))
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (error (concat (orgn--ls "file-not-found") ": " characters-file))
          (throw 'CHARACTER-DELETION-FAULT (concat (orgn--ls "file-not-found") ": " characters-file))))
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-rename-character ()
  "Rename a character from the character index and update associated files."
  (interactive)
  (catch 'RENAME-CHARACTER-FAULT-AT-INDEX
    (let* ((story-folder (orgn--story-root-folder))
           (indices-folder (orgn--ls "indices-folder"))
           (characters-headlines (orgn--get-all-story-characters-headlines story-folder))
           chosen-character
           new-character-name
           (characters-file (concat story-folder / indices-folder / (orgn--ls "characters-file" orgn--file-ending))))
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      ;; characters-headlines now contains a list of all characters to present to user for renaming.
      ;; This includes files without an entry in the index, and entries in the index without a file.
      (if characters-headlines
          (progn
            (while (not chosen-character)
              (setq chosen-character (completing-read (concat (orgn--ls "character-name-query") " ")
                                                      characters-headlines)))
            ;; Character has been chosen for renaming by user. What is the new name?
            (setq new-character-name (orgn--sanitize-string (read-string (concat (orgn--ls "new-character-name-query") " "))))
            ;; Rename character file and internal headings, and rename character in the index.
            (orgn--rename-character-files-for chosen-character new-character-name story-folder)
            (orgn--rename-character-in-index chosen-character new-character-name story-folder))
        (progn
          (message (concat characters-file ": " (orgn--ls "no-characters-found")))
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (throw 'RENAME-CHARACTER-FAULT-AT-INDEX (concat characters-file ": " (orgn--ls "no-chapters-found")))))
      (if (file-exists-p characters-file)
          (if (file-readable-p characters-file)
              (find-file characters-file)
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat characters-file " " (orgn--ls "is-not-readable")))
              (throw 'RENAME-CHARACTER-FAULT-AT-INDEX (concat characters-file " " (orgn--ls "is-not-readable")))))
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (error (concat (orgn--ls "file-not-found") ": " characters-file))
          (throw 'RENAME-CHARACTER-FAULT-AT-INDEX (concat (orgn--ls "file-not-found") ": " characters-file))))
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-new-prop (prop-name)
  "Create and modify the minimum number of linked files for a new prop.
PROP-NAME will be the name given to the prop."
  (interactive (list (read-string (concat (orgn--ls "prop-name-query") " "))))
  (setq prop-name (orgn--sanitize-string prop-name))
  (catch 'PROP-CREATION-FAULT
    (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
           (props-file (orgn--ls "props-file" orgn--file-ending))
           (indices-folder (orgn--ls "indices-folder"))
           (notes-folder (orgn--ls "notes-folder"))
           (story-name (orgn--story-name story-folder))
           (existing-props (orgn--prop-hash-table story-folder))
           (prop-file (concat (orgn--ls "prop-file-prefix") (orgn--system-safe-name prop-name) orgn--file-ending))
           (keys (hash-table-keys existing-props))
           key
           insert-point)
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      (unless (file-exists-p (concat story-folder / indices-folder / props-file))
        (orgn--populate-props-template story-name story-folder))
      (while keys
        (setq key (pop keys))
        (when (string= prop-file key)
          (progn
            (completing-read (concat (orgn--ls "name-already-in-use") " ") (list (orgn--ls "okay")))
            (setq prop-name (read-string (concat (orgn--ls "prop-name-query") " ")))
            (orgn-new-prop prop-name)  ; Call this function again
            (setq orgn-automatic-referencing-p orgn--autoref-p)
            (throw 'PROP-CREATION-FAULT (concat (orgn--ls "name-already-in-use") ": " story-folder / notes-folder / prop-file)))))
      (find-file (concat story-folder / indices-folder / props-file))
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-min))
      (org-novelist-mode)
      (orgn--fold-show-all)  ; Belts and braces
      (when (not (org-next-visible-heading 1))
        (if (and (string= (orgn--heading-last-link-headline-text) (orgn--story-name story-folder))
                 (= 1 (org-current-level)))
            (if (org-goto-first-child)
                ;; Existing entries found.
                (progn
                  (re-search-backward org-outline-regexp-bol nil t)  ; Go to parent heading
                  (org-end-of-subtree)  ; Go to end of last subheading in child tree
                  (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                  (org-todo)  ; Turn heading into a TODO item
                  (setq insert-point (point))
                  (goto-char (point-min))
                  (orgn--delete-line)
                  (goto-char (- insert-point 1)))
              ;; First entry.
              (progn
                (org-end-of-subtree)  ; Go to end of last subheading
                (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                (org-demote)  ; Turn heading into a subheading
                (org-todo)  ; Turn heading into a TODO item
                (setq insert-point (point))
                (goto-char (point-min))
                (orgn--delete-line)
                (goto-char (- insert-point 1))))
          ;; File Malformed
          (progn
            (goto-char (point-min))
            (orgn--delete-line)
            (orgn--rebuild-props-index story-folder)
            (orgn-new-prop prop-name)  ; Call this function again
            (setq orgn-automatic-referencing-p orgn--autoref-p)
            (throw 'PROP-CREATION-FAULT (concat (orgn--ls "file-malformed") ": " story-folder / indices-folder / props-file)))))
      ;; By here, point should be at the correct location to create the new prop.
      (save-excursion  ; Allow cursor to be placed at the start of the new prop name
        (orgn--make-prop-at-index-point prop-name))  ; Create prop at point
      (save-buffer)
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-destroy-prop ()
  "Remove a prop from the prop index, and delete associated files."
  (interactive)
  (catch 'PROP-DELETION-FAULT
    (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
           (indices-folder (orgn--ls "indices-folder"))
           (props-file (concat story-folder / indices-folder / (orgn--ls "props-file" orgn--file-ending)))
           (props-headlines (orgn--get-all-story-props-headlines story-folder))
           chosen-prop)
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      ;; props-headlines now contains a list of all props to present to user for deletion.
      ;; This includes files without an entry in the index, and entries in the index without a file.
      (if props-headlines
          (progn
            (while (not chosen-prop)
              (setq chosen-prop (completing-read (concat (orgn--ls "prop-name-query") " ")
                                                 props-headlines)))
            ;; Prop has been chosen by user for deletion.
            ;; Delete prop's file and remove it from the index.
            (orgn--delete-prop-files-for chosen-prop story-folder)
            (orgn--delete-prop-from-index chosen-prop story-folder))
        (progn
          (message (concat props-file ": " (orgn--ls "no-props-found")))
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (throw 'PROP-DELETION-FAULT (concat props-file ": " (orgn--ls "no-props-found")))))
      (if (file-exists-p props-file)
          (if (file-readable-p props-file)
              (find-file props-file)
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat props-file " " (orgn--ls "is-not-readable")))
              (throw 'PROP-DELETION-FAULT (concat props-file " " (orgn--ls "is-not-readable")))))
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (error (concat (orgn--ls "file-not-found") ": " props-file))
          (throw 'PROP-DELETION-FAULT (concat (orgn--ls "file-not-found") ": " props-file))))
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-rename-prop ()
  "Rename a prop from the prop index and update associated files."
  (interactive)
  (catch 'RENAME-PROP-FAULT-AT-INDEX
    (let* ((story-folder (orgn--story-root-folder))
           (indices-folder (orgn--ls "indices-folder"))
           (props-headlines (orgn--get-all-story-props-headlines story-folder))
           chosen-prop
           new-prop-name
           (props-file (concat story-folder / indices-folder / (orgn--ls "props-file" orgn--file-ending))))
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      ;; props-headlines now contains a list of all props to present to user for renaming.
      ;; This includes files without an entry in the index, and entries in the index without a file.
      (if props-headlines
          (progn
            (while (not chosen-prop)
              (setq chosen-prop (completing-read (concat (orgn--ls "prop-name-query") " ")
                                                 props-headlines)))
            ;; Prop has been chosen for renaming by user. What is the new name?
            (setq new-prop-name (orgn--sanitize-string (read-string (concat (orgn--ls "new-prop-name-query") " "))))
            ;; Rename prop file and internal headings, and rename prop in the index.
            (orgn--rename-prop-files-for chosen-prop new-prop-name story-folder)
            (orgn--rename-prop-in-index chosen-prop new-prop-name story-folder))
        (progn
          (message (concat props-file ": " (orgn--ls "no-props-found")))
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (throw 'RENAME-PROP-FAULT-AT-INDEX (concat props-file ": " (orgn--ls "no-props-found")))))
      (if (file-exists-p props-file)
          (if (file-readable-p props-file)
              (find-file props-file)
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat props-file " " (orgn--ls "is-not-readable")))
              (throw 'RENAME-PROP-FAULT-AT-INDEX (concat props-file " " (orgn--ls "is-not-readable")))))
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (error (concat (orgn--ls "file-not-found") ": " props-file))
          (throw 'RENAME-PROP-FAULT-AT-INDEX (concat (orgn--ls "file-not-found") ": " props-file))))
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-new-place (place-name)
  "Create and modify the minimum number of linked files for a new place.
PLACE-NAME will be the name given to the place."
  (interactive (list (read-string (concat (orgn--ls "place-name-query") " "))))
  (setq place-name (orgn--sanitize-string place-name))
  (catch 'PLACE-CREATION-FAULT
    (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
           (places-file (orgn--ls "places-file" orgn--file-ending))
           (indices-folder (orgn--ls "indices-folder"))
           (notes-folder (orgn--ls "notes-folder"))
           (story-name (orgn--story-name story-folder))
           (existing-places (orgn--place-hash-table story-folder))
           (place-file (concat (orgn--ls "place-file-prefix") (orgn--system-safe-name place-name) orgn--file-ending))
           (keys (hash-table-keys existing-places))
           key
           insert-point)
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      (unless (file-exists-p (concat story-folder / indices-folder / places-file))
        (orgn--populate-places-template story-name story-folder))
      (while keys
        (setq key (pop keys))
        (when (string= place-file key)
          (progn
            (completing-read (concat (orgn--ls "name-already-in-use") " ") (list (orgn--ls "okay")))
            (setq place-name (read-string (concat (orgn--ls "place-name-query") " ")))
            (orgn-new-place place-name)  ; Call this function again
            (setq orgn-automatic-referencing-p orgn--autoref-p)
            (throw 'PLACE-CREATION-FAULT (concat (orgn--ls "name-already-in-use") ": " story-folder / notes-folder / place-file)))))
      (find-file (concat story-folder / indices-folder / places-file))
      (goto-char (point-min))
      (insert "\n")
      (goto-char (point-min))
      (org-novelist-mode)
      (orgn--fold-show-all)  ; Belts and braces
      (when (not (org-next-visible-heading 1))
        (if (and (string= (orgn--heading-last-link-headline-text) (orgn--story-name story-folder))
                 (= 1 (org-current-level)))
            (if (org-goto-first-child)
                ;; Existing entries found.
                (progn
                  (re-search-backward org-outline-regexp-bol nil t)  ; Go to parent heading
                  (org-end-of-subtree)  ; Go to end of last subheading in child tree
                  (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                  (org-todo)  ; Turn heading into a TODO item
                  (setq insert-point (point))
                  (goto-char (point-min))
                  (orgn--delete-line)
                  (goto-char (- insert-point 1)))
              ;; First entry.
              (progn
                (org-end-of-subtree)  ; Go to end of last subheading
                (org-insert-heading-respect-content)  ; Add the beginning of a new heading at the end of the current tree
                (org-demote)  ; Turn heading into a subheading
                (org-todo)  ; Turn heading into a TODO item
                (setq insert-point (point))
                (goto-char (point-min))
                (orgn--delete-line)
                (goto-char (- insert-point 1))))
          ;; File Malformed
          (progn
            (goto-char (point-min))
            (orgn--delete-line)
            (orgn--rebuild-places-index story-folder)
            (orgn-new-place place-name)  ; Call this function again
            (setq orgn-automatic-referencing-p orgn--autoref-p)
            (throw 'PLACE-CREATION-FAULT (concat (orgn--ls "file-malformed") ": " story-folder / indices-folder / places-file)))))
      ;; By here, point should be at the correct location to create the new place.
      (save-excursion  ; Allow cursor to be placed at the start of the new place name
        (orgn--make-place-at-index-point place-name))  ; Create place at point
      (save-buffer)
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-destroy-place ()
  "Remove a place from the place index, and delete associated files."
  (interactive)
  (catch 'PLACE-DELETION-FAULT
    (let* ((story-folder (orgn--story-root-folder))  ; Get the story's root directory for the buffer being displayed when this function is called. This also ensures we're in a story folder.
           (indices-folder (orgn--ls "indices-folder"))
           (places-file (concat story-folder / indices-folder / (orgn--ls "places-file" orgn--file-ending)))
           (places-headlines (orgn--get-all-story-places-headlines story-folder))
           chosen-place)
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      ;; places-headlines now contains a list of all places to present to user for deletion.
      ;; This includes files without an entry in the index, and entries in the index without a file.
      (if places-headlines
          (progn
            (while (not chosen-place)
              (setq chosen-place (completing-read (concat (orgn--ls "place-name-query") " ")
                                                  places-headlines)))
            ;; Place has been chosen by user for deletion.
            ;; Delete place's file and remove it from the index.
            (orgn--delete-place-files-for chosen-place story-folder)
            (orgn--delete-place-from-index chosen-place story-folder))
        (progn
          (message (concat places-file ": " (orgn--ls "no-places-found")))
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (throw 'PLACE-DELETION-FAULT (concat places-file ": " (orgn--ls "no-places-found")))))
      (if (file-exists-p places-file)
          (if (file-readable-p places-file)
              (find-file places-file)
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat places-file " " (orgn--ls "is-not-readable")))
              (throw 'PLACE-DELETION-FAULT (concat places-file " " (orgn--ls "is-not-readable")))))
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (error (concat (orgn--ls "file-not-found") ": " places-file))
          (throw 'PLACE-DELETION-FAULT (concat (orgn--ls "file-not-found") ": " places-file))))
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-rename-place ()
  "Rename a place from the place index and update associated files."
  (interactive)
  (catch 'RENAME-PLACE-FAULT-AT-INDEX
    (let* ((story-folder (orgn--story-root-folder))
           (indices-folder (orgn--ls "indices-folder"))
           (places-headlines (orgn--get-all-story-places-headlines story-folder))
           chosen-place
           new-place-name
           (places-file (concat story-folder / indices-folder / (orgn--ls "places-file" orgn--file-ending))))
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      ;; places-headlines now contains a list of all places to present to user for renaming.
      ;; This includes files without an entry in the index, and entries in the index without a file.
      (if places-headlines
          (progn
            (while (not chosen-place)
              (setq chosen-place (completing-read (concat (orgn--ls "place-name-query") " ")
                                                  places-headlines)))
            ;; Place has been chosen for renaming by user. What is the new name?
            (setq new-place-name (orgn--sanitize-string (read-string (concat (orgn--ls "new-place-name-query") " "))))
            ;; Rename place file and internal headings, and rename place in the index.
            (orgn--rename-place-files-for chosen-place new-place-name story-folder)
            (orgn--rename-place-in-index chosen-place new-place-name story-folder))
        (progn
          (message (concat places-file ": " (orgn--ls "no-places-found")))
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (throw 'RENAME-PLACE-FAULT-AT-INDEX (concat places-file ": " (orgn--ls "no-places-found")))))
      (if (file-exists-p places-file)
          (if (file-readable-p places-file)
              (find-file places-file)
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat places-file " " (orgn--ls "is-not-readable")))
              (throw 'RENAME-PLACE-FAULT-AT-INDEX (concat places-file " " (orgn--ls "is-not-readable")))))
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (error (concat (orgn--ls "file-not-found") ": " places-file))
          (throw 'RENAME-PLACE-FAULT-AT-INDEX (concat (orgn--ls "file-not-found") ": " places-file))))
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-update-references (&optional story-folder)
  "Given a STORY-FOLDER, update all crossreferences in story."
  (interactive)
  (if (not story-folder)
      (setq story-folder (orgn--story-root-folder))
    (setq story-folder (orgn--story-root-folder story-folder)))
  (let ((curr-buff (current-buffer))
        (curr-pos (point)))
    (if orgn-automatic-referencing-p
        (progn
          (setq orgn--autoref-p orgn-automatic-referencing-p)
          (setq orgn-automatic-referencing-p nil)
          ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
          (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
          ;; (orgn--rebuild-indices story-folder)  ; This should really only be called when one of the functions manipulating an index are used
          (orgn--update-object-references story-folder)
          (orgn--update-glossaries story-folder)  ; Always call this after object-references, because object-references will delete all glossaries
          (setq orgn-automatic-referencing-p t)
          ;; Remove hook to reset automatic referencing since we made it to the end of the function.
          (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))
      (progn
        ;; (orgn--rebuild-indices story-folder)  ; This should really only be called when one of the functions manipulating an index are used
        (orgn--update-object-references story-folder)
        (orgn--update-glossaries story-folder)))  ; Always call this after object-references, because object-references will delete all glossaries
    (switch-to-buffer curr-buff)
    (goto-char curr-pos)))

(defun orgn-rename-story ()
  "Rename the story and update all necessary files."
  (interactive)
  (catch 'RENAME-STORY-FAULT
    (let* ((story-folder (orgn--story-root-folder))
           (story-name (orgn--story-name))
           (story-files (directory-files-recursively story-folder (format "^%s%s\\'" (orgn--ls "sys-safe-name") orgn--file-ending)))
           new-story-name
           new-story-folder-name
           curr-file
           rename-story-folder-p)
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      (setq new-story-name (orgn--sanitize-string (read-string (concat (orgn--ls "new-story-name-query") " "))))
      (setq new-story-folder-name (orgn--system-safe-name new-story-name))
      ;; Ask user if they want to rename folder as well.
      (setq rename-story-folder-p (yes-or-no-p (orgn--ls "rename-story-folder-query")))
      (while story-files
        (setq curr-file (pop story-files))
        ;; If filename starts with a period, don't add it.
        (unless (string= (substring (file-name-nondirectory curr-file) 0 1) ".")
          ;; If open in buffer, save and close.
          (when (and rename-story-folder-p (get-file-buffer curr-file))
            (switch-to-buffer (get-file-buffer curr-file))
            (save-buffer)
            (kill-buffer))
          ;; Don't add the main.org file.
          (unless (string= (file-name-nondirectory curr-file) (concat (orgn--ls "main-file") orgn--file-ending))
            (orgn--string-to-file
             (orgn--replace-string-in-string (concat "\[\[file:.." / (orgn--ls "main-file")
                                                     orgn--file-ending "\]\[" story-name "\]\]")
                                             (concat "\[\[file:.." / (orgn--ls "main-file")
                                                     orgn--file-ending "\]\[" new-story-name "\]\]")
                                             (org-file-contents curr-file)) curr-file))))
      ;; Change title in main.org.
      (orgn--set-story-name new-story-name story-folder)
      (when (and rename-story-folder-p (get-file-buffer (concat story-folder / (orgn--ls "main-file") orgn--file-ending)))
        (switch-to-buffer (get-file-buffer curr-file))
        (save-buffer)
        (kill-buffer))
      (when rename-story-folder-p
        (if (not (file-exists-p (expand-file-name (concat story-folder / ".." / new-story-folder-name))))
            (if (file-writable-p (expand-file-name (concat story-folder / ".." / new-story-folder-name)))
                (progn
                  (copy-directory story-folder (expand-file-name (concat story-folder / ".." / new-story-folder-name)) t t t)
                  (delete-directory story-folder t nil)
                  (find-file (concat (expand-file-name (concat story-folder / ".." / new-story-folder-name)) / (orgn--ls "main-file") orgn--file-ending))
                  (setq story-folder new-story-folder-name))
              (progn
                (setq orgn-automatic-referencing-p orgn--autoref-p)
                (find-file (concat story-folder / (orgn--ls "main-file") orgn--file-ending))
                (user-error (concat (expand-file-name (concat story-folder / ".." / new-story-folder-name)) " " (orgn--ls "is-not-writable")))
                (throw 'RENAME-STORY-FAULT (concat (expand-file-name (concat story-folder / ".." / new-story-folder-name)) " " (orgn--ls "is-not-writable")))))
          (progn
            (setq orgn-automatic-referencing-p orgn--autoref-p)
            (find-file (concat story-folder / (orgn--ls "main-file") orgn--file-ending))
            (user-error (concat (orgn--ls "folder-already-exists") ": " (expand-file-name (concat story-folder / ".." / new-story-folder-name))))
            (throw 'RENAME-STORY-FAULT (concat (orgn--ls "folder-already-exists") ": " (expand-file-name (concat story-folder / ".." / new-story-folder-name)))))))
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))

(defun orgn-export-story ()
  "Export story to specified formats according to org-novelist-config.org.
After checking the indices are not malformed, this function will first
construct a single Org file, made up from the chapter files of the
story. The order of the chapters will be taken from the chapter index,
with matter sections correctly ordered (not user selected), and the
chapters labelled with their matter type. Chapter file properties will
be moved to property drawers in the output Org file for each chapter
heading.
Once Org file is created, go though any export settings in the story
config file and apply them to the Org file.
Once the single Org file is complete, go through any out export
templates specified in config file and run them to create all specified
export files."
  (interactive)
  (catch 'EXPORT-STORY-FAULT
    (let* ((story-folder (orgn--story-root-folder))
           (indices-folder (orgn--ls "indices-folder"))
           (exports-folder (orgn--ls "exports-folder"))
           (story-name (orgn--story-name story-folder))
           (chapter-index (concat (orgn--ls "chapters-file") orgn--file-ending))
           (fm-file-list '())
           (mm-file-list '())
           (bm-file-list '())
           curr-chap-file
           curr-properties-list
           curr-property
           (content "")
           exports-hash
           keys
           key)
      (setq orgn--autoref-p orgn-automatic-referencing-p)
      (setq orgn-automatic-referencing-p nil)
      ;; Temporarily add a hook to reset automatic referencing in case user aborts minibuffer.
      (add-hook 'post-command-hook 'orgn--reset-automatic-referencing)
      (orgn--rebuild-indices story-folder)  ; Make sure the chapter index is in good condition (this function actually checks all indices)
      (if (file-exists-p (concat story-folder / indices-folder / chapter-index))
          (if (file-readable-p (concat story-folder / indices-folder / chapter-index))
              (progn
                (find-file (concat story-folder / indices-folder / chapter-index))
                ;; If there is a front matter in chapter index, get files in order and add to front matter list
                (with-temp-buffer
                  (insert-file-contents (concat story-folder / indices-folder / chapter-index))
                  (goto-char (point-min))
                  (insert "\n")
                  (goto-char (point-min))
                  (org-novelist-mode)
                  (orgn--fold-show-all)  ; Belts and braces
                  (while (not (org-next-visible-heading 1))
                    (when (string= (orgn--ls "front-matter-heading") (nth 4 (org-heading-components)))
                      ;; Found front matter, get files in order and add to list.
                      (when (org-goto-first-child)
                        (setq fm-file-list (cons (orgn--heading-last-link-absolute-link-text) fm-file-list))
                        (while (org-goto-sibling)
                          (setq fm-file-list (cons (orgn--heading-last-link-absolute-link-text) fm-file-list))))
                      (goto-char (point-max))))  ; No need to check any more, so skip to the end go exit loop
                  (setq fm-file-list (reverse fm-file-list)))  ; Put files back in order
                ;; If there is a main matter in chapter index, get files in order and add to main matter list
                (with-temp-buffer
                  (insert-file-contents (concat story-folder / indices-folder / chapter-index))
                  (goto-char (point-min))
                  (insert "\n")
                  (goto-char (point-min))
                  (org-novelist-mode)
                  (orgn--fold-show-all)  ; Belts and braces
                  (while (not (org-next-visible-heading 1))
                    (when (string= (orgn--ls "main-matter-heading") (nth 4 (org-heading-components)))
                      ;; Found main matter, get files in order and add to list.
                      (when (org-goto-first-child)
                        (setq mm-file-list (cons (orgn--heading-last-link-absolute-link-text) mm-file-list))
                        (while (org-goto-sibling)
                          (setq mm-file-list (cons (orgn--heading-last-link-absolute-link-text) mm-file-list))))
                      (goto-char (point-max))))  ; No need to check any more, to skip to the end go exit loop
                  (setq mm-file-list (reverse mm-file-list)))
                ;; If there is a back matter in chapter index, get files in order and add to back matter list
                (with-temp-buffer
                  (insert-file-contents (concat story-folder / indices-folder / chapter-index))
                  (goto-char (point-min))
                  (insert "\n")
                  (goto-char (point-min))
                  (org-novelist-mode)
                  (orgn--fold-show-all)  ; Belts and braces
                  (while (not (org-next-visible-heading 1))
                    (when (string= (orgn--ls "back-matter-heading") (nth 4 (org-heading-components)))
                      ;; Found back matter, get files in order and add to list.
                      (when (org-goto-first-child)
                        (setq bm-file-list (cons (orgn--heading-last-link-absolute-link-text) bm-file-list))
                        (while (org-goto-sibling)
                          (setq bm-file-list (cons (orgn--heading-last-link-absolute-link-text) bm-file-list))))
                      (goto-char (point-max))))  ; No need to check any more, to skip to the end go exit loop
                  (setq bm-file-list (reverse bm-file-list))))
            (progn
              (setq orgn-automatic-referencing-p orgn--autoref-p)
              (error (concat chapter-index " " (orgn--ls "is-not-readable")))
              (throw 'EXPORT-STORY-FAULT (concat chapter-index " " (orgn--ls "is-not-readable")))))
        (progn
          (setq orgn-automatic-referencing-p orgn--autoref-p)
          (error (concat (orgn--ls "file-not-found") ": " chapter-index))
          (throw 'EXPORT-STORY-FAULT (concat (orgn--ls "file-not-found") ": " chapter-index))))
      ;; Correctly ordered file lists have been made.
      (while fm-file-list
        (setq curr-chap-file (expand-file-name (pop fm-file-list)))
        (setq curr-properties-list (delete "TITLE" (orgn--get-file-properties curr-chap-file)))
        (with-temp-buffer
          (org-novelist-mode)
          (orgn--fold-show-all)  ; Belts and braces
          (insert (concat "* " (orgn--get-file-property-value curr-chap-file "TITLE") "\n"))
          (org-set-property (orgn--ls "matter-type-property") (upcase (orgn--ls "front-matter-heading")))
          (while curr-properties-list
            (setq curr-property (pop curr-properties-list))
            (unless (org-entry-get (point) curr-property)
              (org-set-property curr-property (orgn--get-file-property-value curr-chap-file curr-property))))
          ;; Chapter setup with properties, just add contents.
          (goto-char (buffer-size))
          (insert "\n")
          (insert (orgn--get-file-subtree curr-chap-file (orgn--ls "content-header") t))
          (setq content (concat content (buffer-substring (point-min) (buffer-size))))))
      (while mm-file-list
        (setq curr-chap-file (pop mm-file-list))
        (setq curr-properties-list (delete "TITLE" (orgn--get-file-properties curr-chap-file)))
        (with-temp-buffer
          (org-novelist-mode)
          (orgn--fold-show-all)  ; Belts and braces
          (insert (concat "* " (orgn--get-file-property-value curr-chap-file "TITLE") "\n"))
          (org-set-property (orgn--ls "matter-type-property") (upcase (orgn--ls "main-matter-heading")))
          (while curr-properties-list
            (setq curr-property (pop curr-properties-list))
            (unless (org-entry-get (point) curr-property)
              (org-set-property curr-property (orgn--get-file-property-value curr-chap-file curr-property))))
          ;; Chapter setup with properties, just add contents.
          (goto-char (buffer-size))
          (insert "\n")
          (insert (orgn--get-file-subtree curr-chap-file (orgn--ls "content-header") t))
          (setq content (concat content (buffer-substring (point-min) (buffer-size))))))
      (while bm-file-list
        (setq curr-chap-file (expand-file-name (pop bm-file-list)))
        (setq curr-properties-list (delete "TITLE" (orgn--get-file-properties curr-chap-file)))
        (with-temp-buffer
          (org-novelist-mode)
          (orgn--fold-show-all)  ; Belts and braces
          (insert (concat "* " (orgn--get-file-property-value curr-chap-file "TITLE") "\n"))
          (org-set-property (orgn--ls "matter-type-property") (upcase (orgn--ls "back-matter-heading")))
          (while curr-properties-list
            (setq curr-property (pop curr-properties-list))
            (unless (org-entry-get (point) curr-property)
              (org-set-property curr-property (orgn--get-file-property-value curr-chap-file curr-property))))
          ;; Chapter setup with properties, just add contents.
          (goto-char (buffer-size))
          (insert "\n")
          (insert (orgn--get-file-subtree curr-chap-file (orgn--ls "content-header") t))
          (setq content (concat content (buffer-substring (point-min) (buffer-size))))))
      ;; Generate Org export file.
      (orgn--populate-export-org-template
       story-name
       orgn-author
       orgn-author-email
       (format-time-string "[%Y-%m-%d %a %H:%M]")
       content
       (concat story-folder / exports-folder / (orgn--system-safe-name story-name) orgn--file-ending))
      ;; Although Org export file is made, the file level properties may need to be overridden by the config file.
      ;; Find all properties in config file, then go through each and add/overwrite what is in Org export file.
      ;; Save the results.
      (when (file-exists-p (concat story-folder / orgn--config-filename))
        (setq curr-properties-list (orgn--get-file-properties (concat story-folder / orgn--config-filename)))
        (while curr-properties-list
          (setq curr-property (pop curr-properties-list))
          (orgn--set-file-property-value curr-property
                                         (orgn--get-file-property-value (concat story-folder / orgn--config-filename) curr-property)
                                         (concat story-folder / exports-folder / (orgn--system-safe-name story-name) orgn--file-ending)))
        (orgn--save-current-file))
      ;; By this point, we should have the Org file correctly exported.
      ;; Run through the export templates in the config file.
      (setq exports-hash (orgn--exports-hash-table story-folder))
      (setq keys (hash-table-keys exports-hash))
      (while keys
        (setq key (pop keys))
        (load-file (expand-file-name (gethash key exports-hash)))
        (declare-function org-novelist--export-template (concat "ext:" (gethash key exports-hash)) (org-input-file output-file))
        (org-novelist--export-template (concat story-folder / exports-folder / (orgn--system-safe-name story-name) orgn--file-ending) key))
      ;; Open exported Org file.
      (find-file (concat story-folder / exports-folder / (orgn--system-safe-name story-name) orgn--file-ending))
      (setq orgn-automatic-referencing-p orgn--autoref-p)
      (when orgn-automatic-referencing-p
        (orgn-update-references story-folder))
      ;; Remove hook to reset automatic referencing since we made it to the end of the function.
      (remove-hook 'post-command-hook 'orgn--reset-automatic-referencing))))




;;;###autoload
(define-derived-mode org-novelist-mode org-mode "Org Novelist"
  "An Org mode system for writing and exporting fiction novels.
Alias: \"JUF's methodology for keeping novel writing nice and tidy.\"

Org Novelist is a methodology for writing novel-length fiction using
Org mode within Emacs. It involves creating and laying out Org mode
files such that notes and plans can be easily created and quickly
accessed while writing the main text of a story. Org Novelist's
secondary function is the ability to use this known structure to
easily export and publish stories to other formats. This package
supplies a collection of support functions which make it easier to
use this methodology.

Creating, linking, and laying out files in the Org Novelist
methodology can be done without the use of Emacs or the Org Novelist
package, but using the package within Emacs will provide helper
functions that make the methodology much easier to use; allowing the
following of links, programmatic updating of crossreferences, and
the ability to programatically export to other formats.

The following commands are available:

`org-novelist-new-story'
`org-novelist-new-chapter'
`org-novelist-destroy-chapter'
`org-novelist-rename-chapter'
`org-novelist-new-character'
`org-novelist-destroy-character'
`org-novelist-rename-character'
`org-novelist-new-prop'
`org-novelist-destroy-prop'
`org-novelist-rename-prop'
`org-novelist-new-place'
`org-novelist-destroy-place'
`org-novelist-rename-place'
`org-novelist-update-references'
`org-novelist-rename-story'
`org-novelist-export-story'"
  (add-hook 'after-save-hook 'orgn--update-references-after-save-hook))

(provide 'org-novelist)
;;; org-novelist.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("orgn-" . "org-novelist-"))
;; End:
