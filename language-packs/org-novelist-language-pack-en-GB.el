;;; org-novelist-language-pack-en-GB.el --- Default Language Pack  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 John Urquhart Ferguson
;;
;; Author: John Urquhart Ferguson <mail@johnurquhartferguson.info>
;; Maintainer: John Urquhart Ferguson <mail@johnurquhartferguson.info>
;; URL: https://johnurquhartferguson.info
;; Keywords: fiction, writing, outlines
;; Prefix: org-novelist
;; Package-Requires: ((org "9.5.5")) 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;;; British English (en-GB)

;;; Code:
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

(provide 'org-novelist-language-pack-en-GB)
;;; org-novelist-language-pack-en-GB.el ends here
