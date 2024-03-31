;;; org-novelist-language-pack-en-us.el --- Org Novelist language pack for en-US (American English) -*- lexical-binding: t; -*-

;; Language pack for Org Novelist.
;; Copyright (c) 2024 John Urquhart Ferguson
;;
;; Author: John Urquhart Ferguson <mail@johnurquhartferguson.info>
;; Maintainer: John Urquhart Ferguson <mail@johnurquhartferguson.info>
;; URL: https://johnurquhartferguson.info
;; Keywords: fiction, writing, outlines, languages, translation
;; Prefix: org-novelist
;; Package-Requires: ((emacs "28.1") (org "9.5.5"))

;; Version 0.0.1

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
;; supplies a language pack for English, as spoken in America, to be
;; used with Org Novelist.
;;
;; Creating, linking, and laying out files in the Org Novelist
;; methodology can be done without the use of Emacs or the Org Novelist
;; package, but using the package within Emacs will provide helper
;; functions that make the methodology much easier to use; allowing the
;; following of links, programmatic updating of cross-references, and
;; the ability to programmatically export to other formats.
;;
;; Installation, Activation, and Documentation
;; -------------------------------------------
;; See the corresponding section of the website at
;;
;;   https://johnurquhartferguson.info
;;
;;; Code:

;;;; Require other packages

(require 'org-novelist)  ; Org Novelist string manipulation functions should be available for use in language packs


;;;; American English (en-US)
;; User Variable Fallback Strings
(defconst orgn--author-not-set-en-US "Author Not Set" "Author not specified by user.")
(defconst orgn--author-email-not-set-en-US "Author Email Not Set" "Author email not specified by user.")
;; File Instructions
(defconst orgn--main-file-instructions-en-US "Write a brief summary of the story here" "Instructions for the main entry-point file.")
(defconst orgn--notes-file-instructions-en-US "Write any general notes for the story here" "Instructions for the general notes file.")
(defconst orgn--research-file-instructions-en-US "Write any research notes for the story here" "Instructions for the general research file.")
(defconst orgn--characters-file-instructions-en-US "This is an index of the characters in the story. Do not edit manually. Use only Org mode or Org Novelist functions." "Instructions for the character index file.")
(defconst orgn--places-file-instructions-en-US "This is an index of the places in the story. Do not edit manually. Use only Org mode or Org Novelist functions." "Instructions for the location index file.")
(defconst orgn--props-file-instructions-en-US "This is an index of the props in the story. Do not edit manually. Use only Org mode or Org Novelist functions." "Instructions for the prop index file.")
(defconst orgn--chapters-file-instructions-en-US "This is an index of the chapters in the story. Do not edit manually. Use only Org mode or Org Novelist functions." "Instructions for the chapter index file.")
(defconst orgn--linked-stories-file-instructions-en-US "This is an index of linked stories. Do not edit manually. Use only Org mode or Org Novelist functions." "Instructions for the linked stories index file.")
;; Folder Names
(defconst orgn--notes-folder-en-US "Notes" "The folder name for storing note files.")
(defconst orgn--indices-folder-en-US "Indices" "The folder name for storing index files.")
(defconst orgn--chapters-folder-en-US "Chapters" "The folder name for storing the chapter files.")
(defconst orgn--exports-folder-en-US "Exports" "The folder name for storing export files.")
;; File Names
(defconst orgn--main-file-en-US "main" "Name for the story's main entry-point file.")
(defconst orgn--notes-file-en-US "notes" "Name for the story's general notes file.")
(defconst orgn--research-file-en-US "research" "Name for the story's general research file.")
(defconst orgn--characters-file-en-US "characters" "Name for the story's character index file.")
(defconst orgn--places-file-en-US "places" "Name for the story's location index file.")
(defconst orgn--props-file-en-US "props" "Name for the story's prop index file.")
(defconst orgn--chapters-file-en-US "chapters" "Name for the story's chapter index file.")
(defconst orgn--linked-stories-file-en-US "linked-stories" "Name for the story's linked stories index file.")
(defconst orgn--chapter-file-prefix-en-US "chapter-" "Prefix for the story's chapter files.")
(defconst orgn--notes-suffix-en-US "-notes" "Suffix for a file's associated notes file.")
(defconst orgn--character-file-prefix-en-US "character-" "Prefix for the story's character files.")
(defconst orgn--prop-file-prefix-en-US "prop-" "Prefix for the story's prop files.")
(defconst orgn--place-file-prefix-en-US "place-" "Prefix for the story's place files.")
;; File Titles
(defconst orgn--notes-title-en-US "Notes" "Name for the story's general notes title.")
(defconst orgn--research-title-en-US "Research" "Name for the story's general research title.")
(defconst orgn--characters-title-en-US "Characters" "Name for the story's character index title.")
(defconst orgn--places-title-en-US "Places" "Name for the story's location index title.")
(defconst orgn--props-title-en-US "Props" "Name for the story's prop index title.")
(defconst orgn--chapters-title-en-US "Chapters" "Name for the story's chapter index title.")
(defconst orgn--config-name-en-US "Export Settings" "Display name for a link to the story's configuration file.")
(defconst orgn--linked-stories-title-en-US "Linked Stories" "Name for the story's linked stories index title.")
;; File Preambles
(defconst orgn--story-name-en-US "story name" "Placeholder for the name of the story, used in generating template preambles.")
(defconst orgn--chapter-name-en-US "chapter name" "Placeholder for the name of a chapter, used in generating template preambles.")
;; <<story name>> (without the << >> brackets) must share the same value as org-novelist--story-name-en-US.
;; <<chapter name>> (without the << >> brackets) must share the same value as org-novelist--chapter-name-en-US.
(defconst orgn--notes-for-story-name-en-US "Notes for <<story name>>" "Part of the preamble for the general notes file.")
(defconst orgn--notes-for-chapter-name-en-US "Notes for <<chapter name>>" "Part of the preamble for the chapter notes file.")
(defconst orgn--research-for-story-name-en-US "Research for <<story name>>" "Part of the preamble for the general research file.")
(defconst orgn--character-index-for-story-name-en-US "Character Index for <<story name>>" "Part of the preamble for the character index file.")
(defconst orgn--place-index-for-story-name-en-US "Place Index for <<story name>>" "Part of the preamble for the location index file.")
(defconst orgn--prop-index-for-story-name-en-US "Prop Index for <<story name>>" "Part of the preamble for the prop index file.")
(defconst orgn--chapter-index-for-story-name-en-US "Chapter Index for <<story name>>" "Part of the preamble for the chapter index file.")
(defconst orgn--linked-stories-index-for-story-name-en-US "Linked Stories Index for <<story name>>" "Part of the preamble for the linked stories index file.")
(defconst orgn--front-matter-heading-en-US "Front Matter" "Name for the Front Matter of the book chapters, used as a heading.")
(defconst orgn--main-matter-heading-en-US "Main Matter" "Name for the Main Matter of the book chapters, used as a heading.")
(defconst orgn--back-matter-heading-en-US "Back Matter" "Name for the Back Matter of the book chapters, used as a heading.")
(defconst orgn--notes-en-US "Notes" "Part of the preamble for a chapter file.")
(defconst orgn--chapter-en-US "chapter" "Part of the preamble for a chapter file.")
(defconst orgn--character-en-US "character" "Part of the preamble for a character file.")
(defconst orgn--prop-en-US "prop" "Part of the preamble for a prop file.")
(defconst orgn--place-en-US "place" "Part of the preamble for a place file.")
(defconst orgn--character-name-en-US "character name" "Placeholder for the name of a character, used in generating template preambles.")
(defconst orgn--prop-name-en-US "prop name" "Placeholder for the name of a prop, used in generating template preambles.")
(defconst orgn--place-name-en-US "place name" "Placeholder for the name of a place, used in generating template preambles.")
;; <<Notes>> (without the << >> brackets) must share the same value as org-novelist--notes-en-US.
;; <<chapter>> (without the << >> brackets) must share the same value as org-novelist--chapter-en-US.
;; <<story name>> (without the << >> brackets) must share the same value as org-novelist--story-name-en-US.
;; <<chapter name>> (without the << >> brackets) must share the same value as org-novelist--chapter-name-en-US.
;; <<character name>> (without the << >> brackets) must share the same value as org-novelist--character-name-en-US.
;; <<character>> (without the << >> brackets) must share the same value as org-novelist--character-en-US.
;; <<prop name>> (without the << >> brackets) must share the same value as org-novelist--prop-name-en-US.
;; <<prop>> (without the << >> brackets) must share the same value as org-novelist--prop-en-US.
;; <<place name>> (without the << >> brackets) must share the same value as org-novelist--place-name-en-US.
;; <<place>> (without the << >> brackets) must share the same value as org-novelist--place-en-US.
(defconst orgn--notes-are-available-for-this-chapter-from-story-name-en-US "<<Notes>> are available for this <<chapter>> from <<story name>>." "Sentence describing chapter notes availability.")
(defconst orgn--notes-for-chapter-name-a-chapter-from-story-name-en-US "Notes for <<chapter name>>, a <<chapter>> from <<story name>>." "Sentence header for chapter notes template.")
(defconst orgn--notes-for-character-name-a-character-from-story-name-en-US "Notes for <<character name>>, a <<character>> from <<story name>>." "Sentence header for character notes template.")
(defconst orgn--notes-for-prop-name-a-prop-from-story-name-en-US "Notes for <<prop name>>, a <<prop>> from <<story name>>." "Sentence header for prop notes template.")
(defconst orgn--notes-for-place-name-a-place-from-story-name-en-US "Notes for <<place name>>, a <<place>> from <<story name>>." "Sentence header for place notes template.")
(defconst orgn--content-header-en-US "Content" "Part of the preamble for a chapter file.")
(defconst orgn--scene-name-here-en-US "Scene Name Here" "Part of the preamble for a chapter file.")
(defconst orgn--glossary-header-en-US "Glossary" "Part of the writing glossary in chapter files.")
(defconst orgn--view-notes-en-US "View Notes" "A link text to let the user view related notes.")
(defconst orgn--new-name-en-US "New name" "Placeholder for the new name in an alias string.")
(defconst orgn--old-name-en-US "old name" "Placeholder for the old name in an alias string.")
;; <<New name>> (without the << >> brackets) must share the same value as org-novelist--new-name-en-US.
;; <<old name>> (without the << >> brackets) must share the same value as org-novelist--old-name-en-US.
(defconst orgn--new-name-is-an-alias-for-old-name-en-US "<<New name>> is an alias for <<old name>>." "Text to let the user know something is an alias.")
(defconst orgn--appearances-in-chapters-header-en-US "Appearances in Chapters" "Part of the references section in notes files.")
(defconst orgn--line-en-US "Line" "The word for the line of a chapter. Used at the start of a sentence.")
(defconst orgn--not-yet-referenced-en-US "Not yet referenced in story." "Display that an object has not yet been mentioned in any of the chapter files.")
(defconst orgn--exports-header-en-US "Exports" "Heading for configuration file to use to list export templates.")
;; File Content
(defconst orgn--chapter-notes-content-en-US
  (concat
   "Show how this chapter contributes to:\n"
   "** Character Development\n"
   "** Moving the Plot Forward\n"
   "** Enriching the Setting\n")
  "Starter content for the chapter notes files.")
(defconst orgn--character-notes-content-en-US
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
(defconst orgn--prop-notes-content-en-US
  (concat
   "** Role in Story\n"
   "** Description\n"
   "** Background\n"
   "** Notes\n")
  "Starter content for the prop notes files.")
(defconst orgn--place-notes-content-en-US
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
(defconst orgn--alias-en-US "Alias" "Alias section announcement for glossaries.")
(defconst orgn--glossary-default-character-desc-en-US "A character in the story." "The default description in the index for a character in the story.")
(defconst orgn--glossary-default-place-desc-en-US "A place in the story." "The default description in the index for a place in the story.")
(defconst orgn--glossary-default-prop-desc-en-US "A prop in the story." "The default description in the index for a prop in the story.")
;; User Queries
(defconst orgn--story-name-query-en-US "Story Name?" "A query to the user for what to name their story.")
(defconst orgn--story-save-location-query-en-US "Story Save Location?" "A query to the user for where to save their story.")
(defconst orgn--chapter-name-query-en-US "Chapter Name?" "A query to the user for the name of a chapter.")
(defconst orgn--chapter-location-query-en-US "Choose Chapter Location From Available Options for \"%s\" (%s/%s/%s):" "A query to the user for what section in which to place a new chapter.")
(defconst orgn--rebuild-chapter-index-location-query-en-US "Rebuilding index: Where should chapters go?" "When rebuilding chapter index, ask user where to place chapters.")
(defconst orgn--file-by-file-en-US "Select individually for each file" "Offer to the user to make selections on a file by file basis.")
(defconst orgn--delete-file-query-en-US "Delete file?" "A query to show the user to see if they want to delete a file.")
(defconst orgn--name-already-in-use-en-US "That name is already in use. Please try again" "Tell user the chosen name is already in use.")
(defconst orgn--okay-en-US "Okay" "Positive acknowledgement to the user.")  ; This is also used to check that a language pack exists
(defconst orgn--new-chapter-name-query-en-US "New Chapter Name?" "A query to the user for the new name of a chapter.")
(defconst orgn--character-name-query-en-US "Character Name?" "A query to the user for what to name a character.")
(defconst orgn--prop-name-query-en-US "Prop Name?" "A query to the user for what to name a prop.")
(defconst orgn--place-name-query-en-US "Place Name?" "A query to the user for what to name a place.")
(defconst orgn--new-character-name-query-en-US "New Character Name?" "A query to the user for the new name of a character.")
(defconst orgn--new-prop-name-query-en-US "New Prop Name?" "A query to the user for the new name of a prop.")
(defconst orgn--new-place-name-query-en-US "New Place Name?" "A query to the user for the new name of a place.")
(defconst orgn--new-story-name-query-en-US "New Story Name?" "A query to the user for the new name for the story.")
(defconst orgn--rename-story-folder-query-en-US "Rename story folder as well?" "A query to the user whether to also rename the story folder.")
(defconst orgn--match-lang-tag-to-story-query-en-US "What language was used to create this story (eg, 'en-US')?" "A query to the user to change the session language tag.")
(defconst orgn--story-folder-to-link-to-query-en-US "Story folder to link to current story?" "A query to the user for the story folder where a story to be linked is located.")
(defconst orgn--unlink-from-which-story-query-en-US "Unlink from which story?" "A query to the user for which story to unlink from the current story.")
;; Error/Throw/Messages
(defconst orgn--function-name-en-US "function name" "Placeholder for the name of the function, used in generating error messages.")
(defconst orgn--filename-en-US "filename" "Placeholder for the filename, used in generating error messages.")
;; <<function name>> (without the << >> brackets) must share the same value as org-novelist--function-name-en-US.
(defconst orgn--no-localised-function-en-US "No localized function found for <<function name>>" "The local language version of the function is missing.")
;; <<filename>> (without the << >> brackets) must share the same value as org-novelist--filename-en-US.
(defconst orgn--filename-is-not-writable-en-US "<<filename>> is not writable" "File is not writable.")
(defconst orgn--story-folder-already-in-use-en-US "That story folder is already in use" "Tell user the selected folder already contains an Org Novelist story.")
;; <<filename>> (without the << >> brackets) must share the same value as org-novelist--filename-en-US.
(defconst orgn--filename-is-not-part-of-a-story-folder-en-US "<<filename>> is not part of an Org Novelist story folder" "Function run from location not appearing to be part of an Org Novelist story.")
(defconst orgn--no-story-found-en-US "No story found" "No story found in folder.")
;; <<filename>> (without the << >> brackets) must share the same value as org-novelist--filename-en-US.
(defconst orgn--filename-is-not-readable-en-US "<<filename>> is not readable" "File is not readable.")
(defconst orgn--new-chapter-created-en-US "New chapter created" "Throw out of chapter creation loop once chapter created. Not an error.")
(defconst orgn--no-more-headings-en-US "No more headings" "Throw out of chapter creation loop as section heading not found. Not an error.")
(defconst orgn--file-malformed-en-US "File malformed" "Throw out of chapter creation function as no top heading. Recoverable error.")
(defconst orgn--file-not-found-en-US "File not found" "The requested file could not be found.")
(defconst orgn--no-chapters-found-en-US "No chapters found" "No chapters found in story.")
(defconst orgn--unsaved-buffer-en-US "Unsaved buffer" "Description of a buffer that is not saved to disk.")
(defconst orgn--no-characters-found-en-US "No characters found" "No characters found in story.")
(defconst orgn--no-props-found-en-US "No props found" "No props found in story.")
(defconst orgn--no-places-found-en-US "No places found" "No places found in story.")
;; <<filename>> (without the << >> brackets) must share the same value as org-novelist--filename-en-US.
(defconst orgn--filename-is-not-a-recognised-index-en-US "<<filename>> is not a recognized index" "Index is not of a known type.")
(defconst orgn--auto-ref-now-on-en-US "Org Novelist automatic referencing has been turned ON" "Inform user that automatic referencing has been turned on.")
(defconst orgn--auto-ref-now-off-en-US "Org Novelist automatic referencing has been turned OFF" "Inform user that automatic referencing has been turned off.")
(defconst orgn--language-tag-en-US "language tag" "Placeholder for the language code, used in generating error messages.")
;; <<language tag>> (without the << >> brackets) must share the same value as org-novelist--language-tag-en-US.
(defconst orgn--language-set-to-language-tag-en-US "Org Novelist language set to: <<language tag>>" "Inform user that language has been set.")
(defconst orgn--language-not-found-en-US "Selected language pack not found." "Inform user that language pack could not be found.")
(defconst orgn--folder-already-exists-en-US "That folder already exists" "Inform user the folder already exists.")
(defconst orgn--no-linked-stories-en-US "Currently not linked to any stories" "Inform user there are currently no linked stories.")
;; Pattern Matches
(defconst orgn--sys-safe-name-en-US "[-A-Za-z0-9]*" "Regexp to match strings produced by `org-novelist--system-safe-name-en-US'.")
(defconst orgn--aliases-separators-en-US "[,\f\t\n\r\v]+" "Regexp to match the separators in a list of aliases.")
(defconst orgn--generate-separators-en-US orgn--aliases-separators-en-US "Regexp to match the separators in a list of generators.")
(defconst orgn--notes-name-search-en-US "[[:space:][:punct:]]+?%s[[:space:][:punct:]]+?" "Regexp to match names of things in chapter files.")
(defconst orgn--notes-name-org-link-search-en-US "\\[\\[:space:\\]\\[:punct:\\]\\]+?%s\\[\\[:space:\\]\\[:punct:\\]\\]+?" "Regexp to match, from an Org mode link, names of things in chapter files.")

;;;; Internationalized Functions

(defun orgn--system-safe-name-en-US (str)
  "Convert STR to a directory safe name.
The resultant string should be suitable for all computer systems using en-US."
  ;; I'm just converting things to CamelCase at the moment, and removing non-Latin alphabet characters.
  ;; I've tried to replace special characters with simpler transliterated equivalents that the camelise function can work with ([-A-Za-z0-9]*).
  ;; Since American English regularly loans words from French, German, Spanish, etc, I've tried to do my best to resolve a sensible list of equivalencies.
  ;; Make sure that the language pack constant `org-novelist--sys-safe-name-en-US' matches the output of this function.
  (let ((special-chars (make-hash-table :test 'equal))
        (case-fold-search nil))
    (puthash "£" "GBP" special-chars)
    (puthash "€" "EUR" special-chars)
    (puthash "$" "USD" special-chars)
    (puthash "Ð" "D" special-chars)  ; DH might be better
    (puthash "ð" "d" special-chars)  ; dh might be better
    (puthash "₫" "dd" special-chars)
    (puthash "Þ" "Th" special-chars)
    (puthash "þ" "th" special-chars)
    (puthash "Õ" "O" special-chars)
    (puthash "õ" "o" special-chars)
    (puthash "Ã" "A" special-chars)
    (puthash "ã" "a" special-chars)
    (puthash "Ø" "Oe" special-chars)
    (puthash "ø" "oe" special-chars)
    (puthash "Ù" "U" special-chars)
    (puthash "ù" "u" special-chars)
    (puthash "Ò" "O" special-chars)
    (puthash "ò" "o" special-chars)
    (puthash "Ì" "I" special-chars)
    (puthash "ì" "i" special-chars)
    (puthash "Å" "AA" special-chars)
    (puthash "å" "aa" special-chars)
    (puthash "Á" "A" special-chars)
    (puthash "á" "a" special-chars)
    (puthash "Í" "I" special-chars)
    (puthash "í" "i" special-chars)
    (puthash "Ó" "O" special-chars)
    (puthash "ó" "o" special-chars)
    (puthash "Ú" "U" special-chars)
    (puthash "ú" "u" special-chars)
    (puthash "ý" "y" special-chars)
    (puthash "Ñ" "N" special-chars)
    (puthash "ñ" "n" special-chars)
    (puthash "Ÿ" "Y" special-chars)
    (puthash "ÿ" "y" special-chars)
    (puthash "Ù" "U" special-chars)
    (puthash "ù" "u" special-chars)
    (puthash "Û" "U" special-chars)
    (puthash "û" "u" special-chars)
    ;; (puthash "Ü" "u" special-chars)  ; Not used here. I'm opting to give German priority as English is more closely related to Germanic than romance languages
    ;; (puthash "ü" "u" special-chars)  ; Not used here. I'm opting to give German priority as English is more closely related to Germanic than romance languages
    (puthash "Ô" "O" special-chars)
    (puthash "ô" "o" special-chars)
    (puthash "Œ" "OE" special-chars)
    (puthash "œ" "oe" special-chars)
    (puthash "Î" "I" special-chars)
    (puthash "î" "i" special-chars)  ; For Italian, it might be better to replace this with a double i, though in other languages a single i seems fair
    (puthash "Ï" "I" special-chars)
    (puthash "ï" "i" special-chars)
    (puthash "É" "E" special-chars)
    (puthash "é" "e" special-chars)
    (puthash "È" "E" special-chars)
    (puthash "è" "e" special-chars)
    (puthash "Ê" "E" special-chars)
    (puthash "ê" "e" special-chars)
    (puthash "Ë" "E" special-chars)
    (puthash "ë" "e" special-chars)
    (puthash "Ç" "C" special-chars)
    (puthash "ç" "c" special-chars)
    (puthash "À" "A" special-chars)
    (puthash "à" "a" special-chars)
    (puthash "Â" "A" special-chars)
    (puthash "â" "a" special-chars)
    (puthash "Æ" "AE" special-chars)
    (puthash "æ" "ae" special-chars)
    (puthash "Ä" "Ae" special-chars)
    (puthash "Ö" "Oe" special-chars)
    (puthash "Ü" "Ue" special-chars)
    (puthash "ä" "ae" special-chars)
    (puthash "ö" "oe" special-chars)
    (puthash "ü" "ue" special-chars)
    (puthash "ẞ" "SS" special-chars)
    (puthash "ß" "ss" special-chars)
    (puthash "ſ" "s" special-chars)
    (puthash "ʒ" "z" special-chars)
    (setq str (orgn--replace-chars special-chars str))
    (orgn--camelise str)))

(defun orgn--camelise-en-US (str)
  "Convert STR to CamelCase, using only the Latin alphabet.
The resultant string should be suitable for all computer systems using en-US."
  (let* ((white-list (list "A" "a" "B" "b" "C" "c" "D" "d" "E" "e" "F" "f" "G" "g" "H" "h" "I" "i" "J" "j" "K" "k" "L" "l" "M" "m" "N" "n" "O" "o" "P" "p" "Q" "q" "R" "r" "S" "s" "T" "t" "U" "u" "V" "v" "W" "w" "X" "x" "Y" "y" "Z" "z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" " "))  ; List of allowed characters in folder names, plus the space character. The space will also be removed later on, but we need it in the white list as it will be used as a word separator.
         (taboo-chars (mapcar 'string (string-to-list (orgn--remove-chars white-list str)))))  ; Add characters to this list that are not in the white list
    (mapconcat 'identity (mapcar
                          (lambda (word) (capitalize (downcase word)))
                          (split-string (orgn--remove-chars taboo-chars str) " ")) nil)))
;;;; American English (en-US) ends here

(provide 'org-novelist-language-pack-en-us)
;;; org-novelist-language-pack-en-us.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("orgn-" . "org-novelist-"))
;; End:
