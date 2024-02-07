;;; org-novelist-language-pack-de-de.el --- Org Novelist language pack for de-DE (German as used in Germany) -*- lexical-binding: t; -*-

;; Language pack for Org Novelist.
;; Copyright (c) 2024 John Urquhart Ferguson & Roxana Schumann
;;
;; Author: John Urquhart Ferguson <mail@johnurquhartferguson.info> & Roxana Schumann
;; Maintainer: John Urquhart Ferguson <mail@johnurquhartferguson.info> & Roxana Schumann
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
;; supplies a language pack for German, as spoken in Germany, to be
;; used with Org Novelist.
;;
;; Creating, linking, and laying out files in the Org Novelist
;; methodology can be done without the use of Emacs or the Org Novelist
;; package, but using the package within Emacs will provide helper
;; functions that make the methodology much easier to use; allowing the
;; following of links, programmatic updating of crossreferences, and
;; the ability to programatically export to other formats.
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


;;;; German as used in Germany (de-DE)
;; User Variable Fallback Strings
(defconst orgn--author-not-set-de-DE "Der Autor wurde nicht angegeben" "Author not specified by user.")
(defconst orgn--author-email-not-set-de-DE "Die E-Mail Adresse des Autors wurde nicht angegeben" "Author email not specified by user.")
;; File Instructions
(defconst orgn--main-file-instructions-de-DE "Schreibe hier eine kurze Zusammenfassung der Geschichte" "Instructions for the main entry-point file.")
(defconst orgn--notes-file-instructions-de-DE "Hier ist Platz für Notizen" "Instructions for the general notes file.")
(defconst orgn--research-file-instructions-de-DE "Hier ist Platz für deine Recherche" "Instructions for the general research file.")
(defconst orgn--characters-file-instructions-de-DE "Dies ist ein Index der Charaktere dieser Geschichte. Bearbeite ihn nicht manuell! Benutze nur Org Mode oder Org Novelist Funktionen." "Instructions for the character index file.")
(defconst orgn--places-file-instructions-de-DE "Dies ist ein Index der Orte dieser Geschichte. Bearbeite ihn nicht manuell! Benutze nur Org Mode oder Org Novelist Funktionen." "Instructions for the location index file.")
(defconst orgn--props-file-instructions-de-DE "Dies ist ein Index aller Bestandteile der Geschichte. Bearbeite ihn nicht manuell! Benutze nur Org Mode oder Org Novelist Funktionen." "Instructions for the prop index file.")
(defconst orgn--chapters-file-instructions-de-DE "Dies ist das Inhaltsverzeichnis aller Kapitel der Geschichte. Bearbeite es nicht manuell! Benutze nur Org Mode oder Org Novelist Funktionen." "Instructions for the chapter index file.")
;; Folder Names
(defconst orgn--notes-folder-de-DE "Notizen" "The folder name for storing note files.")
(defconst orgn--indices-folder-de-DE "Indizes" "The folder name for storing index files.")
(defconst orgn--chapters-folder-de-DE "Kapitel" "The folder name for storing the chapter files.")
(defconst orgn--exports-folder-de-DE "Exporte" "The folder name for storing export files.")
;; File Names
(defconst orgn--main-file-de-DE "Hauptdokument" "Name for the story's main entry-point file.")
(defconst orgn--notes-file-de-DE "Notizen" "Name for the story's general notes file.")
(defconst orgn--research-file-de-DE "Recherche" "Name for the story's general research file.")
(defconst orgn--characters-file-de-DE "Charaktere" "Name for the story's character index file.")
(defconst orgn--places-file-de-DE "Orte" "Name for the story's location index file.")
(defconst orgn--props-file-de-DE "Bestandteile" "Name for the story's prop index file.")
(defconst orgn--chapters-file-de-DE "Kapitel" "Name for the story's chapter index file.")
(defconst orgn--chapter-file-prefix-de-DE "Kapitel-" "Prefix for the story's chapter files.")
(defconst orgn--notes-suffix-de-DE "-notizen" "Suffix for a file's associated notes file.")
(defconst orgn--character-file-prefix-de-DE "Charakter-" "Prefix for the story's character files.")
(defconst orgn--prop-file-prefix-de-DE "Bestandteil-" "Prefix for the story's prop files.")
(defconst orgn--place-file-prefix-de-DE "Ort-" "Prefix for the story's place files.")
;; File Titles
(defconst orgn--notes-title-de-DE "Notizen" "Name for the story's general notes title.")
(defconst orgn--research-title-de-DE "Recherche" "Name for the story's general research title.")
(defconst orgn--characters-title-de-DE "Charaktere" "Name for the story's character index title.")
(defconst orgn--places-title-de-DE "Orte" "Name for the story's location index title.")
(defconst orgn--props-title-de-DE "Bestandteile" "Name for the story's prop index title.")
(defconst orgn--chapters-title-de-DE "Kapitel" "Name for the story's chapter index title.")
(defconst orgn--config-name-de-DE "Export-Einstellungen" "Display name for a link to the story's configuration file.")
;; File Preambles
(defconst orgn--story-name-de-DE "Name der Geschichte" "Placeholder for the name of the story, used in generating template preambles.")
(defconst orgn--chapter-name-de-DE "Kapitelname" "Placeholder for the name of a chapter, used in generating template preambles.")
;; <<Name der Geschichte>> (without the << >> brackets) must share the same value as org-novelist--story-name-de-DE.
;; <<Kapitelname>> (without the << >> brackets) must share the same value as org-novelist--chapter-name-de-DE.
(defconst orgn--notes-for-story-name-de-DE "Notizen für <<Name der Geschichte>>" "Part of the preamble for the general notes file.")
(defconst orgn--notes-for-chapter-name-de-DE "Notizen für <<Kapitelname>>" "Part of the preamble for the chapter notes file.")
(defconst orgn--research-for-story-name-de-DE "Recherche für <<Name der Geschichte>>" "Part of the preamble for the general research file.")
(defconst orgn--character-index-for-story-name-de-DE "Charakterindex für <<Name der Geschichte>>" "Part of the preamble for the character index file.")
(defconst orgn--place-index-for-story-name-de-DE "Ortsindex für <<Name der Geschichte>>" "Part of the preamble for the location index file.")
(defconst orgn--prop-index-for-story-name-de-DE "Index der Bestandteile für <<Name der Geschichte>>" "Part of the preamble for the prop index file.")
(defconst orgn--chapter-index-for-story-name-de-DE "Kapitelindex für <<Name der Geschichte>>" "Part of the preamble for the chapter index file.")
(defconst orgn--front-matter-heading-de-DE "Eröffnung" "Name for the Front Matter of the book chapters, used as a heading.")
(defconst orgn--main-matter-heading-de-DE "Haupttext" "Name for the Main Matter of the book chapters, used as a heading.")
(defconst orgn--back-matter-heading-de-DE "Nachtrag" "Name for the Back Matter of the book chapters, used as a heading.")
(defconst orgn--notes-de-DE "Notizen" "Part of the preamble for a chapter file.")
(defconst orgn--chapter-de-DE "Kapitel" "Part of the preamble for a chapter file.")
(defconst orgn--character-de-DE "Charakter" "Part of the preamble for a character file.")
(defconst orgn--prop-de-DE "Bestandteil" "Part of the preamble for a prop file.")
(defconst orgn--place-de-DE "Ort" "Part of the preamble for a place file.")
(defconst orgn--character-name-de-DE "Charaktername" "Placeholder for the name of a character, used in generating template preambles.")
(defconst orgn--prop-name-de-DE "Name des Bestandteils" "Placeholder for the name of a prop, used in generating template preambles.")
(defconst orgn--place-name-de-DE "Ortsname" "Placeholder for the name of a place, used in generating template preambles.")
;; <<Notizen>> (without the << >> brackets) must share the same value as org-novelist--notes-de-DE.
;; <<Kapitel>> (without the << >> brackets) must share the same value as org-novelist--chapter-de-DE.
;; <<Name der Geschichte>> (without the << >> brackets) must share the same value as org-novelist--story-name-de-DE.
;; <<Kapitelname>> (without the << >> brackets) must share the same value as org-novelist--chapter-name-de.DE.
;; <<Charaktername>> (without the << >> brackets) must share the same value as org-novelist--character-name-de-DE.
;; <<Charakter>> (without the << >> brackets) must share the same value as org-novelist--character-de-DE.
;; <<Name des Bestandteils>> (without the << >> brackets) must share the same value as org-novelist--prop-name-de-DE.
;; <<Bestandteil>> (without the << >> brackets) must share the same value as org-novelist--prop-de-DE.
;; <<Ortsname>> (without the << >> brackets) must share the same value as org-novelist--place-name-de-DE.
;; <<Ort>> (without the << >> brackets) must share the same value as org-novelist--place-de-DE.
(defconst orgn--notes-are-available-for-this-chapter-from-story-name-de-DE "<<Notizen>> für dieses <<Kapitel>> aus <<Name der Geschichte>> sind verfügbar." "Sentence describing chapter notes availability.")
(defconst orgn--notes-for-chapter-name-a-chapter-from-story-name-de-DE "Notizen für <<Kapitelname>>, ein <<Kapitel>> aus <<Name der Geschichte>>." "Sentence header for chapter notes template.")
(defconst orgn--notes-for-character-name-a-character-from-story-name-de-DE "Notizen für <<Charaktername>>, ein <<Charakter>> aus <<Name der Geschichte>>." "Sentence header for character notes template.")
(defconst orgn--notes-for-prop-name-a-prop-from-story-name-de-DE "Notizen für <<Name des Bestandteils>>, ein <<Bestandteil>> aus <<Name der Geschichte>>." "Sentence header for prop notes template.")
(defconst orgn--notes-for-place-name-a-place-from-story-name-de-DE "Notizen für <<Ortsname>>, ein <<Ort>> aus <<Name der Geschichte>>." "Sentence header for place notes template.")
(defconst orgn--content-header-de-DE "Inhalt" "Part of the preamble for a chapter file.")
(defconst orgn--scene-name-here-de-DE "Kurzbeschreibung der Szene" "Part of the preamble for a chapter file.")
(defconst orgn--glossary-header-de-DE "Glossar" "Part of the writing glossary in chapter files.")
(defconst orgn--view-notes-de-DE "Notizen ansehen" "A link text to let the user view related notes.")
(defconst orgn--new-name-de-DE "Neuer Name" "Placeholder for the new name in an alias string.")
(defconst orgn--old-name-de-DE "alter Name" "Placeholder for the old name in an alias string.")
;; <<Neuer Name>> (without the << >> brackets) must share the same value as org-novelist--new-name-de-DE.
;; <<alter Name>> (without the << >> brackets) must share the same value as org-novelist--old-name-de-DE.
(defconst orgn--new-name-is-an-alias-for-old-name-de-DE "<<Neuer Name>> ist ein Alias für <<alter Name>>." "Text to let the user know something is an alias.")
(defconst orgn--appearances-in-chapters-header-de-DE "Auftritt in Kapitel" "Part of the references section in notes files.")
(defconst orgn--line-de-DE "Zeile" "The word for the line of a chapter. Used at the start of a sentence.")
(defconst orgn--not-yet-referenced-de-DE "Noch nicht in der Geschichte genannt" "Display that an object has not yet been mentioned in any of the chapter files.")
(defconst orgn--exports-header-de-DE "Exporteinstellungen" "Heading for configuration file to use to list export templates.")
;; File Content
(defconst orgn--chapter-notes-content-de-DE
  (concat
   "Erläutere den Beitrag dieses Kapitels zu:\n"
   "** Charakterentwicklung\n"
   "** Voranbringen der Handlung\n"
   "** Bereicherung des Schauplatzes\n")
  "Starter content for the chapter notes files.")
(defconst orgn--character-notes-content-de-DE
  (concat
   "** Rolle in der Geschichte\n"
   "** Was will der Charakter?\n"
   "** Was würde den Charakter am meisten zur Handlung motivieren?\n"
   "** Was würde den Charakter am ehesten davon abhalten zu handeln?\n"
   "** Was ist das Schlimmste, das diesem Charakter passieren könnte?\n"
   "** Was ist das Beste, das diesem Charakter passieren könnte?\n"
   "** Wer oder was verhindert, dass der Charakter bekommt, was er/sie will?\n"
   "** Was muss der Charakter lernen, um glücklich zu sein?\n"
   "** Beruf/Beschäftigung\n"
   "** Äußerliche Beschreibung\n"
   "** Persönlichkeit\n"
   "** Gewohnheiten/Eigenarten\n"
   "** Hintergrund/Werdegang\n"
   "** Innere Konflikte\n"
   "** Äußere Konflikte\n"
   "** Notizen\n")
  "Starter content for the character notes files.")
(defconst orgn--prop-notes-content-de-DE
  (concat
   "** Rolle in der Geschichte\n"
   "** Beschreibung\n"
   "** Hintergrund/Werdegang\n"
   "** Notizen\n")
  "Starter content for the prop notes files.")
(defconst orgn--place-notes-content-de-DE
  (concat
   "** Rolle in der Geschichte\n"
   "** Beschreibung\n"
   "** Hintergrundgeschichte\n"
   "** Dazugehörige Charaktere\n"
   "** Jahreszeit\n"
   "** Einzigartige Eigenschaften\n"
   "** Visuelle Beschreibung\n"
   "** Klang\n"
   "** Geruch\n"
   "** Notizen\n")
  "Starter content for the place notes files.")
(defconst orgn--alias-de-DE "Pseudonym" "Alias section announcement for glossaries.")
(defconst orgn--glossary-default-character-desc-de-DE "Ein Charakter der Geschichte." "The default description in the index for a character in the story.")
(defconst orgn--glossary-default-place-desc-de-DE "Ein Ort der Geschichte." "The default description in the index for a place in the story.")
(defconst orgn--glossary-default-prop-desc-de-DE "Ein Bestandteil der Geschichte." "The default description in the index for a prop in the story.")
;; User Queries
(defconst orgn--story-name-query-de-DE "Name der Geschichte?" "A query to the user for what to name their story.")
(defconst orgn--story-save-location-query-de-DE "Speicherort?" "A query to the user for where to save their story.")
(defconst orgn--chapter-name-query-de-DE "Kapitelname?" "A query to the user for the name of a chapter.")
(defconst orgn--chapter-location-query-de-DE "Wähle die Position des Kapitels für \"%s\" aus den möglichen Optionen (%s/%s/%s):" "A query to the user for what section in which to place a new chapter.")
(defconst orgn--rebuild-chapter-index-location-query-de-DE "Aktualisierung des Inhaltsverzeichnisses: Wohin sollen die Kapitel verschoben werden?" "When rebuilding chapter index, ask user where to place chapters.")
(defconst orgn--file-by-file-de-DE "Für jede Datei einzeln auswählen" "Offer to the user to make selections on a file by file basis.")
(defconst orgn--delete-file-query-de-DE "Datei löschen?" "A query to show the user to see if they want to delete a file.")
(defconst orgn--name-already-in-use-de-DE "Dieser Name wird bereits genutzt. Bitte versuche es erneut" "Tell user the chosen name is already in use.")
(defconst orgn--okay-de-DE "Okay" "Positive acknowledgement to the user.")  ; This is also used to check that a language pack exists
(defconst orgn--new-chapter-name-query-de-DE "Neuer Kapitelname?" "A query to the user for the new name of a chapter.")
(defconst orgn--character-name-query-de-DE "Charaktername?" "A query to the user for what to name a character.")
(defconst orgn--prop-name-query-de-DE "Name des Bestandteils?" "A query to the user for what to name a prop.")
(defconst orgn--place-name-query-de-DE "Name des Orts?" "A query to the user for what to name a place.")
(defconst orgn--new-character-name-query-de-DE "Neuer Charaktername?" "A query to the user for the new name of a character.")
(defconst orgn--new-prop-name-query-de-DE "Neuer Name des Bestandteils?" "A query to the user for the new name of a prop.")
(defconst orgn--new-place-name-query-de-DE "Neuer Ortsname?" "A query to the user for the new name of a place.")
(defconst orgn--new-story-name-query-de-DE "Neuer Name der Geschichte?" "A query to the user for the new name for the story.")
(defconst orgn--rename-story-folder-query-de-DE "Soll der Ordner der Geschichte ebenfalls umbenannt werden?" "A query to the user whether to also rename the story folder.")
(defconst orgn--match-lang-tag-to-story-query-de-DE "Mit welcher Sprache wurde diese Geschichte geschrieben (z.B. 'de-DE')?" "A query to the user to change the session language tag.")
;; Error/Throw/Messages
(defconst orgn--function-name-de-DE "Name der Funktion" "Placeholder for the name of the function, used in generating error messages.")
(defconst orgn--filename-de-DE "Dateiname" "Placeholder for the filename, used in generating error messages.")
;; <<Name der Funktion>> (without the << >> brackets) must share the same value as org-novelist--function-name-de-DE.
(defconst orgn--no-localised-function-de-DE "Es konnte keine lokalisierte Funktion gefunden werden für <<Name der Funktion>>" "The local language version of the function is missing.")
;; <<Dateiname>> (without the << >> brackets) must share the same value as org-novelist--filename-de-DE.
(defconst orgn--filename-is-not-writable-de-DE "Die Datei <<Dateiname>> ist schreibgeschützt" "File is not writable.")
(defconst orgn--story-folder-already-in-use-de-DE "Dieser Ordner wird bereits genutzt" "Tell user the selected folder already contains an Org Novelist story.")
;; <<Dateiname>> (without the << >> brackets) must share the same value as org-novelist--filename-de-DE.
(defconst orgn--filename-is-not-part-of-a-story-folder-de-DE "Die Datei <<Dateiname>> gehört nicht zu einem Org Novelist Ordner" "Function run from location not appearing to be part of an Org Novelist story.")
(defconst orgn--no-story-found-de-DE "Keine Geschichte gefunden" "No story found in folder.")
;; <<Dateiname>> (without the << >> brackets) must share the same value as org-novelist--filename-de-DE.
(defconst orgn--filename-is-not-readable-de-DE "Die Datei <<Dateiname>> kann nicht geöffnet werden" "File is not readable.")
(defconst orgn--new-chapter-created-de-DE "Neues Kapitel erstellt" "Throw out of chapter creation loop once chapter created. Not an error.")
(defconst orgn--no-more-headings-de-DE "Keine weiteren Überschriften" "Throw out of chapter creation loop as section heading not found. Not an error.")
(defconst orgn--file-malformed-de-DE "Datei ist fehlerhaft" "Throw out of chapter creation function as no top heading. Recoverable error.")
(defconst orgn--file-not-found-de-DE "Datei wurde nicht gefunden" "The requested file could not be found.")
(defconst orgn--no-chapters-found-de-DE "Keine Kapitel gefunden" "No chapters found in story.")
(defconst orgn--unsaved-buffer-de-DE "Ungespeicherter Zwischenspeicher" "Description of a buffer that is not saved to disk.")
(defconst orgn--no-characters-found-de-DE "Keine Charaktere gefunden" "No characters found in story.")
(defconst orgn--no-props-found-de-DE "Keine Bestandteile gefunden" "No props found in story.")
(defconst orgn--no-places-found-de-DE "Keine Orte gefunden" "No places found in story.")
;; <<Dateiname>> (without the << >> brackets) must share the same value as org-novelist--filename-de-DE.
(defconst orgn--filename-is-not-a-recognised-index-de-DE "Die Datei <<Dateiname>> ist kein bekannter Index" "Index is not of a known type.")
(defconst orgn--auto-ref-now-on-de-DE "Org Novelist automatische Querverweise sind eingeschaltet" "Inform user that automatic referencing has been turned on.")
(defconst orgn--auto-ref-now-off-de-DE "Org Novelist automatische Querverweise sind ausgeschaltet" "Inform user that automatic referencing has been turned off.")
(defconst orgn--language-tag-de-DE "Sprachkürzel" "Placeholder for the language code, used in generating error messages.")
;; <<Sprachkürzel>> (without the << >> brackets) must share the same value as org-novelist--language-tag-de-DE.
(defconst orgn--language-set-to-language-tag-de-DE "Org Novelist Sprache wurde geändert zu: <<Sprachkürzel>>" "Inform user that language has been set.")
(defconst orgn--language-not-found-de-DE "Ausgewähltes Sprachpaket konnte nicht gefunden werden." "Inform user that language pack could not be found.")
(defconst orgn--folder-already-exists-de-DE "Dieser Ordner existiert bereits" "Inform user the folder already exists.")
;; Pattern Matches
(defconst orgn--sys-safe-name-de-DE "[-A-Za-z0-9]*" "Regexp to match strings produced by `org-novelist--system-safe-name-de-DE'.")
(defconst orgn--aliases-separators-de-DE "[,\f\t\n\r\v]+" "Regexp to match the separators in a list of aliases.")
(defconst orgn--generate-separators-de-DE orgn--aliases-separators-de-DE "Regexp to match the separators in a list of generators.")
(defconst orgn--notes-name-search-de-DE "[[:space:][:punct:]]+?%s[[:space:][:punct:]]+?" "Regexp to match names of things in chapter files.")
(defconst orgn--notes-name-org-link-search-de-DE "\\[\\[:space:\\]\\[:punct:\\]\\]+?%s\\[\\[:space:\\]\\[:punct:\\]\\]+?" "Regexp to match, from an Org mode link, names of things in chapter files.")

;;;; Internationalized Functions

(defun orgn--system-safe-name-de-DE (str)
  "Convert STR to a directory safe name.
The resultant string should be suitable for all computer systems using de-DE."
  ;; I'm just converting things to CamelCase at the moment, and removing non-Latin alphabet characters.
  ;; I've tried to replace special characters with simpler transliterated equivalents that the camlise function can work with ([-A-Za-z0-9]*).
  ;; Since there are many loan words between German, English, French, Spanish, etc, I've tried to do my best to resolve a sensible list of equivalencies.
  ;; Make sure that the language pack constant `org-novelist--sys-safe-name-de-DE' matches the output of this function.
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
    ;; (puthash "Ü" "u" special-chars)  ; Not used here. I'm opting to give German priority as this is a German language pack
    ;; (puthash "ü" "u" special-chars)  ; Not used here. I'm opting to give German priority as this is a German language pack
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

(defun orgn--camelise-de-DE (str)
  "Convert STR to CamelCase, using only the Latin alphabet.
The resultant string should be suitable for all computer systems using de-DE."
  (let* ((white-list (list "A" "a" "B" "b" "C" "c" "D" "d" "E" "e" "F" "f" "G" "g" "H" "h" "I" "i" "J" "j" "K" "k" "L" "l" "M" "m" "N" "n" "O" "o" "P" "p" "Q" "q" "R" "r" "S" "s" "T" "t" "U" "u" "V" "v" "W" "w" "X" "x" "Y" "y" "Z" "z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" " "))  ; List of allowed characters in folder names, plus the space character. The space will also be removed later on, but we need it in the white list as it will be used as a word separator.
         (taboo-chars (mapcar 'string (string-to-list (orgn--remove-chars white-list str)))))  ; Add characters to this list that are not in the white list
    (mapconcat 'identity (mapcar
                          (lambda (word) (capitalize (downcase word)))
                          (split-string (orgn--remove-chars taboo-chars str) " ")) nil)))
;;;; German as used in Germany (de-DE) ends here

(provide 'org-novelist-language-pack-de-de)
;;; org-novelist-language-pack-de-de.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("orgn-" . "org-novelist-"))
;; End:
