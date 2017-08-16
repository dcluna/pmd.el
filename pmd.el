;;; pmd.el --- Poor man's debugger (through print statements)  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Daniel Luna

;; Author: Daniel Luna <dancluna@gmail.com>
;; Keywords: convenience, lisp, tools

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

;; Emacs package to automate insertion of debug print statements for various modes.

;;; Code:

(defvar pmd-input-separator ",")
(defvar pmd-output-separator " | ")

(defvar pmd-print-open nil
  "String that opens a print-var statement.")
(defvar pmd-print-close nil
  "String that closes a print-var statement.")

(defvar pmd-/var-formatting-function nil
  "Formatting function for a variable inside a print statement. Mostly language-specific.")

(defun pmd//ruby-interpolation-formatting-fn (var)
  "VAR-formatting fn for languages that accept Ruby-ish string interpolation.
Example: \"var = #{var}\"."
  (format "%s = #{%s}" var var))

(defun pmd//parse-input (input)
  "Parses INPUT string into a list of variables."
  (split-string input (concat "\\\\" pmd-input-separator) t "[[:space:]]+"))

(defun pmd//prepare-output (list-vars)
  "Prepares print statement to display LIST-VARS."
  (concat pmd-print-open (mapconcat pmd-/var-formatting-function list-vars pmd-output-separator) pmd-print-close))

(defun pmd//ruby-setup ()
  (setq-local pmd-print-open "puts \"")
  (setq-local pmd-print-close "\"")
  (setq-local pmd-/var-formatting-function #'pmd//ruby-interpolation-formatting-fn))

(provide 'pmd)
;;; pmd.el ends here
