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

;; Emacs package to automate insertion of debug print statements for various major modes.

;;; Code:
(require 'cl)

(when (not (fboundp 'alist-get))
  (defun alist-get (key alist &optional default remove)
    "Return the value associated with KEY in ALIST, using `assq'.
If KEY is not found in ALIST, return DEFAULT.

This is a generalized variable suitable for use with `setf'.
When using it to set a value, optional argument REMOVE non-nil
means to remove KEY from ALIST if the new value is `eql' to DEFAULT."
    (ignore remove) ;;Silence byte-compiler.
    (let ((x (assq key alist)))
      (if x (cdr x) default))))

(defgroup pmd nil
  "Poor man's debugger"
  :prefix "pmd-"
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/dcluna/pmd.el"))

(defcustom pmd-input-separator ","
  "Token sequence that separates different variables."
  :group 'pmd
  :tag "pmd Input Separator"
  :type 'string)

(defcustom pmd-modifier-separator "/"
  "Separator between modifiers and input."
  :group 'pmd
  :tag "pmd Modifier Separator"
  :type 'string)

(defcustom pmd-output-separator " | "
  "Token sequence separating variables in the output statement."
  :group 'pmd
  :tag "pmd Output Separator"
  :type 'string)

(defcustom pmd-require-escape-input-separator nil
  "If set, requires `pmd-input-separator' to be escaped with a `\\'. Can be overriden with the `re' modifiers."
  :group 'pmd
  :tag "pmd Escape Input Separator"
  :type 'boolean)

(defvar pmd-print-open nil
  "String that opens a print-var statement.")

(defvar pmd-print-close nil
  "String that closes a print-var statement.")
(defvar pmd-show-file-name nil
  "If set, prints `file-name-base' before the variable contents")

(defvar pmd-/multi-var-format-fn nil
  "Formatting function for a list of variables.")

(defun pmd//extract-modifiers (input)
  "Splits modifiers and input vars from INPUT."
  (let ((input-list (split-string input pmd-modifier-separator)))
    (if (<= (length input-list) 1)
        (append input-list '(nil))
      (list (mapconcat 'identity (cdr input-list) pmd-modifier-separator) (split-string (car input-list) ";" t "[[:space:]]+")))))

(defconst pmd-modifier-name-alist '((re . pmd--m-require-escape)
                                    (re! . (pmd--m-require-escape false))
                                    (fl . pmd--m-show-file-name)
                                    (fl! . (pmd--m-show-file-name false))
                                    (el . pmd--m-eval-input-as-lisp)
                                    (sh . pmd--m-eval-input-as-shell)
                                    (rb . pmd--m-eval-input-as-ruby)
                                    (pl . pmd--m-eval-input-as-perl)))

(defun pmd//ruby-perl-eval-print (interpreter program)
  (shell-command-to-string (concat interpreter " -e \"print " program "\"")))

(defun pmd--m-eval-input-as-lisp (program)
  (list 'let '((input (eval (read input))))
        program))

(defun pmd--m-eval-input-as-shell (program)
  (list 'let '((input (shell-command-to-string input)))
        program))

(defun pmd--m-eval-input-as-ruby (program)
  (list 'let '((input (pmd//ruby-perl-eval-print "ruby" input)))
        program))

(defun pmd--m-eval-input-as-perl (program)
  (list 'let '((input (pmd//ruby-perl-eval-print "perl" input)))
        program))

(defun pmd--m-require-escape (program &optional value)
  (list 'let (list (list 'pmd-require-escape-input-separator (if (equal value 'false) nil t)))
     program))

(defun pmd--m-show-file-name (program &optional value)
  (list 'let (list (list 'pmd-show-file-name (if (equal value 'false) nil t)))
        program))

(defun pmd//apply-mod (modifier program)
  "Translates MODIFIER into an internal modifier function and generates a new PROGRAM based on the original."
  (let* ((mod-fn-and-possible-args (alist-get (intern modifier) pmd-modifier-name-alist))
         (list-fn (listp mod-fn-and-possible-args))
         (mod-fn (if list-fn (car mod-fn-and-possible-args) mod-fn-and-possible-args))
         (args (if list-fn (cdr mod-fn-and-possible-args) nil)))
    (apply mod-fn program args)))

(defun pmd//process-modifiers (modifiers program)
  "Generates a new PROGRAM from translating MODIFIERS in sequence."
  (reduce (lambda (new-program modifier)
            (pmd//apply-mod modifier new-program))
          (reverse modifiers)
          :initial-value program))

(defun pmd//split-input (input)
  "Splits INPUT into a list of variables."
  (split-string
   input
   (concat (if pmd-require-escape-input-separator "\\\\" "") pmd-input-separator)
   t
   "[[:space:]]+"))

(defun pmd//file-name-prefix ()
  (if pmd-show-file-name (concat (file-name-base) ": ") nil))

(defun pmd//run-program-in-input-context (input &optional program)
  "Parses INPUT string into a list of variables."
  (let ((program (or program '(pmd//prepare-output (pmd//split-input input)))))
    (destructuring-bind (var-input modifiers) (pmd//extract-modifiers input)
      (let ((generated-program (pmd//process-modifiers modifiers program)))
        (eval `(let ((input ,var-input)) ,generated-program))))))

(defun pmd//apply-formatting-fn (list-vars)
  (funcall pmd-/multi-var-format-fn list-vars))

(defun pmd//prepare-output (list-vars)
  "Prepares print statement to display LIST-VARS."
  (pmd//apply-formatting-fn list-vars))

(defun pmd//escape-str (to-escape in-str)
  (replace-regexp-in-string to-escape (concat "\\" to-escape) in-str nil t))

(defun pmd//ruby-interpolation-formatting-fn (list-vars)
  "VAR-formatting fn for languages that accept Ruby-ish string interpolation.
Example: \"var = #{var}\"."
  (concat
   pmd-print-open
   (pmd//file-name-prefix)
   (mapconcat (lambda (var) (format "%s = #{%s}" var var)) list-vars pmd-output-separator)
   pmd-print-close))

(defun pmd//js-interpolation-formatting-fn (list-vars)
  "VAR-formatting fn for Javascript.
Example: \"var = \" + var."
  (concat
   pmd-print-open
   "\""
   (concat (pmd//file-name-prefix) "\" + ")
   (mapconcat (lambda (var) (format "\"%s = \" + %s" (pmd//escape-str "\"" var) var)) list-vars (concat " + \" " pmd-output-separator " \" + "))
   pmd-print-close))

(defun pmd//rust-println-exp (list-vars)
  (concat
   pmd-print-open
   "\""
   (pmd//file-name-prefix)
   (mapconcat (lambda (var) (concat var " = {:?}")) list-vars pmd-output-separator)
   "\", "
   (mapconcat 'identity list-vars ", ")
   pmd-print-close))

(defun pmd//js2-setup ()
  (setq-local pmd-print-open "console.log(")
  (setq-local pmd-print-close ");")
  (setq-local pmd-show-file-name t)
  (setq-local pmd-/multi-var-format-fn #'pmd//js-interpolation-formatting-fn))

(defun pmd//coffee-setup ()
  (setq-local pmd-print-open "console.log \"")
  (setq-local pmd-print-close "\"")
  (setq-local pmd-/multi-var-format-fn #'pmd//ruby-interpolation-formatting-fn))

(defun pmd//ruby-setup ()
  (setq-local pmd-print-open "puts \"")
  (setq-local pmd-print-close "\"")
  (setq-local pmd-/multi-var-format-fn #'pmd//ruby-interpolation-formatting-fn))

(defun pmd//rust-setup ()
  (setq-local pmd-print-open "println!(")
  (setq-local pmd-print-close ");")
  (setq-local pmd-/multi-var-format-fn #'pmd//rust-println-exp))

(add-hook 'js2-mode-hook 'pmd//js2-setup)
(add-hook 'typescript-mode-hook 'pmd//js2-setup)
(add-hook 'enh-ruby-mode-hook 'pmd//ruby-setup)
(add-hook 'ruby-mode-hook 'pmd//ruby-setup)
(add-hook 'coffee-mode-hook 'pmd//coffee-setup)
(add-hook 'rust-mode-hook 'pmd//rust-setup)

(defun pmd//print-vars-internal (input)
  (when (not (looking-at-p "^[[:space:]]*$"))
    (end-of-line)
    (insert "\n")
    (back-to-indentation))
  (insert (pmd//run-program-in-input-context input))
  (indent-according-to-mode))

(defun pmd/print-vars (input)
  (interactive "sVar-string: ")

  (pmd//print-vars-internal input))

(when (featurep 'evil)
  (evil-define-command pmd/evil-print-vars (input)
    (interactive "<a>")
    (pmd//print-vars-internal input))

  (evil-ex-define-cmd "print-vars" 'pmd/evil-print-vars)
  (evil-ex-define-cmd "pv" 'pmd/evil-print-vars))

(provide 'pmd)
;;; pmd.el ends here
