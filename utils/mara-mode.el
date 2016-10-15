;;
;; mara-mode.el for Mara
;;
;; Made by Pierre Surply
;; <pierre.surply@gmail.com>
;;
;; Started on  Sat Jan 19 16:37:09 2013 Pierre Surply
;; Last update Tue Aug 20 21:15:18 2013 Pierre Surply
;;


(defvar mara-struct-keywords
  '("end"
    "setup"
    "loop"
    "func"
    "method"
    "attr"
    "state"
    "action"
    "input"
    "using"
    "class"
    "lib"
    "interrupt"
    "lambda"
    "fsm"
    "extern")
  "Mara struct-keywords")

(defvar mara-keywords
  '("if"
    "then"
    "else"
    "endif"
    "elif"
    "while"
    "waitfor"
    "for"
    "do"
    "done"
    "to"
    "downto"
    "var"
    "return"
    "bitls"
    "bitrs"
    "bitor"
    "bitand"
    "compl"
    "and"
    "or"
    "not")
  "Mara keywords")

(defvar mara-types
  '("integer"
    "undef"
    "cstring"
    "string"
    "list"
    "vect"
    "void")
  "Mara types")

(defvar mara-constants
  '("self"
    "true"
    "false"
    "none")
  "Mara constants")

(defvar mara-functions
  '("asm"
    "new"
    "sei"
    "cli"
    "wdr"
    "sleep"
    "nop"
    "break"
    "del")
  "Mara functions")

(defvar mara-keywords-regexp (regexp-opt mara-keywords 'words))
(defvar mara-struct-regexp (regexp-opt mara-struct-keywords 'words))
(defvar mara-type-regexp (regexp-opt mara-types 'words))
(defvar mara-constant-regexp (regexp-opt mara-constants 'words))
(defvar mara-functions-regexp (regexp-opt mara-functions 'words))

(setq mara-struct-keywords nil)
(setq mara-keywords nil)
(setq mara-types nil)
(setq mara-constants nil)
(setq mara-functions nil)

(setq mara-font-lock-keywords
  `(
    (,mara-type-regexp . font-lock-type-face)
    (,mara-constant-regexp . font-lock-constant-face)
    (,mara-keywords-regexp . font-lock-keyword-face)
    (,mara-struct-regexp . font-lock-keyword-face)
    (,mara-functions-regexp . font-lock-builtin-face)
))

(defvar mara-syntax-table nil "Syntax table for `mara-mode'.")
(setq mara-syntax-table
      (let ((synTable (make-syntax-table)))

        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        synTable))

(define-derived-mode mara-mode fundamental-mode
  "Mara"
  "Major mode for editing Mara"
  :syntax-table mara-syntax-table
  
  (setq font-lock-defaults '((mara-font-lock-keywords)))

  (setq mara-functions-regexp nil)
  (setq mara-keywords-regexp nil)
  (setq mara-struct-regexp nil)
  (setq mara-types-regexp nil)
  (setq mara-constants-regexp nil)
)

(provide 'mara-mode)
