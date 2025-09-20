;;; abnf-mode.el --- Major mode for highlighting ABNF files   -*- coding: utf-8; lexical-binding: t -*-

;;; Copyright (c) 2025 Alexander Feterman Naranjo
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;; 
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;; 
;;;     * Redistributions in binary form must reproduce the above
;;;       copyright notice, this list of conditions and the following
;;;       disclaimer in the documentation and/or other materials provided
;;;       with the distribution.
;;; 
;;;     * Neither the name of the copyright holder nor the names of its
;;;       contributors may be used to endorse or promote products derived
;;;       from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;;;   Simple derived mode.  Based on fundamental-mode, uses its own
;;;   string-quoting.
;;;   To use, place in a folder listed in the load-path variable
;;;   (or modify the list to add one) and then (require 'abnf-mode).

;;; Code:

;; Font-locking defaults
(defvar abnf-font-lock-defaults
  `(
    (
      (";.*$" . font-lock-comment-face) ; comments
      ("<[^>]*>" . font-lock-doc-face) ; prose
      ("\\(?:\\%[si]\\)?\"[^\"\n\r]*\"" . font-lock-string-face) ; double-quoted strings with optional case-sensitivity
      ("\\%[bdx][0-9a-fA-F]+\\(?:-[0-9a-fA-F]+\\|\\(\\.[0-9a-fA-F]+\\)+\\)?" . font-lock-string-face)  ; numerically-specified strings
      ("[0-9]*\\*[0-9]*" . font-lock-constant-face) ; repetition prefix
      ("ALPHA\\|BIT\\|CHAR\\|CR\\|CRLF\\|CTL\\|DIGIT\\|DQUOTE\\|HEXDIG\\|HTAB\\|LF\\|LWSP\\|OCTECT\\|SP\\|VCHAR\\|WSP" . font-lock-builtin-face) ; core ABNF
      ("^[a-zA-Z][a-zA-Z0-9-]*" . font-lock-variable-name-face) ; non-terminal declaration
      ("[a-zA-Z][a-zA-Z0-9-]*" . font-lock-variable-use-face) ; non-terminal usage
      ("(\\|)\\|\\[\\|]" . font-lock-bracket-face) ; bracketing
      ("=/?\\|/" . font-lock-operator-face) ; definition and alternation operators will be treated as keywords
    )
    t ; don't auto-highglight strings, use our definitions
  )
)


;; The ABNF mode proper
(define-derived-mode abnf-mode fundamental-mode "ABNF"
  "Simple Major mode for highlighting ABNF files."
  (setq font-lock-defaults abnf-font-lock-defaults)
  (setq comment-start ";")
  (setq comment-end "")
  (modify-syntax-entry ?- "_")
  (modify-syntax-entry ?% "'")
  (modify-syntax-entry ?\; "<\n")
  (modify-syntax-entry ?\n ">")
  (modify-syntax-entry ?= ".")
  (modify-syntax-entry ?/ ".")
  (modify-syntax-entry ?\< "(>")
  (modify-syntax-entry ?\> ")<")
)

(add-to-list 'auto-mode-alist '("\\.abnf\\'" . abnf-mode))

(provide 'abnf-mode)

;;; abnf-mode.el ends here

