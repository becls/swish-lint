;;; Copyright 2020 Beckman Coulter, Inc.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(require 'lsp-mode)

(defun swish-indent-sexp ()
  "Indent each line of the sexp starting just after point."
  (interactive)
  (save-excursion
    (let ((start (point)))
      (forward-sexp 1)
      (lsp-format-region start (point)))))

(defun swish-indent-line ()
  "Indent current line as Scheme code."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (lsp-format-region start (point))))
  (skip-chars-forward " \t"))

(add-to-list 'lsp-language-id-configuration '(scheme-mode . "scheme"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   '("swish-lint" "--lsp" "-r" "info" "TODO.*"))
  :major-modes '(scheme-mode)
  :server-id 'swish-ls
  ))

(add-hook 'scheme-mode-hook #'lsp)

(provide 'lsp-swish)
