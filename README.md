# Swish Lint

Swish-Lint is a tool that analyzes source code to flag programming
errors, bugs, stylistic errors, and suspicious constructs.  It
provides feedback to improve code quality during development before
reviews and inspections.  Swish-Lint provides language features like
auto completion, go to definition, and find all references to editors
like Emacs and VSCode that support the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/).

Features include:

- Code completion
- Goto definition
- Find references
- [Indentation](indentation.md)

## Install for Emacs

1. Install [lsp-mode](https://emacs-lsp.github.io/lsp-mode/)

1. Install [lsp-ui](https://emacs-lsp.github.io/lsp-ui/)

1. Install [flycheck](https://www.flycheck.org/) - Flycheck version 31
does not work properly with lsp-mode. We recommend either using the
non-stable MELPA url `https://melpa.org/packages/` or another package
management system to get a newer version.

1. Install [company-mode](http://company-mode.github.io/)

You may also want to follow the [performance tuning instructions for
lsp-mode](https://emacs-lsp.github.io/lsp-mode/page/performance/).

```
(require 'flycheck)

(require 'company)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0.0)

(require 'lsp-mode)
(setq lsp-prefer-capf t)
(setq lsp-prefer-flymake nil)
(setq lsp-enable-snippet nil)
(setq lsp-idle-delay 0.100)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'scheme-mode-hook 'flycheck-mode)

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(add-to-list 'load-path "~/.emacs.d/swish-lint")
(add-to-list 'exec-path "~/.emacs.d/swish-lint")
(require 'lsp-swish)
```

To use Swish-Lint's indentation, bind `swish-indent-sexp` in your
scheme mode hook:

```
(add-hook 'scheme-mode-hook
  (function
   (lambda ()
     (local-set-key (kbd "C-M-q") 'swish-indent-sexp))))
```
