# Swish-Lint

Swish-Lint analyzes source code to flag stylistic errors, helps
developers navigate code bases, and provides code completion.  It
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

## Install

1. Download a pre-built binary from the
[Releases](https://github.com/becls/swish-lint/releases) page and
extract the files to `~/.emacs.d/swish-lint`.

2. Run `swish-lint --version` to verify that it starts properly.

### Configure Emacs

1. Install [lsp-mode 8.0.0](https://emacs-lsp.github.io/lsp-mode/)

2. Install [lsp-ui 8.0.0](https://emacs-lsp.github.io/lsp-ui/)

3. Install [flycheck](https://www.flycheck.org/) - Flycheck version 31
does not work properly with lsp-mode. We recommend either using the
non-stable MELPA url `https://melpa.org/packages/` or another package
management system to get a newer version.

4. Install [company-mode](http://company-mode.github.io/)

You may also want to follow the [performance tuning instructions for
lsp-mode](https://emacs-lsp.github.io/lsp-mode/page/performance/). As
as starting point, add the following to your Emacs configuration,
e.g., .emacs.d/init.el or .emacs file:

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

## Customize Configuration

Swish-Lint provides user-level and project-level
[configuration](configuration.md) to customize your experience.

## Design

Swish-Lint uses a client/server architecture. The client provides the
LSP support. The server maintains a central database called "The
Tower". All good sourcerers keep their knowledge in a tower.

Each time you open a new project, the client scans the code for
definitions and references then updates the tower. This allows
features like "goto definition" to jump from one code base to another.

Swish-Lint attempts to deal with incomplete code. It reads your code
using Chez Scheme's reader. When that fails, it attempts to extract
identifiers using regular expressions. This means that "Find
references" and highlighting may not provide lexical context or
semantics you expect from the language itself.

Swish-Lint generates a set of keywords to use for completions by
invoking `swish`, `scheme`, and `petite` stopping upon success. Swish
is not required, and can be removed from your `PATH` to run in a "Chez
Scheme-Only mode".

## Diagnosing Problems

### Client-side

Swish-Lint writes output to stderr which is available in a dedicated
Emacs buffer. The output is helpful diagnosing protocol or performance
problems.

### Server-side

The tower uses an in-memory database and is automatically started when
a client starts. An internal web server provides an aggregate view of
client logs. See the logs here: http://localhost:51342/?limit=100

Normally the tower's database is stored in memory only. If "goto
definition" or "find references" provides unexpected behavior, you can
start the tower manually and write the database to disk for analysis.

The following command starts the tower, writes the database to disk,
and writes diagnostic messages to the terminal:

`./swish-lint --tower -v --tower-db tower.db3`

### Known Issues

Performance can always be improved.

"Find references" and "goto definition" apply trivial search
rules. Lexical scope and language semantics are ignored.

Chez Scheme's code can fool Swish-Lint. The Chez Scheme code is
complex. Swish-Lint is simple. Your mileage will vary.
