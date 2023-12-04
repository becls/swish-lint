# Configuration

A user may customize how Swish-Lint runs personally, or for a whole project.

When creating a new configuration or an external checker, it is useful to set the environment variable `SWISH_LINT_TRACE=yes` and run from the command-line to get additional diagnostics on stderr.

## User-level

When interacting in an editor, user-level configuration is loaded by default. If the system defines `XDG_CONFIG_HOME`, Swish-Lint searches for user-level configuration in `XDG_CONFIG_HOME/swish/swish-lint.ss`. Alternatively, if `HOME` is defined, it loads the configuration from `HOME/.config/swish/swish-lint.ss`.

For consistent results at the command-line and on build systems, user-level configuration is not considered by default and must be enabled explicitly with the `--user-config` option.

### `(find-files cmd . args)`

By default, Swish-Lint recursively searches through a project's directories seeking `*.ss` and `*.ms` files while ignoring `.git` directories. However, it can be advantageous to customize this behavior to utilizing an external program like `git`, as demonstrated below:

```scheme
(find-files "git" "ls-files" "--" "*.ss" "*.ms")
```

### `(optional-checkers e0 e1 ...)`

There are parameterizable checkers in the system. Regular expressions or external programs may be used to augment Swish-Lint's results.

#### `(regexp type expr)`

This form checks for a single regular expression and reports lint of a specific type. `type` is one of `"info"`, `"warning"`, `"error"`. `expr` is a regular expression.

```scheme
(optional-checkers
  (regexp "info" "TODO.*"))
```

#### `(external cmd . args)`

After saving a file, Swish-Lint may spawn an external process to perform additional checks. The external program should be able to ignore most of the details of LSP to focus on the specific task at hand.

Swish-Lint will replace references to the symbol `filename` within `args` with the filename to check. Alternately, `(filename regexp)` can be used. The checker will only run if the filename matches the provided `regexp`.

The external process may emit single lines of output. Swish-Lint attempts to extract exception text and source information as if an exception was generated from Chez Scheme. These messages are always reported as errors.

The external process may emit JSON objects. Each JSON object may contain the following fields: `message`, `type`, `line`, `column`.

`message` (required) is a string to present to the user.

`type` (optional) is a one of `"info"`, `"hint"`, `"warning"`, `"error"`. `type` defaults to `"error"`.

`line` (optional) and `column` (optional) are 1-based indexes. These ignore the complexities of LSP's UTF-16 code point counting.

```scheme
(optional-checkers
  (external "my-checker" "--load" filename))
```

As an example, a simple my-checker program might look like the following:

```scheme
#!/usr/bin/env swish

(json:pretty
 (json:make-object
  [type "info"]
  [message (format "args: ~s" (command-line-arguments))]))
```

## Project-level

Swish-Lint looks for project-level configuration at the top-level of the current repository in `.swish/swish-lint.ss`.

### `(definition-keywords kwd ...)`

In a project, macros have the ability to create definitions. Swish-Lint provides the `definition-keywords` option to
enable navigation to these definitions. Each `kwd` is recognized as a definition for the identifier that follows it in a form analogous to `(define x ...)`, `(define (x ...) ...)`, or the `meta` or `trace-define` variations of these.

For example, in an object-oriented system, macros of the form `(class id body ...)` or `(interface id body ...)` a project may specify the following:

``` scheme
(definition-keywords "class" "interface")
```
