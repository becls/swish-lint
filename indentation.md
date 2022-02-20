# Indentation

With inspiration from
[scmindent](https://github.com/ds26gte/scmindent), this code also
tries to provide a style similar to Emacs while making it
straightforward to add new forms.  This code does not currently
provide "options" as we'd like to apply the same rules to all
code. This may change.

This indenter uses regular expressions to define delimiters in the
file. This allows it to track both code and comments. Generally, it
follows this workflow:

* tokenize the input

* mark tokens with properties like comments, strings, and numbers

* assemble some tokens like named characters and strings

* indent code

* indent comments

## Not Like Emacs

Default Emacs configurations use a `fill-column` of 70. Doom Emacs
defaults this to 80 without being explicitly set in the user's
configuration. This code uses 70.

The following examples attempt to express the differences between
Emacs's style and this indentation code.

### Block comments `#| ... |#`

The indenter leaves block comments alone. This makes them more
convenient for embedding ASCII art and other insignificant trifles of
little importance.

### Datum comments `#;`

The indenter applies its normal code indentation strategy for datum
comments `#;`.

### Certain bytevectors

In Emacs:
``` scheme
#vu8(#x01 #x02 #x03
      #x04 #x05 #x06)
```

The indenter:
``` scheme
#vu8(#x01 #x02 #x03
     #x04 #x05 #x06)
```

### Certain `cond` cases

In Emacs:
``` scheme
(cond
 [(and (> (- end start) 0)
       (as-token text start end #t)) =>
       (lambda (t) (proc acc t start end))]
 [else acc])
```

The indenter:
``` scheme
(cond
 [(and (> (- end start) 0)
       (as-token text start end #t)) =>
  (lambda (t) (proc acc t start end))]
 [else acc])
```

### Certain `match` cases

In Emacs:
``` scheme
(match x
  [,x
    12]
  [#vu8(1 2 3)
    13]
  [(,abc .
     ,def)
    14])
```

The indenter:
``` scheme
(match x
  [,x
   12]
  [#vu8(1 2 3)
   13]
  [(,abc .
    ,def)
   14])
```

### Lines that end in dot

In Emacs:
``` scheme
(match x
  [(,abc .
     ,def)
   12]
  [#vu8(1 2 3)
    13])
```

The indenter:
``` scheme
(match x
  [,x
   12]
  [#vu8(1 2 3)
   13])
```

### Named Let

Emacs handles `let` differently than other forms. Emacs may win this
battle.

In Emacs:
``` scheme
(let lp
    ([x 12]
     [y 13])
  body
  ...)
```

The indenter:
``` scheme
(let lp
  ([x 12]
   [y 13])
  body
  ...)
```

### Unbalanced parenthesis

Emacs fails to indent this expression.

The indenter:
``` scheme
)))))(let ([x 12]
           [y 13])
       body
       ...)
```
