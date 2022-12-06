(library (config-params)
  (export
   config:definition-keywords
   optional-checkers
   )
  (import
   (chezscheme)
   (swish imports)
   )
  ;; Project-level
  (define config:definition-keywords (make-parameter '()))

  ;; User-level
  (define optional-checkers (make-parameter '()))
  )
