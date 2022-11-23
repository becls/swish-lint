(library (config-params)
  (export
   config:definition-keywords
   )
  (import
   (chezscheme)
   (swish imports)
   )
  ;; Project-level
  (define config:definition-keywords (make-parameter '()))
  )
