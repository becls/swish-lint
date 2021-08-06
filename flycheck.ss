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

(library (flycheck)
  (export
   compile-format
   flycheck:process-file
   report-format
   )
  (import
   (checkers)
   (chezscheme)
   (read)
   (swish imports)
   )

  (define current-filename (make-parameter #f))
  (define current-source-table (make-parameter #f))
  (define report-format (make-parameter #f))

  (define (compile-format fmt-str)
    (let lp ([offset 0] [acc '()])
      (match (pregexp-match-positions (re "%\\w+") fmt-str offset)
        [#f
         (let ([acc (if (< offset (string-length fmt-str))
                        (cons (substring fmt-str offset (string-length fmt-str))
                          acc)
                        acc)])
           (reverse acc))]
        [((,start . ,end))
         (let ([acc (if (= offset start)
                        acc
                        (cons (substring fmt-str offset start) acc))])
           (lp end (cons `(lookup ,(substring fmt-str (+ start 1) end)) acc)))])))

  (define (apply-format fmt-expr ht)
    (for-each
     (lambda (e)
       (match e
         [(lookup ,key) (display (hashtable-ref ht (string->symbol key) key))]
         [,_ (display e)]))
     fmt-expr))

  (define (report x type fmt . args)
    (apply-format (report-format)
      (match x
        [,line (guard (fixnum? line))
          (json:make-object
           [file (current-filename)]
           [type type]
           [line line]
           [column ""]
           [bfp ""]
           [efp ""]
           [msg (apply format fmt args)])]
        [#(range ,start-line ,start-column ,end-line ,end-column)
         (guard (and (fixnum? start-line) (fixnum? start-column)
                     (fixnum? end-line) (fixnum? end-column)))
         (json:make-object
          [file (current-filename)]
          [type type]
          [line start-line]
          [column start-column]
          [bfp ""]
          [efp ""]
          [msg (apply format fmt args)])]
        [`(annotation [source ,src])
         (let* ([bfp (source-object-bfp src)]
                [ht (json:make-object
                     [file (current-filename)]
                     [type type]
                     [bfp bfp]
                     [efp (source-object-efp src)]
                     [msg (apply format fmt args)])])
           (let-values ([(line char) (fp->line/char (current-source-table) bfp)])
             (json:extend-object ht
               [line line]
               [column char]))
           ht)]))
    (newline))

  (define (flycheck:process-file filename)
    (parameterize ([current-filename filename])
      (define loaded #f)
      (match (try (let ([code (utf8->string (read-file filename))])
                    (set! loaded code)
                    (cons code (read-code code))))
        [`(catch ,reason)
         (let-values ([(line msg) (reason->line/msg reason loaded)])
           (report line 'error msg))]
        [(,code . ,annotated-code)
         (parameterize ([current-source-table (make-code-lookup-table code)])
           (check-import/export annotated-code report)
           (check-line-whitespace code #f report)
           (run-optional-checkers annotated-code code report))])))
  )
