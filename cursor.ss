;;; Copyright 2022 Beckman Coulter, Inc.
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

#!chezscheme
(library (cursor)
  (export
   cursor->string
   cursor-line
   cursor:copy
   cursor:goto-line!
   line-str
   lsp:change-content
   string->cursor
   trace-cursor
   )
  (import
   (chezscheme)
   (json)
   (swish imports)
   (trace)
   )
  (define string-append+
    (case-lambda
     [(a b)
      (let ([a? (not (equal? a ""))]
            [b? (not (equal? b ""))])
        (cond
         [(not a?) b]
         [(not b?) a]
         [else (string-append a b)]))]
     [(a b c)
      (let ([a? (not (equal? a ""))]
            [b? (not (equal? b ""))]
            [c? (not (equal? c ""))])
        (cond
         [(not a?)
          (cond
           [(not b?) c]
           [(not c?) b]
           [else (string-append b c)])]
         [(not b?)
          (cond
           [(not c?) a]
           [else (string-append a c)])]
         [(not c?)
          (string-append a b)]
         [else
          (string-append a b c)]))]))

  (define (single-line? s)
    (let lp ([i (fx- (string-length s) 1)])
      (cond
       [(fx< i 0) #t]
       [(char=? (string-ref s i) #\newline) #f]
       [else (lp (fx1- i))])))

  (define-record-type line
    (nongenerative)
    (sealed #t)
    (fields
     (immutable str)
     (mutable prev)
     (mutable next))
    (protocol
     (lambda (new)
       (lambda (str)
         (new str #f #f)))))

  (define-record-type cursor
    (nongenerative)
    (sealed #t)
    (fields
     (mutable line)
     (mutable index$))                  ; 0-based index
    (protocol
     (lambda (new)
       (lambda (first-line)
         (new first-line 0)))))

  (define (cursor-index c)
    (or (cursor-index$ c)
        (let lp ([x (line-prev (cursor-line c))] [index 0])
          (cond
           [(not x)
            (cursor-index$-set! c index)
            index]
           [else
            (lp (line-prev x) (fx1+ index))]))))

  (define (cursor:copy c)
    (make-cursor
     (let* ([old (first-line (cursor-line c))]
            [first (make-line (line-str old))])
       (line-next-set! first
         (let lp ([old (line-next old)] [p first])
           (cond
            [(not old) #f]
            [else
             (let ([new (make-line (line-str old))])
               (line-prev-set! new p)
               (line-next-set! new (lp (line-next old) new))
               new)])))
       first)))

  (define (insert-after! line text)
    (let ([new (make-line text)])
      (line-prev-set! new line)
      (when line
        (let ([next (line-next line)])
          (line-next-set! new next)
          (line-next-set! line new)
          (when next
            (line-prev-set! next new))))
      new))

  (define (replace! c line1 line2 prefix text suffix)
    (cond
     [(single-line? text)
      (let ([new (make-line (string-append+ prefix text suffix))]
            [prev (line-prev line1)]
            [next (line-next line2)])
        (when prev
          (line-next-set! prev new))
        (when next
          (line-prev-set! next new))
        (line-prev-set! new prev)
        (line-next-set! new next)
        ;; No need to update cursor index
        (cursor-line-set! c new)
        c)]
     [else
      (let* ([prev (line-prev line1)]
             [next (line-next line2)]
             [text-lines (split text #\newline)]
             [first (make-line (string-append+ prefix (car text-lines)))]
             [last #f]
             [rest
              (let lp ([ls (cdr text-lines)] [p first])
                (match ls
                  [(,str)
                   (let ([new (make-line (string-append+ str suffix))])
                     (line-prev-set! new p)
                     (line-next-set! new next)
                     (set! last new)
                     new)]
                  [(,str . ,rest)
                   (let ([new (make-line str)])
                     (line-prev-set! new p)
                     (line-next-set! new (lp rest new))
                     new)]))])
        (when prev
          (line-next-set! prev first))
        (when next
          (line-prev-set! next last))
        (line-prev-set! first prev)
        (line-next-set! first rest)
        ;; No need to update cursor index
        (cursor-line-set! c first)
        c)]))

  (define (move-down line n)
    (if (= n 0)
        line
        (move-down (line-next line) (- n 1))))

  (define (move-up line n)
    (if (= n 0)
        line
        (move-up (line-prev line) (- n 1))))

  (define (first-line line)
    (let ([prev (line-prev line)])
      (if prev
          (first-line prev)
          line)))

  (define (last-line line)
    (let ([next (line-next line)])
      (if next
          (last-line next)
          line)))

  (define (cursor:goto-line! c line0)
    (let* ([i (cursor-index c)]
           [delta (- line0 i)])
      (cond
       [(zero? delta)]
       [(positive? delta)
        (cursor-line-set! c (move-down (cursor-line c) delta))
        (cursor-index$-set! c line0)]
       [else
        (cursor-line-set! c (move-up (cursor-line c) (abs delta)))
        (cursor-index$-set! c line0)])
      c))

  (define (ip->lines ip)
    (let lp ([acc (make-line (get-line ip))])
      (let ([line (get-line ip)])
        (if (eof-object? line)
            (insert-after! acc "")
            (lp (insert-after! acc line))))))

  (define (string->cursor text)
    (make-cursor
     (if (equal? text "")
         (make-line "")
         (first-line (ip->lines (open-input-string text))))))

  (define (cursor->string c)
    (let ([op (open-output-string)])
      (let lp ([line (first-line (cursor-line c))])
        (when line
          (let ([str (line-str line)]
                [next (line-next line)])
            (display-string str op)
            (when (or next (not (equal? str "")))
              (newline op))
            (lp next))))
      (get-output-string op)))

  (define (lsp:change-content cursor deltas)
    (fold-left
     (lambda (cursor delta)
       (let ([text (json:get delta 'text)]
             [range (json:ref delta 'range #f)])
         (cond
          [range
           (let* ([start (json:get range 'start)]
                  [end (json:get range 'end)]
                  [sl (json:get start 'line)]
                  [sc (json:get start 'character)]
                  [el (json:get end 'line)]
                  [ec (json:get end 'character)]
                  [_ (cursor:goto-line! cursor sl)]
                  [line1 (cursor-line cursor)]
                  [sstr (line-str line1)]
                  [slen (string-length sstr)]
                  [prefix (substring sstr 0 (min sc slen))]
                  [line2 (move-down line1 (- el sl))]
                  [estr (line-str line2)]
                  [elen (string-length estr)]
                  [suffix (substring estr (min ec elen) elen)])
             (replace! cursor line1 line2 prefix text suffix))]
          [else
           (string->cursor text)])))
     cursor
     deltas))

  (define (trace-cursor cursor)
    (define ln (cursor-index cursor))
    (when trace?
      (let lp ([n 0] [line (first-line (cursor-line cursor))])
        (when line
          (fprintf (trace-output-port) "~a~d: ~a\n" (if (= n ln) "*" " ") n (line-str line))
          (lp (+ n 1) (line-next line))))
      (newline (trace-output-port))
      (flush-output-port (trace-output-port)))
    cursor)
  )

#!eof mats

(load-this-exposing
 '(cursor-index cursor-index$ cursor-index$-set!
    first-line last-line line-next
    string-append+
    ))

(import
 (cursor)
 (software-info))

(software-info:install)

(define (make-pos line char)
  (json:make-object [line line] [character char]))

(define (make-range start end)
  (json:make-object [start start] [end end]))

(define (check-doc cursor expected)
  (let ([index (cursor-index$ cursor)])
    (cond
     [index
      (cursor-index$-set! cursor #f)
      (let ([expected (cursor-index cursor)])
        (unless (equal? expected index)
          (throw `#(mismatched-index ,expected ,index))))]
     [else
      (printf "there is not an index\n")]))

  (trace-cursor cursor)
  (let lp ([n 1] [line (first-line (cursor-line cursor))] [exp (split expected #\newline)])
    (cond
     [line
      (match exp
        [() (throw `#(line-without-expected ,(line-str line)))]
        [(,exp . ,rest)
         (unless (equal? (line-str line) exp)
           (printf "> ~a: ~a\n" n exp)
           (printf "< ~a: ~a\n" n (line-str line))
           (throw `#(mismatch ,exp ,(line-str line))))
         (lp (+ n 1) (line-next line) rest)])]
     [else
      (unless (null? exp)
        (throw `#(expected-without-line ,(car exp))))]))

  (let ([flip (cursor->string (string->cursor expected))])
    (unless (equal? expected flip)
      (throw 'round-trip-failed)))

  ;; Moving all the way down and back up should yield the same object
  (let ([line (cursor-line cursor)])
    (unless (eq? (first-line line) (first-line (last-line line)))
      (throw 'incorrect-structure)))
  )

(isolate-mat string-append+ ()
  (match-let*
   ([,a "a"]
    [,b "b"]
    [,c "c"]
    [#t (eq? a (string-append+ a ""))]
    [#t (eq? b (string-append+ "" b))]
    ["ab" (string-append+ a b)]
    ["" (string-append+ "" "")]
    [#t (eq? a (string-append+ a "" ""))]
    [#t (eq? b (string-append+ "" b ""))]
    [#t (eq? c (string-append+ "" "" c))]
    ["ab" (string-append+ a b "")]
    ["ac" (string-append+ a "" c)]
    ["bc" (string-append+ "" b c)]
    ["abc" (string-append+ a b c)]
    ["" (string-append+ "" "" "")])
   'ok))

(isolate-mat edit-line ()
  (define (perform-edits ls)
    (match-let*
     ([,start (string-append "\n" (make-string (+ (apply max ls) 1) #\*) "\n")]
      [,doc (string->cursor start)]
      [,_ (check-doc doc start)]
      [,doc
       (fold-left
        (lambda (doc n)
          (lsp:change-content doc
            (list
             (json:make-object
              [text (format "~d" n)]
              [range
               (make-range
                (make-pos 1 n)
                (make-pos 1 (+ n 1)))]))))
        doc
        ls)]
      [,_ (check-doc doc (format "\n~{~a~}\n" (sort < ls)))])
     'ok))
  (perform-edits '(0 1 2 3 4 5))
  (perform-edits '(5 4 3 2 1 0))
  (perform-edits '(0 3 2 5 1 4)))

(isolate-mat edit-lines ()
  (define (perform-edits ls)
    (match-let*
     ([,start (format "~{*** ~a ***\n~}" (iota (+ (apply max ls) 1)))]
      [,doc (string->cursor start)]
      [,_ (check-doc doc start)]
      [,doc
       (fold-left
        (lambda (doc n)
          (lsp:change-content doc
            (list
             (json:make-object
              [text (format "~d\n" n)]
              [range
               (make-range
                (make-pos n 0)
                (make-pos (+ n 1) 0))]))))
        doc
        ls)]
      [,_ (check-doc doc (format "~{~a\n~}" (sort < ls)))])
     'ok))
  (perform-edits '(0 1 2 3 4 5))
  (perform-edits '(5 4 3 2 1 0))
  (perform-edits '(0 3 2 5 1 4)))

(isolate-mat insert-multiple-lines ()
  (define (perform-edits ls)
    (match-let*
     ([,start (format "~{*** ~a ***\n~}" (iota (* (+ (apply max ls) 1) 2)))]
      [,doc (string->cursor start)]
      [,_ (check-doc doc start)]
      [,doc
       (fold-left
        (lambda (doc n)
          (lsp:change-content doc
            (list
             (json:make-object
              [text (format "~d\nbonus\n" n)]
              [range
               (make-range
                (make-pos (* n 2) 0)
                (make-pos (+ (* n 2) 2) 0))]))))
        doc
        ls)]
      [,_ (check-doc doc (format "~{~a\nbonus\n~}" (sort < ls)))])
     'ok))
  (perform-edits '(0 1 2 3 4 5))
  (perform-edits '(5 4 3 2 1 0))
  (perform-edits '(0 3 2 5 1 4)))

(isolate-mat basic ()
  (match-let*
   ([,start "(let ()\n  (+ x y))\n"]
    [,doc (string->cursor start)]
    [,_ (check-doc doc start)]
    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text "a"]
              [range
               (make-range
                (make-pos 0 0)
                (make-pos 0 0))])))]
    [,_ (check-doc doc (string-append "a" start))]
    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text ""]
              [range
               (make-range
                (make-pos 0 0)
                (make-pos 0 1))])))]
    [,_ (check-doc doc start)]
    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text ""]
              [range
               (make-range
                (make-pos 0 4)
                (make-pos 1 9))])))]
    [,_ (check-doc doc "(let)\n")]
    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text " ()\n  (+ x y)"]
              [range
               (make-range
                (make-pos 0 4)
                (make-pos 0 4))])))]
    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text " ()\n  (+ x y)"]
              [range
               (make-range
                (make-pos 0 4)
                (make-pos 1 9))])))]
    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text ")"]
              [range
               (make-range
                (make-pos 1 8)
                (make-pos 1 9))])))]
    [,_ (check-doc doc start)]

    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text "abc\n"]
              [range
               (make-range
                (make-pos 2 0)
                (make-pos 2 0))])))]
    [,_ (check-doc doc (string-append start "abc\n"))]

    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text ""]
              [range
               (make-range
                (make-pos 2 0)
                (make-pos 2 3))])))]
    [,_ (check-doc doc (string-append start "\n"))]
    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text ""]
              [range
               (make-range
                (make-pos 2 0)
                (make-pos 3 0))])))]
    [,_ (check-doc doc start)]
    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text ""])))]
    [,_ (check-doc doc "")]
    [,doc (lsp:change-content doc
            (list
             (json:make-object
              [text "abc\n"])))]
    [,_ (check-doc doc "abc\n")]
    )
   'ok))
