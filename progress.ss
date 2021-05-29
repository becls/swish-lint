;;; Copyright 2021 Beckman Coulter, Inc.
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

(library (progress)
  (export
   progress:inc-done
   progress:inc-total
   progress:start
   )
  (import
   (chezscheme)
   (swish imports)
   )
  (define (progress:start title render-msg send-msg trace-expr)
    (define-state-tuple <progress>
      start-time
      wake-time
      max-reported
      total
      done
      )
    (define (no-reply state)
      `#(no-reply ,state ,(or ($state wake-time) 'infinity)))
    (define (wake-at old)
      (or old (+ (erlang:now) 100)))
    (define (start-at old)
      (or old
          (begin
            (send-msg
             (json:make-object
              [kind "begin"]
              [title title]
              [percentage 0]))
            (erlang:now))))

    (define (init)
      `#(ok
         ,(<progress> make
            [start-time #f]
            [wake-time #f]
            [max-reported 0]
            [total 0]
            [done 0])))
    (define (terminate reason state)
      ($state open [start-time])
      (when start-time
        (send-msg (json:make-object [kind "end"]))
        (trace-expr `(time ,title ,(- (erlang:now) start-time) ms))))
    (define (handle-call msg from state) (match msg))
    (define (handle-cast msg state)
      (match msg
        [inc-total
         (no-reply
          ($state copy*
            [start-time (start-at start-time)]
            [wake-time (wake-at wake-time)]
            [total (fx+ total 1)]))]
        [inc-done
         (no-reply
          ($state copy*
            [start-time (start-at start-time)]
            [wake-time (wake-at wake-time)]
            [done (fx+ done 1)]))]))
    (define (handle-info msg state)
      (match msg
        [timeout
         (<progress> open state [max-reported total done])
         (let ([msg (render-msg done total)]
               [percent
                (max max-reported
                     (min 100
                          (if (zero? total)
                              0
                              (floor (* (/ done total) 100)))))])
           (send-msg
            (json:make-object
             [kind "report"]
             [message msg]
             [percentage percent]))
           (let ([state ($state copy [max-reported percent] [wake-time #f])])
             (if (< done total)
                 (no-reply state)
                 `#(stop normal ,state))))]))
    (gen-server:start #f))

  (define (progress:inc-total who)
    (gen-server:cast who 'inc-total))

  (define (progress:inc-done who)
    (gen-server:cast who 'inc-done))
  )
