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

#!chezscheme
(library (tower)
  (export
   tower:running?
   tower:start-server
   tower:sup-spec
   )
  (import
   (chezscheme)
   (json)
   (keywords)
   (software-info)
   (swish imports)
   )

  (define verbosity (make-parameter 0))

  (define (mapn f ls)
    (let lp ([i 0] [ls ls])
      (if (null? ls)
          '()
          (cons (f i (car ls))
            (lp (+ i 1) (cdr ls))))))

  (define (rpc:respond ws req result)
    (let ([res (json:make-object [jsonrpc "2.0"])]
          [id (json:ref req 'id #f)])
      (when id
        (json:extend-object res [id id]))
      (when result
        (json:extend-object res [result result]))
      (ws:send ws (json:object->string res))))

  (define (send-sync-event ws dir key)
    (ws:send ws
      (json:object->bytevector
       (json:make-object
        [method "synchronize"]
        [params
         (json:make-object
          [sync-token
           (json:make-object
            [dir dir]
            [key key])])]))))

  (define (scalar x)
    (match x
      [(#(,v)) v]
      [() #f]))

  (define ($update-keywords timestamp keywords)
    (db:log 'log-db "delete from keywords")
    (for-each
     (lambda (kw)
       (db:log 'log-db "insert into keywords(timestamp,keyword,meta) values(?,?,?)"
         (coerce timestamp)
         (coerce (json:get kw 'keyword))
         (coerce (json:get kw 'meta))))
     keywords))

  (define (maybe-rows rows)
    (and (not (null? rows)) rows))

  (define ($defns-in-file name filename)
    (maybe-rows
     (execute "
select F.filename,D.line,D.char from refs D
inner join files F on F.file_pk=D.file_fk
where D.name=?
  and F.filename=?
  and D.type='defn'
order by D.line asc"
       name filename)))

  (define ($defns-in-workspace name root-fk)
    (maybe-rows
     (execute "
select F.filename,D.line,D.char from refs D
inner join files F on F.file_pk=D.file_fk
where D.name=?
  and D.root_fk=?
  and D.type='defn'
order by substr(F.filename,-3)='.ss' desc, F.filename asc, D.line asc"
       name root-fk)))

  (define ($refs-in-file name filename)
    (execute "
select D.line,D.char from refs D
inner join files F on F.file_pk=D.file_fk
where D.name=?
  and F.filename=?
order by D.line asc"
      name filename))

  (define ($refs-in-workspace name root-fk)
    (execute "
select F.filename,D.line,D.char from refs D
inner join files F on F.file_pk=D.file_fk
where D.name=?
  and D.root_fk=?
order by substr(F.filename,-3)='.ss' desc, F.filename asc, D.line asc"
      name root-fk))

  (define ($defns-anywhere name filename)
    (maybe-rows
     (execute "
select F.filename,D.line,D.char from refs D
inner join roots R on D.root_fk=R.root_pk
inner join files F on F.file_pk=D.file_fk
where D.name=?
  and D.type='defn'
order by F.filename=? desc, R.timestamp desc,
substr(F.filename,-3)='.ss' desc, F.filename asc, D.line asc"
       name filename)))

  (define do-log
    (case-lambda
     [(level message)
      (do-log level (erlang:now) message)]
     [(level timestamp message)
      (db:log 'log-db
        "insert into events(timestamp,pid,message) values(?,?,?)"
        (coerce timestamp)
        (coerce self)
        (coerce message))
      (cond
       [(< (verbosity) level) (void)]
       [(json:object? message)
        (json:write (current-output-port) message 0)]
       [else
        (display message)
        (newline)])]))

  (define (prefix-integer s)
    (if (string=? s "")
        0
        (char->integer (char-downcase (string-ref s 0)))))

  (define (handle-message ws msg)
    (match (string->symbol (json:get msg 'method))
      [log
       (do-log 2
         (or (json:ref msg '(params timestamp) #f) (erlang:now))
         (json:get msg '(params message)))]
      [get-completions
       (let* ([filename (json:get msg '(params filename))]
              [line (json:get msg '(params line))]
              [char (json:get msg '(params char))]
              [prefix (json:get msg '(params prefix))]
              [root-fk (root-key)]
              [start (erlang:now)]
              [rows
               (transaction 'log-db
                 (let ([file-fk (scalar (execute "select file_pk from files where filename=?" filename))]
                       [pre1 (prefix-integer prefix)])
                   (execute "
select
  case when file_fk=?4 then sum(?5 - min(0, line)) else 0 end as rank
  ,count(refs.name) as count
  ,candidates.name
from
(select keyword as name from keywords where keyword LIKE (?2 || '%')
 union
 select name from refs
 where pre1=?1 and name LIKE (?2 || '%') and root_fk=?3
) candidates
left outer join refs
on candidates.name = refs.name
group by candidates.name
order by rank desc, count desc, candidates.name asc"
                     pre1 prefix root-fk file-fk line)))]
              [rows
               (match rows
                 [(#(,_ 1 ,@prefix) . ,rest) rest]
                 [,_ rows])]
              [nrows (length rows)]
              [completions
               (mapn
                (lambda (i row)
                  (match row
                    [#(,rank ,count ,name)
                     (let ([sortText (format "~6d" i)]
                           [score (- 1.0 (/ i nrows))])
                       (json:make-object
                        [label name]
                        ;;[detail (format "| rank:~a | count:~a | sort: ~a | score: ~a" rank count sortText score)]
                        [sortText sortText]
                        [score score]))]))
                rows)]
              [end (erlang:now)]
              [log (json:make-object
                    [_op_ "get-completions"]
                    [filename filename]
                    [line line]
                    [char char]
                    [prefix prefix]
                    [found (length completions)]
                    [time (- end start)])])
         (do-log 1 log)
         (rpc:respond ws msg completions))]
      [get-definitions
       (let* ([filename (json:get msg '(params filename))]
              [name (json:get msg '(params name))]
              [root-fk (root-key)]
              [start (erlang:now)]
              [defns
               (map
                (lambda (row)
                  (match row
                    [#(,fn ,line ,char)
                     (json:make-object
                      [filename fn]
                      [line line]
                      [char char])]))
                (transaction 'log-db
                  (or ($defns-in-file name filename)
                      ($defns-in-workspace name root-fk)
                      ($defns-anywhere name filename)
                      '())))]
              [end (erlang:now)]
              [log (json:make-object
                    [_op_ "get-definitions"]
                    [filename filename]
                    [name name]
                    [found (length defns)]
                    [time (- end start)])])
         (do-log 1 log)
         (rpc:respond ws msg defns))]
      [get-local-references
       (let* ([filename (json:get msg '(params filename))]
              [name (json:get msg '(params name))]
              [start (erlang:now)]
              [refs
               (map
                (lambda (row)
                  (match row
                    [#(,line ,char)
                     (json:make-object
                      [line line]
                      [char char])]))
                (transaction 'log-db
                  ($refs-in-file name filename)))]
              [end (erlang:now)]
              [log (json:make-object
                    [_op_ "get-local-references"]
                    [filename filename]
                    [name name]
                    [found (length refs)]
                    [time (- end start)])])
         (do-log 1 log)
         (rpc:respond ws msg refs))]
      [get-references
       (let* ([filename (json:get msg '(params filename))]
              [name (json:get msg '(params name))]
              [root-fk (root-key)]
              [start (erlang:now)]
              [refs
               (map
                (lambda (row)
                  (match row
                    [#(,fn ,line ,char)
                     (json:make-object
                      [filename fn]
                      [line line]
                      [char char])]))
                (transaction 'log-db
                  ($refs-in-workspace name root-fk)))]
              [end (erlang:now)]
              [log (json:make-object
                    [_op_ "get-references"]
                    [filename filename]
                    [name name]
                    [found (length refs)]
                    [time (- end start)])])
         (do-log 1 log)
         (rpc:respond ws msg refs))]
      [reset-directory
       (let* ([dir (json:get msg '(params directory))]
              [pk
               (transaction 'log-db
                 (match (scalar (execute "select root_pk from roots where path=?" dir))
                   [#f
                    (execute "insert into roots(timestamp,path,meta) values(?,?,?)"
                      (coerce (erlang:now))
                      (coerce dir)
                      (coerce (json:make-object)))
                    (scalar (execute "select last_insert_rowid()"))]
                   [,pk
                    (execute "update roots set timestamp=?, meta=json_patch(meta,?) where path=?"
                      (coerce (erlang:now))
                      (coerce (json:make-object))
                      dir)
                    (execute "delete from refs where root_fk=? or root_fk is null" pk)
                    pk]))])
         (root-dir dir)
         (root-key pk)
         (send-sync-event ws dir pk)
         (rpc:respond ws msg "ok"))]
      [synchronize
       (let ([dir (json:get msg '(params sync-token dir))]
             [key (json:get msg '(params sync-token key))])
         (assert (not (root-dir)))
         (assert (not (root-key)))
         (root-dir dir)
         (root-key key)
         (do-log 1
           (json:make-object
            [_op_ "synchronize"]
            [root-dir dir]
            [root-key key])))]
      [update-keywords
       (let ([keywords (json:get msg '(params keywords))]
             [start (erlang:now)])
         ($update-keywords start keywords)
         (unless (< (verbosity) 1)
           (transaction 'log-db
             (do-log 1
               (json:make-object
                [_op_ "update-keywords"]
                [keywords (scalar (execute "select count(*) from keywords"))]
                [time (- (erlang:now) start)]))))
         (rpc:respond ws msg "ok"))]
      [update-references
       (let ([filename (json:get msg '(params filename))]
             [refs (json:get msg '(params references))]
             [root-fk (root-key)]
             [start (erlang:now)])
         (assert (path-absolute? filename))
         (db:log 'log-db "insert into files (timestamp,filename) values (?,?) on conflict(filename) do update set timestamp=excluded.timestamp"
           (coerce start)
           (coerce filename))
         (let ([file-fk (transaction 'log-db
                          (scalar (execute "select file_pk from files where filename=?" filename)))])
           (db:log 'log-db "delete from refs where file_fk=?" file-fk)
           (for-each
            (lambda (ref)
              (let ([meta (json:get ref 'meta)]
                    [name (json:get ref 'name)])
                (db:log 'log-db "insert into refs(timestamp,root_fk,file_fk,pre1,name,type,line,char,meta) values(?,?,?,?,?,?,?,?,?)"
                  (coerce start)
                  (coerce root-fk)
                  (coerce file-fk)
                  (coerce (prefix-integer name))
                  (coerce name)
                  (coerce (and (= (json:ref meta 'definition 0) 1)
                               "defn"))
                  (coerce (json:get ref 'line))
                  (coerce (json:get ref 'char))
                  (coerce meta))))
            refs))
         (unless (< (verbosity) 1)
           (transaction 'log-db
             (do-log 1
               (json:make-object
                [_op_ "update-references"]
                [filename filename]
                [definitions (scalar (execute "select count(*) from refs where type='defn'"))]
                [references (scalar (execute "select count(*) from refs"))]
                [time (- (erlang:now) start)]))))
         (rpc:respond ws msg "ok"))]
      [shutdown
       (do-log 1 (json:make-object [_op_ "shutdown"]))
       (rpc:respond ws msg "ok")
       (app:shutdown)]
      ))

  (define root-dir (make-process-parameter #f))
  (define root-key (make-process-parameter #f))

  (define (client)
    (ui:register)
    (let lp ()
      (receive
       [#(ws:message ,ws ,msg)
        (match (try
                (handle-message ws
                  (if (string? msg)
                      (json:string->object msg)
                      (json:bytevector->object msg))))
          [`(catch ,reason ,e)
           (do-log 1 (exit-reason->english reason))
           (throw e)]
          [,_ (lp)])]
       [#(ws:closed ,ws ,code ,reason) 'ok]
       [#(ws:init ,ws) (lp)])))

  (define (ui:start&link)
    (define-state-tuple <ui> waketime clients)

    (define (reply x state)
      `#(reply ,x ,state ,($state waketime)))
    (define (no-reply state)
      `#(no-reply ,state ,($state waketime)))

    (define (init) `#(ok ,(<ui> make [waketime 'infinity] [clients '()])))
    (define (terminate reason state) 'ok)
    (define (handle-call msg from state)
      (match msg
        [#(register ,pid)
         (monitor pid)
         (let ([clients (cons pid ($state clients))])
           (do-log 1
             (json:make-object
              [_op_ "connected"]
              [pid (coerce pid)]
              [total-clients (length clients)]))
           (reply 'ok ($state copy [waketime 'infinity] [clients clients])))]
        [num-clients
         (reply (length ($state clients)) state)]))
    (define (handle-cast msg state) (match msg))
    (define (handle-info msg state)
      (match msg
        [`(DOWN ,_ ,pid ,_)
         (let ([clients (remq pid ($state clients))])
           (do-log 1
             (json:make-object
              [_op_ "disconnected"]
              [pid (coerce pid)]
              [total-clients (length clients)]))
           (no-reply ($state copy
                       [clients clients]
                       [waketime
                        (if (null? clients)
                            (+ (erlang:now) 30000)
                            'infinity)])))]
        [timeout
         (app:shutdown)
         `#(no-reply ,state)]))
    (gen-server:start&link 'ui))

  (define (ui:register)
    (gen-server:call 'ui `#(register ,self)))

  (define (ui:num-clients)
    (gen-server:call 'ui 'num-clients))

  (define (tower:running? port-number)
    (match (try
            (let-values ([(ip op) (connect-tcp "localhost" port-number)])
              (close-port op)))
      [`(catch ,reason) #f]
      [,_ #t]))

  (define (tower-db:setup)
    (define schema-name 'swish-lint)
    (define schema-version "2020-05-22")
    (define (init-db)
      (define (create-index name sql)
        (execute (format "create index if not exists ~a on ~a" name sql)))

      (create-table events
        [timestamp integer]
        [pid text]
        [message text])
      (create-table files
        [file_pk integer primary key]
        [timestamp integer]
        [filename text unique])
      (create-table keywords
        [timestamp integer]
        [keyword text]
        [meta text])
      (create-table refs
        [timestamp integer]
        [root_fk integer]
        [file_fk integer]
        [pre1 integer]
        [name text]
        [type text]
        [line integer]
        [char integer]
        [meta text])
      (create-table roots
        [root_pk integer primary key]
        [timestamp integer]
        [path text]
        [meta text])

      (create-prune-on-insert-trigger 'events 'timestamp 1 10)
      (create-index 'events_timestamp "events(timestamp)")

      (create-index 'refs_name "refs(name)")
      (create-index 'refs_root "refs(root_fk)")
      (create-index 'refs_file "refs(file_fk)")
      (create-index 'refs_pre1 "refs(pre1)")
      (create-index 'refs_type "refs(type)"))
    (define (upgrade-db)
      (match (log-db:version schema-name)
        [,@schema-version 'ok]
        [#f
         (log-db:version schema-name schema-version)
         'ok]
        [,version (throw `#(unsupported-db-version ,schema-name ,version))]))

    (match (db:transaction 'log-db
             (lambda ()
               (upgrade-db)
               (init-db)))
      [#(ok ,_)
       (let ([keywords
              (get-keywords
               (lambda (reason)
                 (do-log 1
                   (json:make-object
                    [_op_ "get-keywords"]
                    [error (exit-reason->english reason)]))))])
         ($update-keywords (erlang:now) keywords))
       (db:expire-cache 'log-db)
       'ignore]
      [,error error]))

  (define (tower:sup-spec port-number)
    `(#(tower-db:setup ,tower-db:setup temporary 1000 worker)
      #(ui ,ui:start&link permanent 1000 worker)
      ,@(http:configure-server 'http port-number
          (http:url-handler
           (match (<request> path request)
             ["/"
              (let* ([num-clients (ui:num-clients)]
                     [limit (http:find-param "limit" params)]
                     [limit (and limit (string->number limit))]
                     [limit (or limit 20)])
                (match (transaction 'log-db
                         (list
                          (scalar (execute "select count(*) from keywords"))
                          (scalar (execute "select count(*) from refs where type='defn'"))
                          (scalar (execute "select count(*) from refs"))
                          (scalar (execute "select count(*) from files"))
                          (execute "select datetime(timestamp/1000,'unixepoch','localtime'),path from roots order by timestamp desc")
                          (execute "select datetime(timestamp/1000,'unixepoch','localtime'),pid,message from events order by rowid desc limit ?" limit)))
                  [(,keywords ,defns ,refs ,files ,roots ,log)
                   (http:respond conn 200 '(("Content-Type" . "text/html"))
                     (html->bytevector
                      `(html5
                        (head
                         (meta (@ (charset "UTF-8")))
                         (title ,(software-product-name)))
                        (body
                         (pre
                          ,(format "Connected clients: ~9:D\n" num-clients)
                          ,(format "         Keywords: ~9:D\n" keywords)
                          ,(format "      Definitions: ~9:D\n" defns)
                          ,(format "       References: ~9:D\n" refs)
                          ,(format "     Unique files: ~9:D\n" files))
                         (pre
                          ,@(map
                             (lambda (root)
                               (match root
                                 [#(,date ,path)
                                  (format "~a ~a\n" date path)]))
                             roots))
                         (pre
                          ,@(map
                             (lambda (row)
                               (match row
                                 [#(,date ,pid ,message)
                                  (format "~a ~a ~a\n" date pid message)]))
                             log))
                         (hr)
                         (pre ,(versions->string))))))]))]
             ["/tower"
              (ws:upgrade conn request (spawn&link client))]
             [,_ #f])))))

  (define (tower:start-server verbose tower-db port-number)
    (verbosity (or verbose 0))
    (log-file
     (cond
      [(not tower-db) ":memory:"]
      [(path-absolute? tower-db) tower-db]
      [else (path-combine (base-dir) tower-db)]))
    (app-sup-spec (append (app-sup-spec) (tower:sup-spec port-number)))
    (app:start)
    (receive))
  )
