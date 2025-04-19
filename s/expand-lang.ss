;;; expand-lang.ss
;;; Copyright 1984-2017 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(define-record-type libreq
  (fields
    (immutable path)
    (immutable version)
    (immutable uid))
  (nongenerative #{libreq fnuxvkuvs8x0xbc68h3hm6-0})
  (sealed #t))

(define-record-type recompile-info
  (fields
    (immutable import-req*)
    (immutable include-req*))
  (nongenerative #{recompile-info fnuxvkuvs8x0xbc68h3hm6-1})
  (sealed #t))

(define-record-type library-info
  (nongenerative #{library-info e10vy7tci6bqz6pmnxgvlq-3})
  (fields
    (immutable path)
    (immutable version)
    (immutable uid)
    (immutable visible?)))

(define-record-type library/ct-info
  (parent library-info)
  (fields
    (immutable import-req*)
    (immutable visit-visit-req*)
    (immutable visit-req*))
  (nongenerative #{library/ct-info fgf0koeh2zn6ajlujfyoyf-4})
  (sealed #t))

(define-record-type library/rt-info
  (parent library-info)
  (fields
    (immutable invoke-req*))
  (nongenerative #{library/rt-info ff86rtm7efmvxcvrmh7t0b-3})
  (sealed #t))

(define-record-type program-info
  (fields (immutable uid) (immutable invoke-req*))
  (nongenerative #{program-info fgc8ptwnu9i5gfqz3s85mr-0})
  (sealed #t))

(module (Lexpand Lexpand?)
  (define library-path?
    (lambda (x)
      (and (list? x) (andmap symbol? x))))

  (define library-version?
    (lambda (x)
      (and (list? x)
           (andmap (lambda (x) (and (integer? x) (exact? x) (>= x 0))) x))))

  (define maybe-optimization-loc? (lambda (x) (or (not x) (box? x)))) ; should be a record
  
  (define maybe-label? (lambda (x) (or (not x) (gensym? x))))

  (define-language Lexpand
    (nongenerative-id #{Lexpand fgy7v2wrvj0so4ro8kvhqo-3})
    (terminals
      (maybe-label (dl))
      (gensym (uid export-id))
      (library-path (path))
      (library-version (version))
      (maybe-optimization-loc (db))
      (prelex (dv))
      (libreq (import-req visit-req visit-visit-req invoke-req))
      (string (include-req))
      (Lsrc (lsrc body init visit-code import-code de)) => unparse-Lsrc
      (recompile-info (rcinfo))
      (library/ct-info (linfo/ct))
      (library/rt-info (linfo/rt))
      (program-info (pinfo)))
    (Outer (outer)
      (recompile-info rcinfo)
      (group outer1 outer2)
      (visit-only inner)
      (revisit-only inner)
      inner)
    (Inner (inner)
      (library/ct-info linfo/ct)
      ctlib
      (library/rt-info linfo/rt)
      rtlib
      (program-info pinfo)
      prog
      lsrc)
    (ctLibrary (ctlib)
      (library/ct uid (export-id* ...) import-code visit-code))
    (rtLibrary (rtlib)
      (library/rt uid
        (dl* ...)
        (db* ...)
        (dv* ...)
        (de* ...)
        body))
    (Program (prog)
      (program uid body))))

(define-record-type source-map
  (nongenerative #{source-map nfne4i66hgd1aupfouk6yvuxc-0})
  (fields
   ;; TODO well, darn: fasl-write doesn't like source tables
   ;;                  but we could (fasl-write (source-table-dump st) op)
   (immutable st)         ;; source-table: src -> (source-info ...)
   ;; TODO we could walk the table move nodes whose key is not a symbol into a separate
   ;;      list of nodes that don't need linking beyond the current file
   (immutable key->node)  ;; TODO rename; hashtable mapping {symbol|prelex|local-label} -> source-info
   (immutable default-src)) ;; ?? at any point we're extending source map for at most one ($sfd)
  ;; TODO make this record type opaque / sealed
  (protocol
   (lambda (new)
     (lambda ()
       (new
        (make-source-table)
        (make-eq-hashtable)
        (cond
         [(#%$sfd) => (lambda (sfd) (make-source-object sfd 0 0))]
         [else #f]))))))

(define-record-type identifier-info
  (nongenerative #{identifier-info nfne4i66hgd1aupfouk6yvuxc-1})
  (fields
   (immutable name)     ;; symbol
   (immutable kind)     ;; prim2 | prim3 | local | global | export | syntax
   ;; TODO these want to tell us about source locations
   ;;  BUT we also want them to be "precise"; i.e., distinguish set! to var from macro call to id w/ same source
   (mutable def)        ;; src
   (mutable set*)       ;; (src ...)
   (mutable ref*))      ;; (src ...)
  (protocol
   (lambda (new)
     (lambda (name kind def-src)
       ;; TODO will we want to replace #f with the magical (make-source-object (#%$sfd) 0 0) ????
       ;;      perhaps in some pass before we resolve everything?
       ;;      heck, maybe we just make a single such source object and stuff it in the source-map itself?
       ;;      then whenever we hit #f in this source map we use that source-object ?
       ;;      (so we do it on demand) ;; OTOH, that might not give us a clean way to drop info related to sfd
       (new name kind def-src '() '())))))

(define-record-type contour-info
  (nongenerative #{contour-info nfne4i66hgd1aupfouk6yvuxc-2})
  (fields
   ;; TODO are name / kind going to be common fields of a parent source-info record type?
   (immutable name)     ;; #f | symbol | library path  ;; TODO what about library version ???
   (immutable kind)     ;; lambda | letrec | letrec* | module | library
   (immutable src)      ;; bfp/efp for region: e.g., individual case-lambda clause
   (immutable bound*)   ;; (identifier-info ...)
   (immutable import*)  ;; edges showing where we were imported (src ...)  ;; TODO more generally: (node ...) ??
   (immutable export*)  ;; (identifier-info ...)
   ))

(define (get-or-add-identifier-info! sm kind key name def-src)
  (let ([cell (eq-hashtable-cell (source-map-key->node sm) key #f)])
    (or (cdr cell)
        (let ([si (make-identifier-info name kind def-src)])
          (set-cdr! cell si)
          si))))

(module (ae->src TODO-FIXME)
  (include "types.ss")  ;; TODO BARF figure out how we really share code
  (define ae->src
    (lambda (ae)
      (and (and (annotation? ae) (fxlogtest (annotation-flags ae) (constant annotation-debug)))
           (annotation-source ae))))

  (define (TODO-FIXME x) ;; TODO FIXME
    (if (source-object? x)
        x
        (ae->src
         (if (syntax-object? x)
             (syntax-object-expression x)
             x))))
  )

(define (get-common-src sm x)
  (cond
   [(TODO-FIXME x) =>
    (lambda (src)
      (let ([cell (source-table-cell (source-map-st sm) src 0)])
        (set-cdr! cell (+ 1 (cdr cell)))
        ;; reuse key, in effect hash consing
        (car cell)))]
   ;; TODO should we keep score here as above? right now folks would have to lookup src in source-map-st to get count
   [else (source-map-default-src sm)]))

(define (cons-uniq x ls)
  (if (memq x ls)
      ls
      (cons x ls)))

(define (add-identifier-ref! sm si id)
  (let ([src (get-common-src sm id)])
    (identifier-info-ref*-set! si
      (cons-uniq src (identifier-info-ref* si)))))

(define (add-identifier-set! sm si id)
  (let ([src (get-common-src sm id)])
    (identifier-info-set*-set! si
      (cons-uniq src (identifier-info-set* si)))))

(define (get-lexical-id-info! sm prelex)
  (get-or-add-identifier-info! sm 'lexical prelex (prelex-name prelex)
    (get-common-src sm (prelex-source prelex))))

(define (add-lexical-ref! sm src prelex)
  (let ([si (get-lexical-id-info! sm prelex)])
    (add-identifier-ref! sm si src)))

(define (add-lexical-set! sm src prelex)
  (let ([si (get-lexical-id-info! sm prelex)])
    (add-identifier-set! sm si src)))

(define (add-lexical-def! sm src prelex)
  (let ([si (get-lexical-id-info! sm prelex)])
    (add-identifier-info-def! sm si src)))

;; TODO temporary thing for debugging
(define (add-identifier-info-def! sm si src)
  (let ([src (get-common-src sm src)])
    (cond
     [(identifier-info-def si) =>
      (lambda (prev)
        (printf "[~a] had def src ~s now change to ~s\n"
          (if (eq? prev src)
              "same"
              "DIFF")
          prev src))])
    (identifier-info-def-set! si src)))

(define (add-global-set! sm src name)
  (printf "punting on global set! ~s\n" name))

;; TODO also do a global-def when we process library guts
(define (add-global-ref! sm src name)
  (printf "punting on global ref! ~s\n" name))
