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

;; TODO export machinery to
;;      - make source-map
;;      - merge them
;;      - dump contents

(define-record-type source-map
  (nongenerative #{source-map 2lv8mlz2kzyg0qqg338ia10pk-0})
  (fields
   ;; TODO well, darn: fasl-write doesn't like source tables
   ;;                  but we could (fasl-write (source-table-dump st) op)
   (immutable st)         ;; source-table: src -> (source-info ...)
   ;; TODO we could walk the table move nodes whose key is not a symbol into a separate
   ;;      list of nodes that don't need linking beyond the current file
   (immutable key->node)  ;; TODO rename; hashtable mapping {symbol|prelex|local-label} -> source-info
   (immutable prim->node)
   (immutable default-cell)) ;; ?? at any point we're extending source map for at most one ($sfd)
  ;; TODO make this record type opaque / sealed
  (protocol
   (lambda (new)
     (lambda ()
       (define default-src
         (cond
          [(#%$sfd) => (lambda (sfd) (make-source-object sfd 0 0))]
          [else #f]))
       (new
        ;; TODO should we keep a count of hits for source in source table?
        (make-source-table)
        (make-eq-hashtable)
        (make-eq-hashtable)
        ;; TODO using (default-src . '()) was misguided: we don't want client to merge everything w/ same source when we can clearly distinguish references prelex for which we have no source
        ;;      still, it's kind of neat to see that file foo.ss contains a reference to our identifier bar somewhere
        (cons default-src '()))))))

;; TODO should we have some notion of approximate "vicinity" source to fall back on
;;      - e.g., for global-set! of library export, if no source, point at source for the identifier where we exported it

(define-record-type identifier-info
  (nongenerative #{identifier-info nfne4i66hgd1aupfouk6yvuxc-1})
  (fields
   (immutable name)     ;; symbol
   (immutable kind)     ;; prim2 | prim3 | local | global | export | syntax
   ;; TODO have a count field for number of times we "generated" this node? (would stencil-vector be useful here w/ diff index for sc-expand, before-cp0, after-cp0 ?)
   ;;      maybe if stencil-vector-mask already cool for our pass then we can use stencil-vector-set! else use stencil-vector-update ?
   ;; TODO these want to tell us about source locations
   ;;  BUT we also want them to be "precise"; i.e., distinguish set! to var from macro call to id w/ same source
#;    (mutable count) ;; see speculation above; but that doesn't tell us which ref or set was eliminated by cp0; need to build more to see what we really want
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

(define-record-type prim-info
  (nongenerative #{prim-info hnfnegd1aupfouk6yvuxc466i-2})
  (fields
   (immutable name)     ;; symbol
   (mutable safe*)      ;; (src ...)
   (mutable unsafe*))   ;; (src ...)
  (protocol
   (lambda (new)
     (lambda (name)
       (new name '() '())))))

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

(define (get-src-cell sm x)
  (cond
   [(TODO-FIXME x) =>
    (lambda (src)
      ;; TODO this model, where we extend w/ list of source-infos means we have a lot of cells of the form (src . ()) because that src is used only as a reference
      ;;      so we might want to prune those when we dump the source table; for which it might be nice to have source-table-fold instead of just dump
      ;;      reason to keep those cells around would be to continue commonizing that src, but that probably doesn't matter across files
      (source-table-cell (source-map-st sm) src '()))]
   [else (source-map-default-cell sm)]))

(define reuse-src car)

(define (get-common-src sm x)
  ;; reuse key, in effect hash consing
  (reuse-src (get-src-cell sm x)))

;; TODO what the heck was I thinking? we need to use the key and maybe the kind and we probably want to do that within the source-table cell
(define (get-or-add-node! sm get-table kind key name def-src get-or-add!)
  (let ([cell (hashtable-cell (get-table sm) key #f)])
    (or (cdr cell)
        (let ([si (get-or-add! sm kind key name def-src)])
          (set-cdr! cell si)
          si))))

(define (get-or-add-identifier-info! sm kind key name def-src)
  (get-or-add-node! sm source-map-key->node kind key name def-src
    get-or-add-node-by-source!))

;; TODO oops, recent reorg ended up overwriting prim info for safe / unsafe variant
;;      --> one option is to use the actual primref as the key; that's the whole point of primref, it bakes in o=2/3
(define (get-or-add-prim-info! sm name)
  ;; name is the key; we don't have source
  (let ([cell (hashtable-cell (source-map-prim->node sm) name #f)])
    (or (cdr cell)
        (let ([pi (make-prim-info name)])
          (set-cdr! cell pi)
          pi))))

;; TODO rename / abstract since this currently talks about identifier-info ???
(define (get-or-add-node-by-source! sm kind key name def-src)
  ;; TODO will likely pull fields out into parent record
  ;;      *BUT* eq? may not work for name if we extend this to contour
  (define source-info-name identifier-info-name)          
  (define source-info-kind identifier-info-kind)          
  (define (matches si)
    ;; TODO consider how to deduplicate source-info nodes
    ;;      - may be pointless to represent each unique graph of references that occurs during expansion
    ;;      - folks interact w/ source, so we may need to coalesce based on source info
    ;;        - yet might not have def-src in some cases
    ;;        - but we do have "kind" and "key" so we don't have to punt entirely
    ;;      - how do we merge nodes that source in common?
    ;;        - ? rely on source table (extended to handle no-source case)
    ;;        - leverage def-src if we have it (prelex)
    ;;        - lookup in source-map-st and reuse existing source-info of the same kind (and name) in that bucket?
    ;;          - have to check kind and name because #%$replace-source could mean we dump multiple things in same bucket
    (and (eq? (source-info-name si) name)
         (eq? (source-info-kind si) kind)
         si))
  ;; TODO misguided; we don't want to commonize every #f src occurrence so they all appear to point at one another
  ;;      - maybe use (list sfd key kind) as a hash key into equal-hash table
  ;;        - and skip the #<source ... 0 0> hack?
  ;;        - or do that wiring in a later phase?
  (let ([src-cell (get-src-cell sm def-src)])
    (let find ([p (cdr src-cell)])
      (cond
       [(null? p)
        (let ([si (make-identifier-info name kind (reuse-src src-cell))])
          (set-cdr! src-cell (cons si (cdr src-cell)))
          si)]
       [(matches (car p))]
       [else (find (cdr p))]))))

(define (cons-uniq x ls) ;; add counts?
  (if (memq x ls)
      ls
      (cons x ls)))

(define (add-node-use! sm info src get-fld set-fld!)
  (let ([src (get-common-src sm src)])
    (set-fld! info (cons-uniq src (get-fld info)))))

(define (add-identifier-ref! sm si id)
  (add-node-use! sm si id identifier-info-ref* identifier-info-ref*-set!))

(define (add-identifier-set! sm si id)
  (add-node-use! sm si id identifier-info-set* identifier-info-set*-set!))

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
