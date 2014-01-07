#!/usr/bin/env racket
#lang racket

(require 
 racket/runtime-path
 racket/serialize
 racket/splicing)

(define (debug-printf fmt . args) (when (current-debug-mode) (apply fprintf (list* (current-error-port) fmt args))))

(define current-root-directory    (make-parameter #f))
(define current-function-to-trace (make-parameter #f))
(define current-debug-mode        (make-parameter #f))
(define current-endpoint-filter   (make-parameter #f))

; Caching
(define all-function-cache        (make-hash))
(define usages-of-cache           (make-hash))
(define recursive-usages-of-cache (make-hash))

(define-runtime-path cache-path "cache")

(when (file-exists? cache-path)
  (debug-printf "Reloading cache...\n")
  (with-input-from-file cache-path
    (thunk
      (set! all-function-cache        (deserialize (read)))
      (set! usages-of-cache           (deserialize (read)))
      (set! recursive-usages-of-cache (deserialize (read))))))

(define (save-cache)
  (with-output-to-file cache-path
    (thunk
      (for ([hash (in-list (list all-function-cache 
                                 usages-of-cache 
                                 recursive-usages-of-cache))])
        (write (serialize hash))
        (newline)))))

; A hash where each value is a set
(define (hashset-add! hash key value)
  (hash-set! hash key (set-add (hash-ref hash key (set)) value)))

; Parameters set during for-each-line
(define current-path        (make-parameter #f))
(define current-file        (make-parameter #f))
(define current-class       (make-parameter #f))
(define current-function    (make-parameter #f))
(define current-line-number (make-parameter #f))
(define current-line        (make-parameter #f))

; Load blacklist functions
(define-runtime-path blacklist-file "blacklist-function-names.txt")
(define blacklist-function-names
  (cond
    [(file-exists? blacklist-file)
     (list->set (file->lines blacklist-file))]
    [else
     (set)]))

; Form of the function signatures returned
(serializable-struct function-signature (file name)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc thing port mode)
     (fprintf port "~a::~a" 
              (function-signature-file thing)
                (function-signature-name thing)))])

(define (current-function-signature)
  (function-signature (current-file) (current-function)))

; Iterate through all files line by line, setting the above params and calling the given thunk for each
(define (for-each-line f)
  (for ([path (in-directory (current-root-directory))]
        #:when (and (file-exists? path)
                    (equal? #"php" (filename-extension path))))
    (define file (file-name-from-path path))
    (parameterize ([current-path path]
                   [current-file file])
      (with-input-from-file path
        (thunk
          (for ([line-number (in-naturals 1)]
                [line (in-lines)])
            (parameterize ([current-line-number line-number]
                           [current-line        line])
              ; Set parameters first, then call the function
              (match line
                [(pregexp #px"^class (\\w+)" (list _ class-name))
                 (current-class class-name)]
                [(pregexp #px"function (\\w+)\\(" (list _ function-name))
                 (current-function function-name)]
                [_ (void)])
              (f))))))))

; Get a list of all of the functions that we've defined
(define (get-all-functions)
  (when (zero? (hash-count all-function-cache))
    (for-each-line
     (thunk
       (match (current-line)
         [(pregexp #px"^class (\\w+)" (list _ class-name))
          (current-class class-name)]
         [(pregexp #px"function (\\w+)\\(" (list _ function-name))
          (current-function function-name)
          (hashset-add! all-function-cache function-name (current-file))]
         [_ (void)]))))
  all-function-cache)

; Get a list of usages for any given function (even if not returned)
(define (usages-of function-name)
  (when (not (hash-has-key? usages-of-cache function-name))
    (for-each-line
     (thunk 
       (when (regexp-match (pregexp function-name) (current-line))
         (hashset-add! usages-of-cache function-name (current-function-signature))))))
  (hash-ref usages-of-cache function-name (set)))

; Trace only usage where the function is returned
(splicing-let ([return-usages-of-cache (make-hash)])
  (define (return-usages-of function-name)
    (when (not (hash-has-key? return-usages-of-cache function-name))
      (define re-function (pregexp function-name))
      (define previous-function #f) ; When this doesn't match (current-function), we've changed functions
      (define flow-variables (set)) ; Store all variables flowing from function-name within a function's scope
      
      ; Add default to prevent rescanning even if this function ever returns
      (hash-ref! return-usages-of-cache function-name (set))
      
      ; Scan through all of the files for any other usages
      (for-each-line
       (thunk 
         ; Reset when function chages
         (when (not (equal? previous-function (current-function)))
           (set! previous-function (current-function))
           (set! flow-variables (set)))

         (match (current-line)
           ; Match any variable assignments
           [(pregexp #px"(\\$\\w+)[^=]*=(.*)" (list _ var rhs))
            ; Is the function we're looking for on the right hand side?
            (when (regexp-match re-function rhs)
              (debug-printf "\n~a:\n  L~a: ~a <- ~a(...)\n" (current-function-signature) (current-line-number) var function-name)
              (set! flow-variables (set-add flow-variables var)))
            
            ; Is any previously known variable on the right hand side?
            (for ([rhs-var (in-set flow-variables)])
              (when (and (not (equal? var rhs-var))
                         (regexp-match (regexp (string-append "\\" rhs-var)) rhs))
                (debug-printf "    L~a: ~a <- ~a\n" (current-line-number) var rhs-var)
                (set! flow-variables (set-add flow-variables var))))]
           
           ; Match returns, if any variable is assigned, store this function
           [(pregexp #px"(echo|return).*(\\$\\w+)" (list _ echo/return var))
            ; Returns of any variable
            (when (set-member? flow-variables var)
              (debug-printf "      L~a: ~a ~a\n" (current-line-number) echo/return var)
              (hashset-add! return-usages-of-cache function-name (current-function-signature)))
            
            ; Direct returns of the function
            (when (regexp-match re-function (current-line))
              (debug-printf "      L~a: ~a ~a(...)\n" (current-line-number) echo/return function-name)
              (hashset-add! return-usages-of-cache function-name (current-function-signature)))]
           
           [_ (void)]))))
    
    (hash-ref return-usages-of-cache function-name (set))))
  
; Trace function usage recursively
(define (recursive-usages-of function-name)
  ; Not in cache, calculate
  (when (not (hash-has-key? recursive-usages-of-cache function-name))
    (hash-set! recursive-usages-of-cache function-name (set))
    (define scanned (set))
    (let loop ([to-scan (set function-name)])
      (debug-printf "\n-- ~a scanned: ~a\n-- ~a left: ~a\n" 
                    (set-count scanned) scanned
                    (set-count to-scan) to-scan)        
      (cond
        ; Done scanning
        [(set-empty? to-scan) (void)]
        ; Already scanned, skip
        [(or (set-member? (hash-ref recursive-usages-of-cache function-name) (set-first to-scan))
             (set-member? scanned (set-first to-scan)))
         (loop (rest to-scan))]
        ; New one; scan, add to output, and add to queue
        [else
         (debug-printf "-- Scanning: ~a\n" (set-first to-scan))
         (set! scanned (set-add scanned (set-first to-scan)))
         (define next-set (return-usages-of (set-first to-scan)))
         (for ([f (in-set next-set)])
           (hashset-add! recursive-usages-of-cache function-name f)
           (hashset-add! recursive-usages-of-cache (set-first to-scan) f))
         (loop (set-subtract
                (set-union
                 (set-rest to-scan)
                 (for/set ([each (in-set next-set)]) (function-signature-name each)))
                scanned))])))
  
  (hash-ref recursive-usages-of-cache function-name))
  
; Trace recursive usage back to the original usage
(define (trace-recursive-usages-of endpoint-name function-name)
  (let/ec return
    (let loop ([current-function-name function-name] [path (list function-name)])
      (debug-printf "~~ Tracing ~a -> ~a, current is ~a, path is ~a\n" function-name endpoint-name current-function-name path)
      (cond
        [(equal? current-function-name endpoint-name)
         (return path)]
        [else
         (for ([next-function (in-set (return-usages-of current-function-name))])
           (define next-function-name (function-signature-name next-function))
           (when (not (member next-function-name path))
             (loop next-function-name (cons next-function-name path))))]))
    (return '())))

; Command line usage
(module+ main
  (command-line
   #:program "trace-usage"
   #:once-each
   [("--source") 
    directory
    "Set the project root directory"
    (current-root-directory directory)]
   [("--function") 
    function-name
    "Original function call to trace"
    (current-function-to-trace function-name)]
   [("--filter")
    filter-re
    "Only return endpoints matching this regular expression"
    (current-endpoint-filter (pregexp filter-re))]
   [("--debug" "--DEBUG")
    "Set debug mode"
    (current-debug-mode #t)])
    
  (when (not (current-root-directory))    (error "No root directory specified via --source"))
  (when (not (current-function-to-trace)) (error "No function specified via --function"))
  
  (for ([endpoint-function (in-list (sort (set->list (recursive-usages-of (current-function-to-trace)))
                                          (λ (f1 f2) (string<? (~a f1) (~a f2)))))])
    (define endpoint-function-name (function-signature-name endpoint-function))
    (when (or (not (current-endpoint-filter))
              (regexp-match (current-endpoint-filter) endpoint-function-name))
      (displayln endpoint-function)
      (for ([traced-function (in-list (trace-recursive-usages-of endpoint-function-name (current-function-to-trace)))])
        (display "  via ")
        (displayln traced-function))
      (newline)))
  
  ; TODO: fix caching, it's not saving for some reason
  #;(save-cache))
