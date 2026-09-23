#lang racket/base

;;;
;;; Differentiation x Squared 1080p PNG Reproduction Probe
;;;

;; Drives the exact prepared slide scene used by the differentiation example,
;; but retains only one overwritten PNG.  It deliberately prints before scene
;; construction, bitmap conversion, and native PNG writing for every frame, so
;; a native failure identifies the last attempted operation without consuming
;; a full movie's frame-cache space.
;;
;; On this host with Racket 9.3.0.2, the following crashes in bitmap%'s native
;; save-file method on its 547th PNG write:
;;   racket tools/differentiation-x-squared-1080p-repro.rkt \
;;     --start-frame 546 --count 547 --reuse-bitmap


;;;
;;; Imports
;;;

(require racket/class
         racket/cmdline
         racket/file
         racket/path
         (prefix-in a: animate)
         animate/slides
         animate/slides/scene
         (prefix-in example:
                    "../calculus/examples/differential-quotient-x-squared.rkt"))


;;;
;;; Command-line validation
;;;

; nonnegative-integer-option : string? symbol? -> exact-nonnegative-integer?
;;   Parses one nonnegative integer command-line option.
(define (nonnegative-integer-option text who)
  (define value (string->number text))
  (unless (exact-nonnegative-integer? value)
    (raise-user-error who "expected a nonnegative integer, given ~a" text))
  value)

; positive-integer-option : string? symbol? -> exact-positive-integer?
;;   Parses one positive integer command-line option.
(define (positive-integer-option text who)
  (define value (string->number text))
  (unless (exact-positive-integer? value)
    (raise-user-error who "expected a positive integer, given ~a" text))
  value)


;;;
;;; Frame writing
;;;

; scene-frame->bitmap : scene? exact-nonnegative-integer? exact-positive-integer?
;                       slide-theme? -> bitmap%
;;   Rasterizes one scene frame with the same theme contexts as project output.
(define (scene-frame->bitmap scene frame-index fps theme)
  (printf "frame ~a: converting scene to bitmap\n" frame-index)
  (flush-output)
  (a:scene-frame->bitmap scene frame-index
                         #:fps fps
                         #:theme (slide-theme-colors theme)
                         #:typography (slide-theme-typography theme)))

; save-bitmap! : bitmap% exact-nonnegative-integer? path? -> void?
;;   Saves one native bitmap through the same PNG encoder as project output.
(define (save-bitmap! bitmap frame-index output)
  (printf "frame ~a: saving PNG\n" frame-index)
  (flush-output)
  (unless (send bitmap save-file output 'png)
    (raise-user-error 'differentiation-x-squared-1080p-repro
                      "save-file returned #f for frame ~a at ~a"
                      frame-index output)))

; render-and-save-range! : scene? exact-nonnegative-integer? exact-positive-integer?
;                          exact-positive-integer? slide-theme? path?
;                          [#:fixed-frame? boolean?] [#:reuse-bitmap? boolean?]
;                          -> void?
;;   Sequentially saves a bounded frame range, retaining only the final PNG.
(define (render-and-save-range! scene start-frame count fps theme output
                                #:fixed-frame? [fixed-frame? #f]
                                #:reuse-bitmap? [reuse-bitmap? #f])
  (define (source-index offset)
    (if fixed-frame?
        start-frame
        (+ start-frame offset)))
  (cond
    [reuse-bitmap?
     (define bitmap (scene-frame->bitmap scene start-frame fps theme))
     (for ([offset (in-range count)])
       (save-bitmap! bitmap (source-index offset) output))]
    [else
     (for ([offset (in-range count)])
       (define frame-index (source-index offset))
       (save-bitmap!
        (scene-frame->bitmap scene frame-index fps theme)
        frame-index output))]))


;;;
;;; Main program
;;;

(module+ main
  (define width 1920)
  (define height 1080)
  (define fps 30)
  (define start-frame 0)
  (define count 1)
  (define fixed-frame? #f)
  (define reuse-bitmap? #f)
  (define output #f)
  (command-line
   #:program "differentiation-x-squared-1080p-repro.rkt"
   #:once-each
   [("--width") text
    "Raster width in pixels (default: 1920)."
    (set! width (positive-integer-option text 'width))]
   [("--height") text
    "Raster height in pixels (default: 1080)."
    (set! height (positive-integer-option text 'height))]
   [("--fps") text
    "Frames per second (default: 30)."
    (set! fps (positive-integer-option text 'fps))]
   [("--start-frame") text
    "First zero-based source frame (default: 0)."
    (set! start-frame (nonnegative-integer-option text 'start-frame))]
   [("--count") text
    "Number of consecutive frames to write (default: 1)."
    (set! count (positive-integer-option text 'count))]
   [("--fixed-frame")
    "Render start-frame for every write instead of advancing source frames."
    (set! fixed-frame? #t)]
   [("--reuse-bitmap")
    "Render start-frame once and reuse its bitmap for every PNG save."
    (set! fixed-frame? #t)
    (set! reuse-bitmap? #t)]
   [("--output") path
    "PNG destination overwritten after each frame (default: a temporary file)."
    (set! output (string->path path))]
   #:args ()
   (void))
  (define theme lecture-dark)
  (define prepared
    (example:prepare-differentiate-x-squared! #:theme theme))
  (define scene
    (storyboard->scene prepared #:size (list width height)))
  (define total-frames
    (a:scene-frame-count scene #:fps fps))
  (unless (if fixed-frame?
              (< start-frame total-frames)
              (<= (+ start-frame count) total-frames))
    (raise-arguments-error 'differentiation-x-squared-1080p-repro
                           "requested range is within the authored scene"
                           "start-frame" start-frame
                           "count" count
                           "available-frames" total-frames))
  (define output-path
    (or output
        (make-temporary-file "differentiation-x-squared-1080p-repro-~a.png")))
  (render-and-save-range! scene start-frame count fps theme output-path
                          #:fixed-frame? fixed-frame?
                          #:reuse-bitmap? reuse-bitmap?)
  (printf "success: ~a\n" output-path))
