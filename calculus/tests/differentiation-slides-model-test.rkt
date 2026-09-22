#lang racket/base
(require rackunit rackunit/text-ui racket/list
         "../examples/private/differentiation-model.rkt"
         "../examples/private/differentiation-script.rkt")
(define (end id) (state-at-shot-end id))
(define tests
  (test-suite
   "Differentiation slides: mathematics and manuscript"
   (test-case "unique ordered beats"
     (check-equal? (length script) (length (remove-duplicates (map shot-id script)))))
   (test-case "opening reveal order has independent geometry groups"
     (for ([pair (in-list '((koordinatsystem tegn-parabel)
                            (tegn-parabel vaelg-P)
                            (vaelg-P aflaes-x0)
                            (aflaes-x0 lup-et-vis)
                            (vaelg-Q aflaes-Q)
                            (aflaes-Q tegn-sekant)
                            (tegn-sekant vandret-tilvaekst)
                            (vandret-tilvaekst lodret-tilvaekst)))])
       (check-true (< (hash-ref shot-offsets (first pair))
                      (hash-ref shot-offsets (second pair)))))
     (check-equal? (hash-ref (end 'koordinatsystem) 'graph) 0)
     (check-equal? (hash-ref (end 'vaelg-P) 'point-reading) 0)
     (check-equal? (hash-ref (end 'navngiv-tangent) 'neighbour) 0)
     (check-equal? (hash-ref (end 'tegn-sekant) 'slope-result) 0))
   (test-case "zoom segments use their full authored durations"
     (define (sample cue fraction)
       (define current (findf (lambda (s) (eq? cue (shot-id s))) script))
       (state-at (+ (hash-ref shot-offsets cue) (* fraction (shot-duration current)))))
     (for ([cue (in-list '(lup-et-zoom-a lup-et-zoom-b lup-et-zoom-c
                           lup-to-zoom-a lup-to-zoom-b lup-to-zoom-c))])
       (define start (hash-ref (sample cue 0) 'zoom))
       (define middle (hash-ref (sample cue 1/2) 'zoom))
       (define finish (hash-ref (sample cue 1) 'zoom))
       (check-true (< start middle finish)))
     (check-equal? (hash-ref (end 'lup-et-sluthold) 'zoom) 40)
     (check-equal? (hash-ref (end 'lup-et-ud) 'zoom) 1))
   (test-case "full theorem precedes split screen"
     (check-true (> theorem-duration 0))
     (check-true (> transition-duration 0))
     (check-eq? (shot-panel (car script)) 'theorem)
     (check-equal? film-duration (+ 4 theorem-duration transition-duration script-duration)))
   (test-case "the model is x squared"
     (for ([x (in-list '(-3 -2 -1 -1/2 0 1/2 1 2 3))])
       (check-equal? (sample-function x) (* x x))))
   (test-case "same semantic points and exact increments"
     (for* ([x (in-list '(-2 -1 0 1 2))] [h (in-list '(1 1/2 1/100))])
       (define m (mathematical-state x h))
       (check-equal? (hash-ref m 'P) (cons x (* x x)))
       (check-equal? (hash-ref m 'Q) (cons (+ x h) (* (+ x h) (+ x h))))
       (check-equal? (hash-ref m 'dx) h)
       (check-equal? (hash-ref m 'dy) (+ (* h h) (* 2 x h)))))
   (test-case "difference quotient agrees with the original algebra"
     (for* ([x (in-list '(-2 -1 0 1 2))] [h (in-list '(1 1/2 1/100))])
       (check-equal? (hash-ref (mathematical-state x h) 'm) (+ h (* 2 x)))))
   (test-case "first circular zoom is centered at x0=1"
     (check-equal? (hash-ref (end 'lup-et-sluthold) 'x0) 1)
     (check-equal? (hash-ref (end 'lup-et-sluthold) 'zoom) 40)
     (check-equal? (hash-ref (end 'lup-et-sluthold) 'lens) 1))
   (test-case "second circular zoom is centered at x0=2"
     (check-equal? (hash-ref (end 'lup-to-sluthold) 'x0) 2)
     (check-equal? (hash-ref (end 'lup-to-sluthold) 'zoom) 40)
     (check-equal? (hash-ref (end 'lup-to-sluthold) 'lens) 1))
   (test-case "the graph is inspected before the tangent overlay"
     (check-equal? (hash-ref (end 'lup-et-sluthold) 'tangent-lens) 0)
     (check-equal? (hash-ref (end 'lup-to-sluthold) 'tangent-lens) 0)
     (check-equal? (hash-ref (end 'navngiv-tangent) 'tangent-lens) 1))
   (test-case "zoom out before choosing another point"
     (check-equal? (hash-ref (end 'lup-et-vaek) 'zoom) 1)
     (check-equal? (hash-ref (end 'lup-et-vaek) 'lens) 0)
     (check-equal? (hash-ref (end 'flyt-P-to) 'lens) 0))
   (test-case "original tangent examples have slopes 2 and 4"
     (check-equal? (hash-ref (mathematical-state 1 1) 'tangent-slope) 2)
     (check-equal? (hash-ref (mathematical-state 2 1) 'tangent-slope) 4))
   (test-case "original three-step order is retained"
     (for ([a (in-list '(tretrin dy-definition dy-kvadrat dq-definition dq-del-broek dq-forkort graense-regn))]
           [b (in-list '(dy-definition dy-kvadrat dq-definition dq-del-broek dq-forkort graense-regn bevis-tangent))])
       (check-true (< (hash-ref shot-offsets a) (hash-ref shot-offsets b)))))
   (test-case "x0 stays fixed through the calculation"
     (for ([id (in-list '(vaelg-Q aflaes-Q tegn-sekant vandret-tilvaekst lodret-tilvaekst dy-definition dy-indsaet dy-kvadrat
                                  dy-forkort dq-definition dq-del-broek dq-forkort
                                  graense-start graense-approach graense-regn bevis-tangent))])
       (check-equal? (hash-ref (end id) 'x0) 1)))
   (test-case "finite animation never evaluates h=0"
     (for ([i (in-range 601)])
       (check-true (> (hash-ref (state-at (* script-duration (/ i 600))) 'h) 0)))
     (check-equal? (hash-ref (end 'graense-approach) 'h) 1/100)
     (check-equal? (hash-ref (mathematical-state 1 1/100) 'm) 201/100))
   (test-case "sampling is independent of the order of frames"
     (define times (for/list ([i (in-range 101)]) (* script-duration (/ i 100))))
     (check-equal? (map state-at times) (reverse (map state-at (reverse times)))))
   (test-case "exact tangent is a separate final object"
     (define last-state (state-at script-duration))
     (check-equal? (hash-ref last-state 'tangent-overview) 1)
     (check-equal? (hash-ref last-state 'secant) 0)
     (check-equal? (hash-ref last-state 'neighbour) 0)
     (check-equal? (hash-ref last-state 'h) 1/100))
   (test-case "invalid times are rejected"
     (check-exn exn:fail:contract? (lambda () (state-at -1)))
     (check-exn exn:fail:contract? (lambda () (state-at (+ script-duration 1)))))))
(module+ test (define failures (run-tests tests)) (unless (zero? failures) (exit 1)))
