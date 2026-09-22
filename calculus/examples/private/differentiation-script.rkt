#lang racket/base

;;;
;;; Differentiation Film Manuscript
;;;

;; Pure random-access timing and Danish narration for the x-squared lesson.

(require racket/list racket/math)

(provide (struct-out shot)
         script theorem-duration transition-duration script-duration film-duration
         state-at state-at-shot-end initial-state shot-offsets)

;; shot represents one narrated visual beat.
;;  - id       symbol?         Stable test and review cue.
;;  - duration  positive-real? Authored duration in seconds.
;;  - panel     symbol?         Prepared left-side teaching panel.
;;  - target    immutable-hash? Presentation state at the endpoint.
;;  - caption   string?         Short screen caption; empty retains the field.
;;  - narration string?         Draft Danish narration; empty is a deliberate hold.
(struct shot (id duration panel target caption narration) #:transparent)

; theorem-duration : exact-positive-integer?
;; Gives the theorem reading and reflection time after the four-second title.
(define theorem-duration 22)

; transition-duration : exact-positive-integer?
;; Gives the semantic theorem-card move to the split layout.
(define transition-duration 2)

; initial-state : immutable-hash?
;; Gives an empty right-hand presentation: no geometry or prepared labels paint.
(define initial-state
  (hash 'x0 1 'h 1 'axes 0 'graph 0 'graph-label 0
        'point 0 'point-reading 0 'neighbour 0 'neighbour-reading 0
        'secant 0 'horizontal 0 'vertical 0 'slope-result 0
        'tangent-lens 0 'tangent-overview 0 'lens 0 'zoom 1))

; S : symbol? positive-real? symbol? immutable-hash? string? string? -> shot?
;; Builds one concise manuscript entry.
(define (S id duration panel target caption narration)
  (shot id duration panel target caption narration))

;; The first 218 seconds realize the reviewed opening after the card move.
;; The original algebra follows it without repeating informal introductions.
(define script
  (list
   (S 'koordinatsystem 6 'theorem (hash 'axes 1) "" "Her er et koordinatsystem. Vi skal tegne grafen for funktionen.")
   (S 'tegn-parabel 5 'theorem (hash 'graph 1) "" "Funktionen f af x lig med x i anden har en parabel som graf.")
   (S 'navngiv-parabel 4 'theorem (hash 'graph-label 1) "Grafen for f(x)=x²." "")
   (S 'vaelg-P 5 'theorem (hash 'point 1) "Vi vælger et punkt P på grafen." "Vi vælger et punkt på grafen og kalder det P.")
   (S 'aflaes-x0 9 'theorem (hash 'point-reading 1) "Punktets førstekoordinat er x₀." "Vi aflæser punktets førstekoordinat på x-aksen. Den kalder vi x nul.")
   (S 'forklar-differentiabel 8 'theorem (hash) "Hvordan ser grafen ud tæt ved P?" "Uformelt betyder differentiabel her, at grafen kommer til at ligne en ret linje, når vi zoomer ind omkring punktet.")
   (S 'lup-et-vis 4 'theorem (hash 'lens 1) "Vi zoomer omkring P." "Vi holder øje med et lille område omkring punktet.")
   (S 'lup-et-zoom-a 6 'theorem (hash 'zoom 2) "" "Nu zoomer vi langsomt ind. Punktet bliver i midten.")
   (S 'lup-et-mellemhold 2 'theorem (hash) "" "")
   (S 'lup-et-zoom-b 7 'theorem (hash 'zoom 8) "" "Kurven ser mere og mere ret ud.")
   (S 'lup-et-zoom-c 4 'theorem (hash 'zoom 40) "" "")
   (S 'lup-et-sluthold 7 'theorem (hash) "Grafen ser lokalt næsten ret ud." "Nu ligner grafen næsten en ret linje. Det er stadig den samme parabel.")
   (S 'lup-et-ud 7 'theorem (hash 'zoom 1) "" "Lad os zoome ud igen.")
   (S 'lup-et-vaek 2 'theorem (hash 'lens 0) "" "")
   (S 'flyt-P-to 6 'theorem (hash 'x0 2) "Vi vælger et andet punkt." "Vi kunne have valgt et andet punkt. Lad os prøve her.")
   (S 'lup-to-vis 4 'theorem (hash 'lens 1) "" "Igen ser vi på området omkring punktet.")
   (S 'lup-to-zoom-a 5 'theorem (hash 'zoom 2) "" "Vi zoomer ind på samme måde.")
   (S 'lup-to-mellemhold 2 'theorem (hash) "" "")
   (S 'lup-to-zoom-b 6 'theorem (hash 'zoom 8) "" "")
   (S 'lup-to-zoom-c 4 'theorem (hash 'zoom 40) "" "")
   (S 'lup-to-sluthold 6 'theorem (hash) "Sætningen gælder for alle x₀ i R." "Også her ser grafen næsten ret ud. Sætningen siger, at det gælder ved ethvert punkt på grafen.")
   (S 'navngiv-tangent 7 'theorem (hash 'tangent-lens 1) "Linjen kaldes tangenten." "Den linje, grafen ligner lokalt, kalder vi tangenten.")
   (S 'lup-to-ud 7 'theorem (hash 'zoom 1) "" "Når vi zoomer ud, kan vi se tangenten i det store koordinatsystem.")
   (S 'vis-oversigtstangent 3 'theorem (hash 'tangent-overview 1 'lens 0) "" "")
   (S 'fortolk-afledet 11 'slope-meaning (hash) "Tangenthældningen er f'(x₀)=2x₀." "Den næste del af sætningen fortæller os tangentens hældning.")
   (S 'haeldning-et 11 'slope-one (hash 'x0 1) "Ved x₀=1 er hældningen 2." "Hvis x nul er et, er hældningen to gange et, altså to.")
   (S 'haeldning-to 11 'slope-two (hash 'x0 2) "Ved x₀=2 er hældningen 4." "Hvis x nul er to, er hældningen to gange to, altså fire.")
   (S 'to-bevismaal 13 'two-goals (hash 'x0 1) "Nu skal vi bevise de to udsagn." "Nu skal vi bevise to ting ved et vilkårligt x nul.")
   (S 'tretrin 11 'three-steps (hash 'tangent-overview 0) "Vi bruger tretrinsreglen." "Vi lader x nul være givet og bruger tretrinsreglen.")
   (S 'vaelg-Q 6 'step-one (hash 'neighbour 1) "Vi vælger et nabopunkt Q." "For at regne hældningen ud vælger vi et nabopunkt Q på grafen.")
   (S 'aflaes-Q 7 'step-one (hash 'neighbour-reading 1) "Nabopunktets førstekoordinat er x₀+h." "Vi kalder dets førstekoordinat x nul plus h.")
   (S 'tegn-sekant 7 'step-one (hash 'secant 1) "Linjen gennem P og Q er sekanten." "Den rette linje gennem de to punkter er en sekant.")
   (S 'vandret-tilvaekst 7 'increments (hash 'horizontal 1) "Δx=h." "Den vandrette tilvækst er h.")
   (S 'lodret-tilvaekst 8 'increments (hash 'vertical 1) "Δy=f(x₀+h)-f(x₀)." "Den lodrette tilvækst kalder vi delta y.")
   (S 'dy-definition 20 'dy-definition (hash) "Trin 1: Udregn funktions-tilvæksten." "Funktions-tilvæksten er f af x nul plus h, minus f af x nul.")
   (S 'dy-indsaet 16 'dy-substitute (hash) "Vi bruger, at funktionen sætter i anden." "Da f af x er x i anden, får vi x nul plus h i anden, minus x nul i anden.")
   (S 'dy-kvadrat 22 'dy-expand (hash) "Vi bruger første kvadratsætning." "Kvadratet giver x nul i anden plus h i anden plus det dobbelte produkt.")
   (S 'dy-forkort 14 'dy-cancel (hash) "De to kvadratled går ud med hinanden." "Derfor er funktions-tilvæksten h i anden plus to gange x nul gange h.")
   (S 'dq-definition 18 'dq-definition (hash) "Trin 2: Udregn differenskvotienten." "Differenskvotienten er sekantens hældning.")
   (S 'dq-del-broek 18 'dq-split (hash) "Vi dividerer hvert led i tælleren med h." "Vi deler brøken op i to.")
   (S 'dq-forkort 16 'dq-cancel (hash 'slope-result 1) "Vi har nu fundet sekantens hældning." "Tilbage står h plus to gange x nul.")
   (S 'graense-start 12 'limit-start (hash) "Trin 3: Undersøg grænseværdien." "Nu skal vi undersøge, hvad der sker med differenskvotienten, når h går mod nul.")
   (S 'graense-approach 16 'limit-start (hash 'h 1/100 'vertical 0) "Nabopunktet nærmer sig det faste punkt." "Vi lader nabopunktet komme tættere og tættere på.")
   (S 'graense-regn 22 'limit-result (hash) "Nu bestemmer vi grænseværdien." "H går mod nul. Grænseværdien er derfor to gange x nul.")
   (S 'bevis-tangent 14 'proved (hash 'neighbour 0 'secant 0 'slope-result 0 'tangent-overview 1) "Sekanthældningerne har en grænseværdi." "Nu har vi vist, at sekanthældningerne har en grænseværdi.")
   (S 'alle-steder 14 'proved (hash 'x0 -1) "Beregningen gælder for ethvert reelt sted." "Der var ikke noget særligt ved det x nul, vi valgte.")
   (S 'tilbage-til-saetning 12 'theorem (hash 'x0 1) "Dermed har vi bevist sætningen." "Funktionen f af x lig med x i anden er altså differentiabel i hele R.")
   (S 'opsamling 18 'summary (hash) "Funktions-tilvækst. Differenskvotient. Grænseværdi." "Det er de tre trin. Dermed har vi bevist vores sætning.")))

; script-duration : nonnegative-real?
;; Gives the accumulated duration of the split-screen lesson.
(define script-duration (apply + (map shot-duration script)))

; film-duration : nonnegative-real?
;; Gives title, theorem, move, and lesson duration.
(define film-duration (+ 4 theorem-duration transition-duration script-duration))

; shot-offsets : immutable-hash?
;; Maps each cue to a time relative to the split-screen lesson.
(define shot-offsets
  (for/fold ([table (hash)] [time 0] #:result table) ([s (in-list script)])
    (values (hash-set table (shot-id s) time) (+ time (shot-duration s)))))

; smooth : real? -> real?
;; Gives the cubic ease used for one authored component track.
(define (smooth t) (* t t (- 3 (* 2 t))))

; apply-target : immutable-hash? immutable-hash? -> immutable-hash?
;; Installs an exact state endpoint.
(define (apply-target state target)
  (for/fold ([next state]) ([(key value) (in-hash target)])
    (hash-set next key value)))

; sample-change : immutable-hash? shot? nonnegative-real? -> immutable-hash?
;; Samples an entire authored beat without a hidden global timing cap.
(define (sample-change before current local)
  (for/fold ([next before]) ([(key finish) (in-hash (shot-target current))])
    (define start (hash-ref before key))
    (define progress (smooth (min 1 (max 0 (/ local (shot-duration current))))))
    (hash-set next key
              (cond [(= progress 0) start]
                    [(= progress 1) finish]
                    [(eq? key 'zoom)
                     (exp (+ (log start) (* progress (- (log finish) (log start)))))]
                    [else (+ start (* progress (- finish start)))]))))

; state-at : real? -> immutable-hash?
;; Samples the presentation state independently of rendering order.
(define (state-at time)
  (unless (and (real? time) (not (nan? time)) (not (infinite? time))
               (<= 0 time script-duration))
    (raise-argument-error 'state-at "finite time in the script interval" time))
  (let loop ([remaining script] [offset 0] [state initial-state])
    (cond [(null? remaining) state]
          [else
           (define current (car remaining))
           (define end (+ offset (shot-duration current)))
           (if (< time end)
               (sample-change state current (- time offset))
               (loop (cdr remaining) end (apply-target state (shot-target current))))])))

; state-at-shot-end : symbol? -> immutable-hash?
;; Samples the exact endpoint of a named cue.
(define (state-at-shot-end id)
  (define current (findf (lambda (candidate) (eq? id (shot-id candidate))) script))
  (unless current (raise-argument-error 'state-at-shot-end "script shot id" id))
  (state-at (+ (hash-ref shot-offsets id) (shot-duration current))))
