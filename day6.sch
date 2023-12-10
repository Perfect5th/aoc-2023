;; Input is a assoc array of time allowed per race to distance to beat
(define EXAMPLE
  '((7 . 9) ; race
    (15 . 40)
    (30 . 200)))

(define EXAMPLE2
  '(71530 . 940200))

;; Count how many ways there are to beat race's target distance within its time constraints.
(define ways-to-win
  (lambda (race)
    (let ([time (car race)]
          [dist (cdr race)])
      (let f ([i 1] [wins 0])
        (cond
         [(= i time) wins]
         [(> (* i (- time i)) dist) (f (+ i 1) (+ wins 1))]
         [else (f (+ i 1) wins)])))))

;; Naive approach. Simply count the number of different length holds that achieve distances greater
;; than the distance to beat.
(define solve1
  (lambda (races)
    (apply * 1 (map ways-to-win races))))

(define solve2
  (lambda (race)
    (ways-to-win race)))

(define INPUT
  '((40 . 233)
    (82 . 1011)
    (84 . 1110)
    (92 . 1487)))

(define INPUT2
  '(40828492 . 233101111101487))

(printf "~a~n" (solve1 EXAMPLE))
(printf "~a~n" (solve1 INPUT))

(printf "~a~n" (solve2 EXAMPLE2))
(printf "~a~n" (solve2 INPUT2))
