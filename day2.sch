;; Target numbers of red, green, and blue cubes
(define red 12)
(define green 13)
(define blue 14)

(define EXAMPLE1
  '((1 ((3 blue) (4 red)) ((1 red) (2 green) (6 blue)) ((2 green)))
    (2 ((1 blue) (2 green)) ((3 green) (4 blue) (1 red)) ((1 green) (1 blue)))
    (3 ((8 green) (6 blue) (20 red)) ((5 blue) (4 red) (13 green)) ((5 green) (1 red)))
    (4 ((1 green) (3 red) (6 blue)) ((3 green) (6 red)) ((3 green) (15 blue) (14 red)))
    (5 ((6 red) (1 blue) (3 green)) ((2 blue) (1 red) (2 green)))))

;; Returns #t if a sample is impossible given TARGETS
(define impossible-sample?
  (lambda (sample)
    (find (lambda (s)
            (> (car s) (eval (cadr s))))
          sample)))

;; Returns #t if a game is possible given TARGETS
(define impossible?
  (lambda (game)
     (find impossible-sample? (cdr game))))

;; Sums the numbers of the impossible games.
(define solve1
  (lambda (games)
    (fold-left + 0 (map car (filter (lambda (g) (not (impossible? g))) games)))))

;; Returns the number of cubes for each sample for color
(define get-color-counts
  (lambda (color samples)
    (let f ([ss samples] [acc '()])
      (if (null? ss)
          acc
          (let* ([s (car ss)]
                 [found (find (lambda (c) (eq? (cadr c) color)) s)])
            (if found
                (f (cdr ss) (cons (car found) acc))
                (f (cdr ss) acc)))))))

;; Gets the minimum number of a specific color cubes in a game's samples
(define get-min
  (lambda (color samples)
    (apply max 0 (get-color-counts color samples))))

;; Calculates the power of a game, defined as the product of the minimum amounts of each colour
;; cube required to produce that game.
(define power
  (lambda (game)
    (let ([samples (cdr game)])
      (* (get-min 'red samples)
         (get-min 'green samples)
         (get-min 'blue samples)))))

;; Sums the power of the minimum set of cubes for each game.
(define solve2
  (lambda (games)
    (fold-left + 0 (map power games))))

(define INPUT1
'((1 ((2 blue) (3 red)) ((3 green) (3 blue) (6 red)) ((4 blue) (6 red)) ((2 green) (2 blue) (9 red)) ((2 red) (4 blue)))
(2 ((4 red) (1 green)) ((3 red)) ((13 green) (5 red) (3 blue)) ((3 green) (2 red)) ((3 blue) (5 red) (3 green)) ((2 red) (3 blue) (12 green)))
(3 ((4 red) (1 green) (1 blue)) ((1 red) (1 blue)) ((6 red) (1 green)) ((6 red) (3 blue) (1 green)) ((4 red)))
(4 ((4 blue) (12 red) (4 green)) ((6 green) (3 blue) (19 red)) ((3 blue) (2 red) (2 green)))
(5 ((1 red) (5 blue) (16 green)) ((1 red) (6 green) (3 blue)) ((2 red) (12 blue)) ((17 blue) (3 green)) ((7 green) (2 red) (6 blue)))
(6 ((3 green) (1 blue) (5 red)) ((5 green) (5 red)) ((2 green) (2 blue) (3 red)) ((5 green) (2 red)) ((3 green) (6 red) (3 blue)) ((5 green) (4 red)))
(7 ((15 blue) (1 red) (6 green)) ((4 blue) (7 green) (2 red)) ((14 blue) (5 green) (2 red)))
(8 ((6 blue) (3 green) (10 red)) ((2 blue) (1 green) (5 red)) ((6 blue) (3 green) (12 red)) ((11 red) (1 green) (1 blue)) ((5 blue) (14 red) (3 green)) ((3 red)))
(9 ((15 red) (3 blue)) ((1 blue) (16 red)) ((1 red) (3 blue)) ((1 blue) (1 green) (9 red)))
(10 ((1 red)) ((1 blue) (7 green)) ((1 green) (5 blue)) ((3 blue) (3 green)) ((1 green)))
(11 ((19 blue) (13 green)) ((19 blue) (2 green)) ((10 blue) (3 red) (12 green)) ((11 blue) (1 red) (6 green)))
(12 ((7 green) (5 blue)) ((6 green) (3 red) (6 blue)) ((2 red) (5 blue) (15 green)) ((2 red) (1 blue) (1 green)) ((4 red) (4 green) (2 blue)) ((3 blue) (6 green)))
(13 ((9 red) (2 blue) (2 green)) ((1 blue) (2 red) (15 green)) ((9 green) (2 blue) (9 red)) ((5 blue) (8 green) (5 red)) ((2 blue) (11 green) (5 red)))
(14 ((9 blue) (1 red)) ((10 blue) (4 green) (3 red)) ((2 red) (6 blue)) ((4 green) (2 blue) (1 red)) ((5 green) (2 red) (11 blue)) ((12 blue) (2 red) (1 green)))
(15 ((9 blue) (7 green) (12 red)) ((9 red) (17 green) (8 blue)) ((6 red) (4 blue) (4 green)) ((5 red) (17 green)))
(16 ((5 green) (4 red)) ((3 blue) (3 red) (14 green)) ((6 red) (5 blue) (12 green)))
(17 ((8 blue) (5 green) (2 red)) ((6 red) (6 blue)) ((9 red)) ((5 blue) (2 green) (8 red)) ((13 red) (4 blue) (4 green)) ((9 blue) (3 green) (5 red)))
(18 ((8 green) (1 red) (2 blue)) ((4 green) (4 red) (1 blue)) ((6 blue) (2 red)))
(19 ((3 green) (9 blue)) ((4 blue) (10 red)) ((6 red) (3 green) (3 blue)) ((6 red) (4 green) (9 blue)))
(20 ((11 green) (3 blue)) ((6 green)) ((3 green) (6 blue)) ((1 red) (5 green)) ((6 blue) (7 green)))
(21 ((1 green) (1 blue) (12 red)) ((6 red) (2 blue)) ((5 green) (4 red) (2 blue)) ((11 red) (8 green) (1 blue)))
(22 ((10 red)) ((1 red) (13 green) (9 blue)) ((6 blue) (12 red) (12 green)) ((10 red) (8 blue) (11 green)) ((2 green) (1 red) (3 blue)) ((7 red) (1 blue) (8 green)))
(23 ((11 red) (15 blue)) ((10 blue) (16 red) (1 green)) ((14 blue) (5 red)) ((1 green) (9 red) (9 blue)) ((1 red) (7 blue) (3 green)) ((6 red) (2 green) (3 blue)))
(24 ((6 blue) (11 red)) ((16 green) (2 red) (1 blue)) ((8 red) (7 blue)) ((14 blue) (9 green) (9 red)) ((13 green) (4 red) (8 blue)) ((2 red) (7 blue) (1 green)))
(25 ((2 green) (12 blue) (1 red)) ((10 blue) (5 red) (5 green)) ((2 blue) (9 red) (3 green)) ((5 blue) (4 red) (2 green)))
(26 ((7 blue) (6 red) (1 green)) ((2 blue) (3 green) (12 red)) ((2 blue) (6 red) (5 green)))
(27 ((2 green) (3 red)) ((4 green)) ((2 red) (1 blue) (1 green)) ((2 red) (1 green) (2 blue)))
(28 ((11 blue) (1 red) (5 green)) ((2 blue) (2 red) (4 green)) ((10 blue) (4 red) (1 green)))
(29 ((6 blue) (17 red) (1 green)) ((8 blue) (4 red)) ((14 blue) (1 red) (3 green)))
(30 ((2 blue) (4 green)) ((7 green) (1 blue) (1 red)) ((1 blue) (8 green)))
(31 ((15 blue) (9 green) (2 red)) ((5 green) (4 blue) (1 red)) ((1 green) (15 red) (7 blue)) ((5 red) (2 blue)))
(32 ((1 blue) (5 red) (3 green)) ((3 green) (8 red) (1 blue)) ((5 green) (1 red)) ((4 green) (3 blue) (15 red)) ((2 green) (1 blue)) ((4 blue) (15 red) (4 green)))
(33 ((3 red) (10 blue)) ((4 red) (9 blue)) ((1 green) (10 blue)))
(34 ((3 blue) (1 green) (9 red)) ((4 green) (2 red) (9 blue)) ((7 blue) (3 red)) ((6 blue) (13 red)) ((4 green) (13 blue) (9 red)))
(35 ((14 red) (1 green)) ((1 red) (2 green) (4 blue)) ((3 blue) (10 red) (6 green)) ((5 blue) (6 red) (7 green)) ((7 blue) (5 red)))
(36 ((2 blue) (8 red) (9 green)) ((9 green) (3 red) (10 blue)) ((6 red) (8 blue) (1 green)) ((6 green) (8 red) (4 blue)))
(37 ((10 green) (3 red) (6 blue)) ((2 blue) (9 red) (5 green)) ((13 green) (9 red) (10 blue)) ((2 blue) (4 green) (9 red)))
(38 ((4 red) (14 blue) (12 green)) ((6 red) (12 green) (18 blue)) ((6 green) (1 blue) (1 red)))
(39 ((5 red) (1 blue) (3 green)) ((1 blue) (3 green) (8 red)) ((15 red) (1 blue) (5 green)) ((3 green) (5 red)) ((1 blue) (14 red)) ((3 green) (1 blue) (12 red)))
(40 ((8 green) (4 blue)) ((5 blue) (7 red) (8 green)) ((5 blue) (8 green)) ((6 green) (3 red) (12 blue)) ((14 blue) (7 green) (2 red)) ((1 green) (7 red) (5 blue)))
(41 ((7 red) (10 green)) ((10 red) (6 green)) ((9 red) (7 green) (1 blue)) ((3 red) (1 blue)))
(42 ((3 green) (2 blue) (13 red)) ((1 blue) (3 red)) ((11 green) (16 red)) ((3 green) (1 blue) (16 red)) ((5 red) (8 green)))
(43 ((12 blue) (9 red)) ((16 blue) (2 red) (7 green)) ((4 red) (1 blue) (11 green)) ((15 blue) (4 red) (9 green)))
(44 ((17 green) (5 blue) (2 red)) ((9 green) (11 blue) (1 red)) ((20 green) (3 blue) (8 red)) ((2 red) (13 green) (9 blue)) ((15 green) (12 blue)) ((4 blue) (7 green) (9 red)))
(45 ((5 green)) ((5 green) (1 red)) ((3 green) (2 blue)) ((1 green) (1 blue) (1 red)))
(46 ((10 red) (11 green)) ((16 green) (8 blue) (12 red)) ((9 green) (9 blue)))
(47 ((20 green) (17 red) (1 blue)) ((16 red) (2 blue) (11 green)) ((3 blue) (19 red) (1 green)) ((3 blue) (17 red) (17 green)) ((12 green) (2 blue) (7 red)))
(48 ((1 red) (4 blue) (6 green)) ((19 green) (1 red) (1 blue)) ((16 green) (3 blue) (1 red)) ((3 blue) (17 green)) ((4 blue) (12 green)))
(49 ((13 green) (2 blue) (1 red)) ((1 green) (8 red) (2 blue)) ((11 red) (11 green) (3 blue)) ((7 red) (8 green) (4 blue)))
(50 ((11 blue) (1 red) (2 green)) ((1 green) (10 blue)) ((1 blue)) ((6 blue)) ((1 green) (2 blue)))
(51 ((3 red) (3 green) (1 blue)) ((3 green) (3 red)) ((10 green) (4 red)) ((3 red) (2 green)))
(52 ((1 red) (4 blue)) ((1 green) (11 blue)) ((1 green) (3 red) (6 blue)) ((4 red) (1 green) (4 blue)) ((9 blue) (1 green)) ((10 blue) (1 green)))
(53 ((2 blue) (4 green) (1 red)) ((8 blue) (4 red) (7 green)) ((9 red) (7 blue) (6 green)) ((3 red) (7 green) (1 blue)) ((2 red) (9 blue) (5 green)) ((1 green) (7 red) (10 blue)))
(54 ((1 red) (1 blue) (5 green)) ((2 red) (1 green) (2 blue)) ((3 green) (3 blue) (2 red)) ((4 red)) ((12 red) (5 green) (2 blue)))
(55 ((2 red) (11 blue)) ((16 green) (7 red) (16 blue)) ((4 blue) (11 green) (7 red)) ((8 green) (18 blue) (8 red)))
(56 ((2 blue) (2 green) (1 red)) ((1 red) (1 green)) ((1 red)) ((4 green)) ((1 blue)) ((1 blue) (7 green)))
(57 ((4 blue) (3 green)) ((16 green) (2 red) (5 blue)) ((1 red) (13 green) (2 blue)) ((3 blue) (12 green) (2 red)) ((2 red) (5 blue) (4 green)) ((10 green) (2 blue)))
(58 ((3 blue) (8 green)) ((4 green) (3 blue)) ((7 green) (5 blue) (5 red)) ((8 green)) ((3 red) (6 blue) (9 green)) ((2 red) (10 green) (4 blue)))
(59 ((7 blue) (6 green) (5 red)) ((7 red) (2 blue)) ((5 red) (11 green) (14 blue)) ((8 green) (17 red)))
(60 ((3 green) (8 blue) (2 red)) ((4 green) (7 blue) (6 red)) ((13 blue) (8 green) (2 red)) ((10 red) (6 blue) (5 green)) ((11 green) (3 blue) (4 red)) ((9 red) (5 green) (9 blue)))
(61 ((4 red) (18 blue) (13 green)) ((9 green) (5 red) (3 blue)) ((4 green) (3 blue) (4 red)) ((8 red) (4 green) (7 blue)) ((8 red) (4 blue) (6 green)) ((10 green) (5 red) (14 blue)))
(62 ((12 red) (14 blue) (9 green)) ((9 blue) (6 red) (4 green)) ((2 red) (5 blue)) ((1 red) (12 blue)))
(63 ((11 blue) (13 red) (11 green)) ((4 blue) (9 green)) ((8 blue) (9 red)) ((7 red) (11 green) (7 blue)))
(64 ((10 blue) (8 red) (12 green)) ((10 red) (12 blue) (9 green)) ((3 green) (17 red)) ((12 green) (15 blue) (16 red)) ((6 green) (15 blue) (1 red)) ((9 red) (6 blue) (10 green)))
(65 ((7 red) (7 blue)) ((3 blue) (1 red) (1 green)) ((3 red) (8 blue)))
(66 ((1 blue) (3 red)) ((10 green) (5 blue)) ((4 green)) ((3 red) (11 green)) ((3 blue) (15 green) (3 red)))
(67 ((1 red)) ((2 blue) (2 green) (1 red)) ((6 green) (1 blue)))
(68 ((7 red) (4 blue)) ((4 blue) (6 red) (7 green)) ((2 green) (19 red) (11 blue)) ((11 green) (9 red)))
(69 ((4 blue) (3 green) (1 red)) ((7 blue) (1 red) (3 green)) ((5 blue) (1 green)) ((2 blue) (10 green) (2 red)) ((2 red) (6 green) (5 blue)) ((1 red) (4 green) (2 blue)))
(70 ((9 blue) (7 red) (6 green)) ((19 blue) (4 red) (5 green)) ((6 blue) (7 red) (4 green)) ((3 blue) (4 red) (2 green)))
(71 ((6 green) (12 blue) (4 red)) ((11 red) (10 green) (11 blue)) ((3 red) (14 blue) (13 green)) ((4 blue) (3 green)))
(72 ((2 green) (1 blue) (9 red)) ((10 red) (3 green) (1 blue)) ((11 red) (2 green)) ((2 green) (1 blue) (5 red)) ((1 red) (1 blue) (3 green)) ((13 red) (4 blue) (1 green)))
(73 ((11 green) (6 blue)) ((7 green) (6 blue) (7 red)) ((12 green) (8 blue) (11 red)) ((4 red) (2 blue) (9 green)) ((4 green) (7 blue) (2 red)))
(74 ((3 blue) (7 red)) ((3 blue) (5 green) (2 red)) ((5 red) (1 green) (3 blue)) ((8 green) (2 blue) (11 red)) ((3 blue) (8 green) (10 red)))
(75 ((2 green)) ((5 blue)) ((1 blue) (1 red)) ((1 red) (9 blue) (2 green)) ((2 blue) (2 green)))
(76 ((12 blue) (13 green)) ((5 red) (11 blue) (9 green)) ((12 green) (6 red)))
(77 ((1 blue) (15 green) (12 red)) ((15 green) (5 blue)) ((14 green) (3 blue) (8 red)))
(78 ((11 green) (8 blue) (1 red)) ((9 green) (8 blue) (1 red)) ((13 green) (5 red) (6 blue)) ((5 red) (7 green) (20 blue)) ((10 blue) (5 red)))
(79 ((3 blue)) ((6 blue) (5 red)) ((4 red) (1 green) (4 blue)) ((7 blue) (6 red)) ((7 red) (1 blue)) ((1 red) (1 blue) (1 green)))
(80 ((11 green) (3 red) (8 blue)) ((2 red) (15 green) (2 blue)) ((5 green) (8 blue) (2 red)) ((8 blue) (14 green)) ((2 blue) (13 green)))
(81 ((9 red) (4 green)) ((7 green) (4 red)) ((2 red) (4 blue) (6 green)) ((6 red) (4 blue) (9 green)) ((1 green) (3 red)) ((6 green) (1 blue) (8 red)))
(82 ((5 blue) (3 red) (3 green)) ((5 red)) ((2 red) (3 green) (8 blue)))
(83 ((10 green) (1 red) (1 blue)) ((3 red) (1 green) (1 blue)) ((4 red) (10 green)))
(84 ((16 red) (2 green) (6 blue)) ((6 red) (3 green) (8 blue)) ((3 green) (10 red) (5 blue)) ((4 blue) (3 green)) ((15 red)))
(85 ((3 green) (2 red)) ((5 green) (4 blue)) ((5 green) (8 red) (3 blue)))
(86 ((7 green) (16 blue) (7 red)) ((1 green) (12 red) (2 blue)) ((15 green) (16 blue) (7 red)))
(87 ((1 red) (6 green) (5 blue)) ((2 green) (1 blue)) ((2 green) (1 red) (1 blue)) ((5 green) (4 blue)))
(88 ((3 green) (3 red) (4 blue)) ((1 red) (1 green)) ((6 blue) (9 red) (1 green)) ((1 green) (11 red) (3 blue)) ((7 red) (6 blue)))
(89 ((2 blue) (3 red) (4 green)) ((5 red) (7 blue) (14 green)) ((8 blue) (5 red) (16 green)) ((2 blue) (5 red) (7 green)) ((5 green) (9 blue) (1 red)))
(90 ((1 blue) (3 red) (7 green)) ((11 green) (4 red) (1 blue)) ((1 red) (1 blue) (6 green)) ((2 blue) (2 green)) ((8 green) (2 blue)) ((3 red) (2 blue) (4 green)))
(91 ((6 blue) (4 red) (1 green)) ((8 red) (3 blue) (3 green)) ((1 green) (2 blue) (5 red)) ((1 blue) (3 green)))
(92 ((8 green) (1 red) (5 blue)) ((2 green) (7 blue)) ((11 blue) (5 green) (8 red)) ((7 blue) (3 red) (4 green)))
(93 ((3 green) (1 red) (9 blue)) ((13 red) (5 blue) (8 green)) ((5 green) (2 red) (7 blue)))
(94 ((4 green) (10 blue) (8 red)) ((4 red) (10 blue) (2 green)) ((2 green) (10 blue) (5 red)) ((5 green) (2 red) (10 blue)))
(95 ((5 green) (1 blue)) ((3 blue) (11 green) (8 red)) ((8 blue) (2 red) (12 green)) ((4 green) (4 blue) (4 red)))
(96 ((1 blue) (13 green)) ((8 blue) (3 red) (4 green)) ((1 red) (3 blue) (10 green)))
(97 ((18 green) (4 red)) ((1 blue) (2 red) (9 green)) ((6 red) (3 blue) (10 green)) ((3 blue) (15 green) (4 red)))
(98 ((2 blue) (3 green) (6 red)) ((1 green) (1 blue) (8 red)) ((8 red) (3 green) (1 blue)) ((2 blue)) ((8 red) (2 green) (2 blue)))
(99 ((1 green) (2 red) (1 blue)) ((8 green) (4 blue) (1 red)) ((7 blue) (1 red) (11 green)) ((9 green) (3 blue)) ((1 red) (2 blue)) ((1 red) (6 blue)))
(100 ((7 blue) (9 green) (2 red)) ((5 red) (9 green)) ((1 blue) (8 red) (13 green)))
  ))

;; Solve example1
(printf "~a~n" (solve1 EXAMPLE1))
(printf "~a~n" (solve1 INPUT1))

(printf "~a~n" (solve2 EXAMPLE1))
(printf "~a~n" (solve2 INPUT1))
