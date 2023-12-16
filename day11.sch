;; Image of space with empty space (x) and galaxies (G).
(define EXAMPLE
  '((x x x G x x x x x x)
    (x x x x x x x G x x)
    (G x x x x x x x x x)
    (x x x x x x x x x x)
    (x x x x x x G x x x)
    (x G x x x x x x x x)
    (x x x x x x x x x G)
    (x x x x x x x x x x)
    (x x x x x x x G x x)
    (G x x x G x x x x x)))

;; Produces the sum of the shortest-distances between each galaxy, expanded as appropriate.
(define solve
  (lambda (input expand-by)
    (let-values ([(galaxies starmap) (build-map (expand-map-by input expand-by))])
      (apply + 0 (shortest-distances galaxies starmap)))))

;; Produces a galaxy where each empty row is repeated expand-by times.
(define expand-rows
  (lambda (input expand-by)
    (let f ([i input] [acc '()])
      (cond
       [(null? i) acc]
       [(empty? (car i)) (f (cdr i) (cons (map (lambda (x) expand-by) (car i)) acc))]
       [else (f (cdr i) (cons (car i) acc))]))))

;; Produces a galaxy where each empty column is repeated expand-by times.
;; Does this by building a list out of the column and producing a new map, rotated 90 degrees.
(define expand-columns
  (lambda (input expand-by)
    (let f ([i input] [acc '()])
      (if (null? (car i))
          acc
          (let ([column (map car i)])
            (if (empty? column)
                (f (map cdr i) (cons (map (lambda (x) expand-by) column) acc))
                (f (map cdr i) (cons column acc))))))))

;; Produces an expanded galaxy, where each empty row is replaced by the number provided in
;; expand-by.
(define expand-map-by
  (lambda (input expand-by)
    (let ([rows-expanded (expand-rows input expand-by)])
      (expand-columns rows-expanded expand-by))))

;; Produces #t if all members of ls are empty space (x).
(define empty?
  (lambda (ls)
    (cond
     [(null? ls) #t]
     [(eq? (car ls) 'G) #f]
     [else (empty? (cdr ls))])))

;; Builds a list of galaxy (x . y) locations, and a hashtable of (x . y) coordinates to the symbol
;; at that location.
(define build-map
  (lambda (input)
    (let ([coords (make-hashtable equal-hash equal?)])
      (let f ([row input] [y 0] [galaxies '()])
        (if (null? row)
            (values galaxies coords)
            (let g ([col (car row)] [x 0] [galaxies galaxies])
              (if (null? col)
                  (f (cdr row) (+ y 1) galaxies)
                  (begin
                    (hashtable-set! coords (cons x y) (car col))
                    (g (cdr col) (+ x 1) (if (eq? (car col) 'G)
                                             (cons (cons x y) galaxies)
                                             galaxies))))))))))

;; Produces a list of the shortest distances between each pair of galaxies.
(define shortest-distances
  (lambda (galaxies starmap)
    (let f ([g galaxies] [acc '()])
      (if (null? g)
          acc
          (f (cdr g)
             (append acc (map (lambda (g*) (shortest-distance (car g) g* starmap)) (cdr g))))))))

;; Calculate the Manhattan distance between g1 and g2, using starmap to assess distances.
(define shortest-distance
  (lambda (g1 g2 starmap)
    (let ([x-moves (- (car g2) (car g1))]
          [y-moves (- (cdr g2) (cdr g1))])
      (+ (traverse g1 x-moves starmap
                   (lambda (curr delta) (cons (+ (car curr) delta) (cdr curr))))
         (traverse g1 y-moves starmap
                   (lambda (curr delta) (cons (car curr) (+ (cdr curr) delta))))))))

;; Calculates the distance from start to the end of moves, using starmap to gauge distances.
;; getter is used to get the dimension we are moving in.
(define traverse
  (lambda (start moves starmap get-next-loc)
    (if (zero? moves)
        0
        (let ([delta (/ moves (abs moves))])
          (let f ([curr (get-next-loc start 0)] [left moves] [dist 0])
            (if (zero? left)
                dist
                (let* ([next-loc (get-next-loc curr delta)]
                       [next-sym (hashtable-ref starmap next-loc 'x)])
                  (if (number? next-sym)
                      (f next-loc (- left delta) (+ dist next-sym))
                      (f next-loc (- left delta) (+ dist 1))))))))))

(printf "~a~n" (solve EXAMPLE 2))
(printf "~a~n" (solve EXAMPLE 100))

(define INPUT
  '(
    (x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x)
    (x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x)
    (x x x x x x x x G x x x x x x x x x x x x x x x x x x x G x x x x x x x x G x x x x x x G x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x)
    (x G x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x G x x x x x x G x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x)
    (x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x G x x x x)
    (x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x G x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x x x x G x x x x x)
    (x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x G x x)
    (x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x)
    (x G x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x G x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x)
    (x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x G x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x G x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x G x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x G x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x)
    (x x x x x x G x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x G x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x G x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x G x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x G x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x G)
    (x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x G x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x G x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x G x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x G x x x x x x G x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x G x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x G x x x x x x x x G x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x G x x x x x G x x x x x)
    (x G x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x G x x x x x x x x x x x G x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (G x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x G x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x)
    (x x x x x x x x x x x x x x x x G x x x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x G x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x G x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x)
    (x x x G x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x G x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x G x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x G x x x x x x)
    (x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x G x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x G x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (G x x x x x x G x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x)
    (x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x G x x x x x G x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x G x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x)
    (x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x G x x x x x x x x x G x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G)
    (x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x G x x x x x x x x)
    (G x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x G x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x G x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x G x x x x x x x x x x x x G x)
    (x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)
    (x x x x x x x x x x x G x x x x x x x G x x x x x x x x x x x x x x x x x x x x x G x x x x x x x x x x x G x x x x x x x x x G x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x G x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x)))

(printf "~a~n" (solve INPUT 2))
(printf "~a~n" (solve INPUT 1000000))
