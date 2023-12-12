;; A map of pipework
;; I is a vertical pipe connecting north and south.
;; - is a horizontal pipe connecting east and west.
;; L is a 90-degree bend connecting north and east.
;; J is a 90-degree bend connecting north and west.
;; 7 is a 90-degree bend connecting south and west.
;; F is a 90-degree bend connecting south and east.
;; x is ground; there is no pipe in this tile.
;; S is the starting position of the animal; there is a pipe on this tile, but your sketch doesn't show what shape the pipe has.
(define EXAMPLE1
  '((7 - F 7 -)
    (x F J I 7)
    (S J L L 7)
    (I F - - J)
    (L J x L J)))

(define EXAMPLE2
  '((x x x x x)
    (x S - 7 x)
    (x I x I x)
    (x L - J x)
    (x x x x x)))

(define EXAMPLE3
  '((F F 7 F S F 7 F 7 F 7 F 7 F 7 F - - - 7)
    (L I L J I I I I I I I I I I I I F - - J)
    (F L - 7 L J L J I I I I I I L J L - 7 7)
    (F - - J F - - 7 I I L J L J 7 F 7 F J -)
    (L - - - J F - J L J x I I - F J L J J 7)
    (I F I F - J F - - - 7 F 7 - L 7 L I 7 I)
    (I F F J F 7 L 7 F - J F 7 I J L - - - 7)
    (7 - L - J L 7 I I F 7 I L 7 F - 7 F 7 I)
    (L x L 7 L F J I I I I I F J L 7 I I L J)
    (L 7 J L J L - J L J L J L - - J L J x L)))

;; Gets a valid next loc based on loc's symbol.
(define get-next
  (lambda (table loc)
    (let* ([sym (hashtable-ref table loc #f)]
           [options
            (cond
             [(eq? sym 'I) `((,(car loc) . ,(- (cdr loc) 1)) . ; north
                             (,(car loc) . ,(+ (cdr loc) 1)))] ; south
             [(eq? sym '-) `((,(- (car loc) 1) . ,(cdr loc)) . ; west
                             (,(+ (car loc) 1) . ,(cdr loc)))] ; east
             [(eq? sym 'L) `((,(car loc) . ,(- (cdr loc) 1)) . ; north
                             (,(+ (car loc) 1) . ,(cdr loc)))] ; east
             [(eq? sym 'J) `((,(car loc) . ,(- (cdr loc) 1)) . ; north
                             (,(- (car loc) 1) . ,(cdr loc)))] ; west
             [(eq? sym '7) `((,(car loc) . ,(+ (cdr loc) 1)) . ; south
                             (,(- (car loc) 1) . ,(cdr loc)))] ; west
             [(eq? sym 'F) `((,(car loc) . ,(+ (cdr loc) 1)) . ; south
                             (,(+ (car loc) 1) . ,(cdr loc)))] ; east
             [else #f])] ; shouldn't happen
           [first (hashtable-ref table (car options) 'x)]
           [second (hashtable-ref table (cdr options) 'x)])
      (if (eq? first 'x)
          (cdr options)
          (car options)))))

;; Finds the next step forward from path, incrementing the count and burning where we've been.
;; path is a pair of ((x . y) . <steps>)
(define step
  (lambda (table path)
    (let ([next (cons (get-next table (car path)) (+ (cdr path) 1))])
      (hashtable-set! table (car path) 'x)
      next)))

;; Produce a list of the four locations in the cardinal directions from loc.
(define cardinals
  (lambda (loc)
    (let ([x (car loc)] [y (cdr loc)])
      `((,x . ,(- y 1))
        (,(+ x 1) . ,y)
        (,x . ,(+ y 1))
        (,(- x 1) . ,y)))))

;; Tests that a loc in table is not 'x.
(define not-x?
  (lambda (table loc)
    (not (eq? (hashtable-ref table loc 'x) 'x))))

;; Produces #t if loc2 contains a pipe type that can traverse to loc1.
(define connected?
  (lambda (table loc1 loc2)
    (let ([pipe (hashtable-ref table loc2 'x)]
          [x-diff (- (car loc2) (car loc1))]
          [y-diff (- (cdr loc2) (cdr loc1))])
      (and (not (eq? pipe 'x))
           (not (and (zero? x-diff) (zero? y-diff)))
           (member pipe (cond
                         [(> x-diff 0) '(- J 7)]
                         [(< x-diff 0) '(- L F)]
                         [(> y-diff 0) '(I L J)]
                         [(< y-diff 0) '(I 7 F)]))))))

;; Finds the two possible directions to start moving in from S, producing a pair of locs.
(define get-heads
  (lambda (table start)
    (filter (lambda (loc) (and (not-x? table loc) (connected? table start loc))) (cardinals start))))

;; Starting from S, walk table in both directions, building a list of step counts until we hit each
;; other.
(define steps
  (lambda (table)
    (let* ([start (hashtable-ref table 'S #f)]
           [heads (get-heads table start)])
      (hashtable-set! table start 'B) ; Tear down so we can't go back accidentally
      (let f ([path-a (cons (car heads) 1)] [path-b (cons (cadr heads) 1)])
        (if (equal? (car path-a) (car path-b)) (max (cdr path-a) (cdr path-b))
            (f (step table path-a) (step table path-b)))))))

;; Turns list list-table into a hashtable mapping pairs of coordinates to symbols.
(define build-table
  (lambda (list-table)
    (let ([table (make-hashtable equal-hash equal?)])
      (let f ([y 0] [l list-table])
        (if (null? l) table
            (begin
              (let g ([x 0] [ll (car l)])
                (if (null? ll) #f
                    (begin
                      (if (eq? (car ll) 'S)
                          (hashtable-set! table 'S (cons x y)) ; For ease of S-lookup
                          #f)
                      (hashtable-set! table (cons x y) (car ll))
                      (g (+ x 1) (cdr ll)))))
              (f (+ y 1) (cdr l))))))))

;; Finds the furthest part of the main loop, in steps, from the start of the pipe in input.
(define solve1
  (lambda (input)
    (steps (build-table input))))

;; Produces a list of all the coordinates that make up the loop.
(define build-loop
  (lambda (table)
    (let* ([start (hashtable-ref table 'S #f)]
           [heads (get-heads table start)]
           [head (car heads)]
           [tail (cadr heads)])
      (hashtable-set! table start 'x)
      (let f ([curr (cons head 1)] [loop (cons start '())])
        (if (equal? (car curr) tail) loop
            (f (step table curr) (cons (car curr) loop)))))))

;; Counts the number of internal locations by iterating across the grid and keeping track of
;; whether we are inside or outside of the loop.
(define solve2
  (lambda (input)
    (count-internal input (loop->set (build-loop (build-table input))))))

;; Produces a hashtable (to use as a hashset) for what locations are members of the loop.
(define loop->set
  (lambda (loop)
    (let ([set (make-hashtable equal-hash equal?)])
      (for-each (lambda (l) (hashtable-set! set l #t)) loop)
      set)))

;; Counts internal locations by iterating across the grid and keeping track of whether we are
;; inside or outside of the loop.
(define count-internal
  (lambda (input set)
    (let f ([rows input] [y 0] [acc 0])
      (if (null? rows) acc
          (let g ([cols (car rows)] [x 0] [prev 'x] [inside #f] [acc acc])
            (if (null? cols) (f (cdr rows) (+ y 1) acc)
                (let-values ([(prev inside acc) (count-loc set (car cols) prev (cons x y) inside acc)])
                  (g (cdr cols) (+ x 1) prev inside acc))))))))

;; If loc is a member of the loop, transitions from inside to outside or vice-versa, as determined
;; by the previous loop boundary transition made.
;; If loc is not a member of the loop, increments acc if we are inside. Otherwise, does nothing.
(define count-loc
  (lambda (set sym prev loc inside acc)
    (cond
     [(hashtable-contains? set loc) (values (if (member sym '(7 I L J F)) sym prev) (transition sym prev inside) acc)]
     [inside (values prev inside (+ acc 1))]
     [else (values prev inside acc)])))

;; Transitions from inside to outside or vice-version, based on some simple transition rules.
(define transition
  (lambda (sym prev inside)
    (cond
     [(member sym '(I L F)) (not inside)]
     [(and (eq? prev 'L) (eq? sym 'J)) (not inside)]
     [(and (eq? prev 'F) (eq? sym '7)) (not inside)]
     [else inside])))

(define INPUT
  '((x L 7 F - - I - 7 7 F L F 7 - F - F L L x F L 7 I 7 F L 7 - 7 x 7 - F J 7 F J - F x I 7 - x F I - I F - J F J F L - J - x F I x 7 F - - 7 7 I 7 7 F 7 F I J 7 7 I - I 7 I - F - 7 x I J F L 7 - 7 x J - - - J x F 7 - - x 7 F I - - L J F L - J x F F F F x - L I - - - 7 7 7 7 F 7 - I)
    (J F - I x I x I - 7 J x 7 - F I - J J 7 F x J 7 L - I J L L - 7 J x J F - 7 x 7 I - F 7 x F F J - - J x L 7 7 F J 7 J x F - - L x L J J F F J L J F - J I I J L I I x F J 7 L - x F 7 I I J L - F F J L x F I F L J L L F L I I I J I L I 7 7 L F 7 x I - - I I I x I L L J L J J x F 7)
    (L L J F 7 F - L x 7 F - J - L J I x F F 7 7 L L F 7 I I F F - - 7 L - 7 7 x J - J 7 J J x I x L 7 F L I x F 7 J F 7 7 F - 7 x L - 7 L 7 J L F 7 x L 7 L L 7 x L I 7 F F L - - J x L 7 - F - L - F J 7 I 7 F F J J - F J I x J 7 7 x J F 7 F L x - J L F J - L I 7 F J F J 7 F F - 7 J J)
    (L I - I J I F - 7 x 7 I x 7 - L - 7 L F J I 7 L I I J F F I J F 7 J I L - 7 J 7 I I I x x I 7 I L I J x x I I J x x F 7 x I F I x - - J 7 - L L 7 - J F L I F 7 x L J 7 J J L - - - J I I J I x L 7 F 7 - - I J L F J 7 J 7 I J I 7 x 7 J - I - - J x x L 7 F - I L F I J J I I 7 L - 7)
    (7 x L J x L J L J - 7 J J - J I - J 7 I I L J F I I F J - L - F L - 7 L J 7 7 F I x L J x I - I I J x F F x L 7 L - I x 7 - I 7 7 x I x x 7 J L - - J L - I - J x J F - - 7 J F J I L 7 J F F - I - I I 7 7 F F - - - 7 F I 7 - - L - L I x I 7 - F - 7 F F J L L - J I J x F J 7 I F L)
    (L 7 x I - J J I I J I L L I - I 7 x 7 J 7 - F F 7 L I I I - - I F I - 7 J L F - J 7 x x F I x L 7 7 F - F 7 L 7 L - - - J - 7 J I - F 7 F L I I L L F x x I F I - J F I F F - - 7 - 7 J 7 F J L I F I I F 7 J L 7 F - J - J x L I J x J x L L - - J x L - J J x I x L - x F I - - - F J)
    (L L - 7 7 x x F F - I 7 I F - F - I I F 7 7 7 J I L L F J I F L J F J x - 7 J J L L 7 7 7 I - F L J x L I I - J F I - I 7 7 I x F 7 I L 7 - 7 7 x I F - F 7 7 J J x I F 7 L 7 F J x x x - F - 7 J F I L J I F - J L - 7 x I F - 7 x I J F x x L F 7 I - 7 J x F I x L J F I J J J L F 7)
    (L L - 7 - L 7 L J J J - F 7 x J F L I - I L 7 F I - 7 L 7 - F 7 7 I L L J I F I I F L 7 7 L x L 7 J 7 F I I L I F - - I - L J - I L J F J L - 7 L I J - I I - J x - F J L 7 I L - 7 - F I I F J F 7 L 7 F J L - 7 F - J 7 7 L 7 J I I F I - 7 x L J J x J L F - L 7 J - F I J x x 7 I I)
    (x x L L x I L F I L I x - J 7 L L 7 I I I F F J - - I x F F 7 J L I x F 7 - J J F I L L I F - x F 7 - F J I 7 L F x x L F J F L L 7 F J J L F - J I F - I I J - J 7 L - 7 I I F - J J I L I L 7 I I - I I F 7 I I L 7 I L 7 I F 7 - F F J I F - I 7 J x J 7 L J L I J x L L - F I F F x)
    (F I x I F I 7 x I x 7 - I I I - x F L J 7 L 7 - I F J 7 F L J I 7 L J - L - I F 7 J 7 - L I x F F J L L 7 I - F - 7 7 F - - I I - I I 7 7 F L J L L I x I I x x L F - - J L J L - 7 x J I L 7 I I I F J L J L - J F J 7 - F 7 - L 7 7 J F I 7 7 L F F 7 L L 7 7 x L - F I I x F L - J F)
    (- F J 7 7 L J - F x - x 7 F I L J 7 - F I F x F F 7 J F J F I F I 7 J F I I F I I x F x F I - F - 7 F 7 I I F I F J F J x x F 7 F J L - - 7 J F 7 - I F I L 7 J L L 7 F 7 F 7 F - J x L F F J I I I L - - 7 F - - J F 7 L I I J x L 7 - L x I I F - L I x L F - J J x L L - F I J L 7 L)
    (L I J L I J I F J 7 x F x F J F L x F L L J F F J I 7 I J F F 7 F 7 F - 7 - F J I F 7 F 7 L L L 7 I I I I I F J I L F x F F J L J F - - - J x I I - L F J F J L F 7 I I L J I L - - 7 7 L L 7 L J L 7 F 7 I L - 7 F J I F J L - 7 x I I 7 F I J - I L - 7 F F J I L 7 - F J I J 7 x I x)
    (7 - 7 - 7 F - 7 F J - - 7 I I F I L J L L I J L 7 L 7 7 F 7 I L J I L 7 I L L 7 I I I F 7 - F 7 I I I I I L J F J F - - 7 L - - 7 I F - - 7 F 7 F - 7 L 7 I F 7 I I L J F - J F - - J F - 7 L 7 F - J I I I F - J I F J I F - - J 7 7 J 7 I I - L 7 7 L F L J F - - F F J x - x F F x -)
    (I L 7 - L 7 L - J J 7 x I - F - 7 7 7 x x L 7 F L 7 L - J L J F - J F J L 7 F J I I I I I I I I I L J L J F - J L I F - J F - - J L J F - J I I L 7 I 7 I I I I I I F 7 L - 7 L - 7 x L 7 I F J L 7 F J I I I F - J I F J L - - - - 7 - 7 7 7 7 F - 7 - L J F L J - I J J - L J 7 I x x)
    (F L x x I J F J J x J 7 L 7 L J - 7 J 7 F 7 J - x L - - - - 7 L - 7 I F - J L 7 L J I I L - J I L - - 7 F J F - 7 I L - 7 L - 7 F 7 F J F 7 I I I I L 7 I I I L J I I L - 7 L 7 F J F 7 I I L - 7 I L 7 L J I L 7 F J I F - - - - - J L F J I - 7 L J I I - L 7 x x I L 7 F - L 7 - x F)
    (7 x F J I L 7 L - L J L F F J J J x F I x L J L F F - - - - J F - J I I F 7 7 L - 7 I L - 7 F J F 7 - I I x L 7 I I F 7 L - 7 I I I I J I I I L 7 L 7 I I I L - 7 I I F 7 L - J L - J I I L 7 F J L 7 I F 7 I F J L - J L - - - - - - 7 J F J 7 J I x F J x L F x F I I L J I L L 7 7 I)
    (- I - - F J F L - J J F L J L - F 7 7 I 7 I L - - L - - - - 7 L - 7 I L J I F - 7 I L - 7 I I F J L 7 I L 7 F J I L J I F - J L J I L 7 I I L 7 I F J L J I F - J I L J I F - - - 7 F J I F J L - 7 L J I I I I F - - - - 7 F - - - - J 7 - J F F 7 F J F L - - 7 7 x F 7 F F L L - J J)
    (L 7 I F J J F F L 7 I x I - 7 - J I F I 7 x I I x F 7 F 7 L L - 7 I I F - J I F J I F 7 L J I L 7 F J I F J I F J F - J L 7 J - F J F J I L 7 I I L 7 F 7 L J F 7 I F - J L - - 7 L J F J L 7 F 7 L 7 F J L J I L - 7 F 7 L J F - - 7 - I - F J F - 7 J J - F L - I 7 F I 7 7 - I J x F)
    (x x I L I - - L L - - 7 I x L - J F I J L F F 7 - I I I I F 7 L I I I L 7 F J L 7 L J L - 7 I F J I F J I x I L 7 L - - 7 I F - J F J x L 7 I I I F J I L 7 F J L J L - - 7 F 7 L 7 x L 7 F J I L - J I F 7 - I F - J I I F 7 L 7 F J F 7 F - 7 I F J J x L F J x I F - J F J x I F - 7)
    (F F 7 - 7 J I L I L F - J x F I - 7 J x F - J L - J L J L J I F J I L 7 I I F - J I F 7 F J I I F J L 7 L 7 L 7 L 7 F 7 I L J F - J F 7 F J L J I L - J x I L 7 F 7 F 7 F J I L - J F - J L 7 L - - 7 L J L - J L 7 x I I I L - J I x I I L 7 I I I I F - F 7 F J L J J 7 J 7 F L J I J)
    (J x I x I L J x - x - 7 L 7 F F 7 x F L L 7 F - - - - - - 7 I L 7 I F J L J I F 7 F J I L 7 I I I F 7 I F J - I F J I I L - 7 I F - J I L - - 7 L 7 F F 7 L 7 I I I I I L 7 I F 7 7 L - - 7 I F 7 F J F - - - - - J F J I I F - - J F J L - J L J L - 7 x I I - 7 F J x x x I x J L 7 7)
    (x F F 7 L - I 7 L I 7 I J F - I L F 7 x F I I F - - - - - J L 7 I I I F - - J I I I F J F J L J I I I I L - 7 I I J I I F 7 I I I F - J F - 7 L 7 I F J L - J L J I I I F J I I L 7 F 7 F J L J I I F J F F 7 J F 7 L 7 I I L 7 F 7 L - 7 F - - - - - J F J I L J I I x L 7 L J - F I 7)
    (x I L 7 F F 7 - F I 7 I F 7 - L 7 7 L - F L J L - - - - 7 F 7 L J I I I F 7 F J I I L 7 L 7 F - J I I I F - J I I F J I I L J I I I F - J F J L I I L - - 7 F - - J I I L 7 I I F J I I L - - 7 I I I - F J L 7 I L 7 I I I F J I I F 7 I L - 7 L F 7 L I F J 7 F F J x I J F - F J x I)
    (I J F 7 J I x x - F J F 7 - 7 7 I 7 F L L L F F 7 F - 7 I I L - 7 L J I I I I F J I F J F J I F 7 I I I L 7 F I I I F J L 7 F J I L J F - J F 7 I I F - - J L - 7 F J I F J I I L 7 I L 7 F - J I I L 7 I F 7 L J F J I I I L 7 I I I L J F 7 L - J L - J L - - 7 7 7 - 7 x F 7 L x L I)
    (L x I I I F x 7 L F J I L L I F L J J F F 7 F J I L 7 I L J F - J F 7 L J L J I x I L 7 L 7 I I L J I I F J F J I I I F 7 I I F J F 7 I F - J L J I L - - 7 F - J L 7 I I F J I F J I F J L 7 F J I F J L J I F - J F J I I F J I I L 7 F J L - - - - - - - - - J L - 7 I 7 I 7 L I J x)
    (L F L I - 7 7 L F F 7 I x I x I I F I F I I L 7 I F J I F 7 L - - J L - - 7 F J F J F J F J I L 7 F J I I F J F J I I I L J L J F J L J L - 7 F - J F - - J I F 7 F J I I I F J L 7 I I F - J L 7 I L - 7 F J I F 7 L 7 I I L 7 I I x I L - - - 7 F - 7 F F - 7 J J I x - L J I F L F -)
    (x F x L F L - - I L 7 - - L - - - F - - J I F J I L 7 L J I F - 7 F - - - J I J L 7 I F L 7 I F J I F J I L 7 L 7 I I L - - - 7 I F 7 J F 7 I L - 7 L - - 7 L J I L 7 I I I I F - J I I L - 7 F J I F - J L 7 L J L 7 I I L 7 L J L - J F - - - J I F J F J F J F - 7 - x L F L L J L J)
    (J J 7 L L 7 J F L - 7 - L I I I L L - - 7 L J F J F L - 7 I L 7 I L - - - 7 L - 7 I L 7 I I I L 7 I L 7 L 7 L 7 I I L 7 F 7 F J L J L 7 I I L - 7 I - F 7 L - 7 L 7 I L J I I I I F J L 7 I I I J I L - 7 - L 7 F - J I I - I F - - 7 F J x F - - J I J I F J F 7 - - J 7 F J L L - - J)
    (F L L x L J x L 7 x L I L L J L - I 7 L L - 7 I F 7 F - J I F J I F 7 F 7 I F - J I F J F J I F J I F J F J F J L J F J I I L - - 7 F J I L - 7 I L 7 I L - 7 L 7 I I F - J I L - J F - J F J L 7 L 7 F J F 7 I I F 7 I L 7 I L - 7 I I F - J F - 7 L - J L - J L 7 x 7 F I - - J 7 I I)
    (J J L - I - 7 x J 7 - - 7 F L - L I - F - - J I I I L - 7 I L 7 I I I I I I I F 7 I I 7 L 7 L J F J L 7 L 7 L 7 F - J F J I F - 7 I I F J F - J L 7 I L - 7 I F J I I I F 7 L - - 7 L 7 F L 7 F J F J I F J I I I I I L 7 I I F - J L J I F - J x L 7 F 7 F - 7 F J - L J 7 J L - - 7 -)
    (I x - x F F J - L 7 7 J I - L 7 F 7 L L - - 7 I I I F - J I F J L J I I L J I I I I I F 7 L - 7 I F 7 I F J F J L - 7 L 7 I I F J I I I F J F 7 F J L 7 F J I L 7 I I L J L - - 7 L 7 I F 7 I I - L 7 I I F J I L J L 7 I I I L - - 7 F J L - S 7 J L J L J F I L - 7 7 x - - L - J I L)
    (F 7 x 7 x 7 7 7 F L 7 F L x F F J L - - - - J L J I I F 7 I L - - 7 I L - 7 L J I I I I L - 7 I L J I I I F L 7 F - J F J L J L - J L J I F J I I F - J L 7 I I I I I F - - - - J F J I I L J L 7 x I I I I F J F 7 F J I I I F - - J I F - - 7 L - - - - 7 F J F - J - - I 7 x F - F 7)
    (L F - L - - F J 7 - J 7 7 7 F L 7 F - 7 F - - - 7 I L J I I I F 7 I I 7 F J F - J I I I F - J I F - J I L 7 F J I F 7 L - 7 F - - 7 F 7 L J F J I L 7 F 7 I I F J L J I F 7 J F 7 L 7 I I F - - J F J I I I L 7 I L J I I I I L - 7 x I L - 7 L - - - 7 F J I F J J 7 I I L J J x I J L)
    (L I 7 x F L L J L L - - 7 J x L L J 7 L J F - - J L 7 F J L - J L J I F J F J - F J I I L 7 F J L 7 I L 7 L J F J I I F 7 L J F 7 L J L - 7 L 7 L 7 I I L J I L - 7 F J I L 7 I I F J I I L 7 F 7 L 7 I I I F J L 7 J F J L J F 7 L - J F 7 L - - - 7 L J L I I J x 7 7 x J I L I J x I)
    (F F J 7 I 7 7 L - I x x - x I - F 7 F - 7 L - - - 7 I L - - - - - 7 I L 7 I F 7 L 7 I L 7 I I F - J F 7 L - 7 I - I I I I F 7 I I 7 F 7 F I F J L I I L 7 F J F 7 I I L I F J I I L 7 I I F J I I F I I I I L - 7 L - J F 7 F J L 7 F - J L - - 7 F J I I L I L 7 x - 7 7 F L 7 x x F I)
    (L L F J J L I x I F - F I F F x I I L 7 I L F 7 x I I F 7 L F 7 F J L - J I I L 7 I I F J I I I F 7 I I F - J L 7 I I I I I I I L 7 I L 7 I L 7 F J L 7 I L 7 I I I L 7 I I F J L 7 I I I I F J I F J I I I F 7 I F - - J L J F 7 L J F - - - 7 I I F - - 7 I F J F L F - F - 7 7 F - 7)
    (x L J J F F I 7 F - 7 I F F - - J L 7 I I F J L - J L J L - J I L - - - 7 I L 7 I I I L 7 L J I I I I I L - 7 F J I I I I I I L 7 I L 7 I I F J I J F I I F J I I I F J I I L 7 F J I I I I I F J I x I I I I L J L - 7 7 F 7 I L - - J F 7 F J L J I F - J L J I J x J J L F F F J I I)
    (F F x J - L 7 7 I I 7 - F L - 7 F 7 I I L J F - - - - - - - 7 I 7 F 7 F J L 7 I L J L 7 I I F J I L J I F 7 I L - J I I L J L - J L 7 I I I L 7 L 7 F J I L 7 I I I I F J I F J L 7 I L J L J L 7 L 7 I I I I F - - - J F J L J F 7 F 7 I I I F 7 F J I - F 7 F F x F L - - - J J - F 7)
    (F F 7 J x F J L - - - 7 L J - L J I I L - - J F - - - - - - J L - J L J L F L J F - - J L 7 L 7 L - 7 I I L J I F - J L - - 7 F 7 F J I I L 7 L 7 I L 7 I F J I I I I L 7 I L 7 F J L - - 7 F - J F J L J I I I F 7 F 7 I F - - J I I I I I L J I I F J F J L - 7 7 7 7 I F 7 J x F F J)
    (L L F - - L 7 F L I x x 7 I F 7 F J L 7 F - 7 L - - - 7 F 7 F 7 F 7 F 7 F F - 7 I F 7 F 7 L 7 I F - J I I F - 7 L 7 F 7 F 7 I I I L 7 I L 7 I F J L 7 I I L 7 I I I L 7 I I F J L 7 F 7 F J I F 7 L - 7 F J I L J L J L J L - - 7 L J L J I F - J I L - J F - - J L 7 - - L L J F L I J)
    (F F I I I L F 7 - L F F F 7 I I L - 7 I L 7 I L F - - J I L J L J L J L - J F J L J I I L 7 I I L 7 F J I L 7 L - J I L J I L J L 7 I L 7 I I L 7 F J I I F J I I I I I I I L 7 F J I I L 7 L J L 7 F J L - J F 7 F - - 7 F 7 F J - F 7 I L J F - J F - 7 I 7 x I F I J I L 7 F I - I x)
    (L I 7 L J - 7 L F 7 F F J L J I F - J L 7 I L - J F 7 F J F - - - - - - - 7 L - 7 F J L 7 L J I F J L 7 I I L 7 F 7 L - 7 L 7 F - J L - J L J F I I F J I L 7 I I I F J I I F J I F J I F J F - - J I F - - 7 I I L - 7 L J I L - 7 I I F 7 F J F 7 L 7 L J - F 7 - 7 F J L L - I J F 7)
    (I x L J x x 7 F - F J L - - 7 I L - - 7 L J F - - J L J L L - - - 7 F 7 F L 7 F J L 7 F J J F J I F - J I F - J I I F - J F J L - - - - - - 7 F J I I F J F J I I I L 7 I I L 7 L J F J L 7 L 7 F 7 I L - 7 L J L - - J F 7 I F - J I L J L J F J I F J F 7 F I J - L F J I F - J - L 7)
    (I x I F - - J I F F - - - - J L - - 7 L - - J x F 7 F 7 F - - 7 F L J L - 7 I L - 7 I L - 7 I F J I F - J L - 7 I I L 7 F J F - - 7 F 7 F 7 I L 7 I I L 7 L 7 I I I - I I I F J F - J F - J F J I L J F - J F - - - - - J I I I F 7 I F - - 7 L 7 I L - J I - J I x I x F 7 F L 7 7 I J)
    (L F x L I J 7 I - L - - - - - - - 7 I x F 7 F 7 I I I I L - 7 L - - 7 F - J L - - J L 7 F J I I F J I F F - - J I I F J L 7 L - 7 I I L J I I x I I I F J F J I I I F J I L J F J F 7 I F 7 L - J F - J F - J F - - - - - J L J I L J L - 7 L - J L - - - J I - - F L I 7 L 7 x L L J 7)
    (x I L - - 7 F I J L I F 7 F 7 F - J L - J L J L J L J I F 7 L - - 7 I L 7 F F 7 F 7 x I L 7 I I L 7 L 7 L - - 7 I I L - 7 L 7 F J I L - 7 I L 7 I I I I F J F J I I L 7 L - 7 L - J L J I I F 7 x L - - J F 7 L - - - - - - 7 F I F - - - J F 7 F - - - 7 L L - 7 I J x F x - 7 F I L J)
    (F F 7 F 7 F J L F - - J L J L J F - 7 F 7 F - 7 F - 7 I I L - - - J L - J F J L J L 7 L - J L J F J F J F - - J I I F 7 L 7 I I F J F - J L - J L J I I L 7 I F J L 7 L 7 F J F 7 F - - J L J L 7 x F - - J L - - - - - - 7 L - J L - 7 F 7 I L J F - - J J 7 J F 7 x - J F - J F 7 J I)
    (7 x L J F J J x I F 7 F 7 F - 7 I F J I L J F J I F J L J F - - - - - - 7 L 7 F - - J F - - 7 F J F J 7 I F 7 F J I I I F J I I L 7 L - - - 7 F - 7 I L 7 L J I F 7 L 7 I I - I I L 7 F 7 F - 7 L 7 L 7 F - 7 F - - - - 7 L - - - - - J I L J F - J F F 7 7 J 7 J I x I J L 7 L J J L I)
    (I 7 - F F J L F I I I I L J I L J L 7 L 7 F J F J I F - 7 I F F - - - - J F J L - - 7 L 7 F J I F J F 7 I I I L 7 I I I L 7 L J x I F 7 F 7 I I F J I F J F - J I L 7 I I I F J L - J I I L 7 L 7 L - J I - L J F - - 7 L - - - - - - - J F 7 I J F - J L 7 x L - J 7 I F - I - L - F -)
    (- - - - J L F - L J I I F - - 7 F 7 L - J L 7 L 7 L J F J I F J F 7 F - - J F 7 F 7 L - J L 7 L J F J L J I L 7 I L J L 7 I F - - J I I I I L J L 7 I L 7 I F 7 I 7 L J L J I F - 7 F J I F J 7 I F 7 F J F - 7 L - 7 I F - - - - - 7 F 7 I I L 7 I F - - J 7 x I - I F - 7 I - F 7 J L)
    (I I - I 7 F 7 J J L L J L - 7 I I I F 7 F 7 L - J F 7 L - J L - J L J F 7 F J L J L - 7 F - J F 7 L - - - J F J L - - 7 L J L - - 7 I L J L - 7 F J L 7 I I I L J F - - - - J I - I I - L J F 7 L J L J - L 7 L - - J I L - 7 F - 7 L J L J L 7 L J L - - - - - 7 7 I F - L - 7 7 J x I)
    (I 7 x F 7 I x I I I F 7 F 7 I I I L J I I L - 7 F J L - - 7 L F 7 J F J I L - - 7 F 7 L J F - J L - - - - 7 I F 7 F - J F - - - - J L - 7 F - J L - 7 I I I L 7 F J F - 7 F 7 I F J L - 7 F J L - 7 F 7 F 7 L - - 7 F J F - J I F J I F 7 7 7 L - - - - 7 F - - J 7 F - J x I L F 7 7 7)
    (I I - J 7 J x L F - J L J L J I L - 7 I L - 7 I L - 7 F 7 L - J L 7 L 7 I F - - J I L 7 x I F - - 7 F - - J L J I L 7 F J F - 7 F 7 F - J L 7 F 7 F J I I I F J L - J I L J I I L 7 F - J L 7 F 7 L J L J L - - - J L 7 L - 7 I L - - J L 7 F 7 F 7 F 7 L J J J L J J x F F L - J x I F)
    (I F 7 - J - 7 I I F 7 F - - 7 L - - J L - - J L - 7 L J L - - - 7 L - J L J x F 7 L 7 L 7 I L - 7 L J - F 7 L F J F J I F J F L J I L - - 7 I I I L 7 I I I L 7 - F - - - 7 I I x L J L F 7 L J L - - - 7 F - - - 7 J L - - J I F - - - 7 L J L J L J L 7 J x x 7 7 7 x 7 J J x F F L J)
    (F - J 7 I x L F L J L J F - J F 7 F - - - - 7 F - J F - - - - 7 L 7 F 7 F - 7 I I L L 7 L J F - J J F - J L 7 L - J I L J F - - - J F - - J I I I F J I I L - J x L - 7 F J L J F - 7 F J L 7 I F 7 F - J I F - 7 L 7 F - - 7 I L - - 7 I F - 7 F 7 F - J F 7 F F F 7 F 7 - 7 7 L J x F)
    (I J - - F F J I I J - L L - - J I I F - 7 F J L 7 7 L - - 7 F J F L J I I F J I I F - J F 7 L - - 7 I F - - J 7 F - - 7 J L 7 F - 7 L 7 F 7 I I L J - L J I F 7 x F 7 I I F 7 L L 7 I L - 7 L 7 I I L - - J L 7 I F J I F - J I F - - J I I 7 L J L J F F J L - - J L J L 7 J I I F J J)
    (I F 7 J L I x F - 7 x F F 7 F 7 L J L 7 I L - 7 I F 7 L F J L 7 F - 7 L J L - J L J F - J L 7 F 7 I I L - - - - J F 7 L 7 F J I 7 L 7 L J I I L - - 7 J - F J L - J L J I I L 7 J I I F 7 L 7 I I I F 7 F - - J L J F J L 7 F J L - 7 x L J F 7 F 7 F 7 I F 7 F - - - - - J - 7 F - - F)
    (x L F 7 L I 7 I F J - F J L J L - 7 F J L - - J L J L 7 L 7 F J L 7 I F - - 7 F - - J F - 7 L J L J I F - - - - - J L - J L 7 I F 7 L 7 F J I F - - J I x L - 7 F 7 F 7 I L 7 I F J L J L 7 I I I I I I L - - 7 F 7 L - 7 I I F - 7 L 7 F 7 I I I I I I I I L J F F 7 7 - I x I 7 F F J)
    (x L I L x I L I I F 7 L - 7 F - 7 I L - - - 7 F 7 F 7 L - J I 7 F J L J F 7 L J J F 7 L 7 L - - - - J L 7 F - - - - - - 7 - L J I I F J L 7 I L 7 J - F 7 7 - L J L J I L - J L J F 7 F 7 L J L J I I I F - - J I L - - J L J L 7 L 7 L J I I L J I I I I I F 7 - I L 7 F 7 7 L F - - x)
    (F F - J F - - I I I I F 7 L J F J L 7 F - - J I L J L - - 7 L 7 L - - - J L 7 F F J L - J F - - - 7 F 7 L J F 7 F 7 F 7 L 7 x I I I I F - J I F J J 7 I L 7 F 7 F 7 F J F - 7 F - J L J L - 7 F 7 L J I L 7 F 7 I F - - 7 F 7 F J J I F - J I F 7 L J L J L J L 7 I F J I I x F I F J 7)
    (L - 7 L 7 L F J L J L J I F 7 L - 7 I L - - - J F - - - - J F J F - - - - 7 L - J F 7 F - J F 7 F L J L - - J L J L J L 7 L 7 F J L J I F - J I F F - L 7 L J L J L J F J F J L 7 F - - - - J I I F 7 L - J I L J L - 7 I I I L - 7 L J F 7 I I L - - - 7 F - 7 L J L - J L - 7 J J J L)
    (7 7 F F - - L 7 F 7 F 7 L J L - 7 I I F - 7 F 7 L - - - - 7 L - J F - - 7 I F 7 F J I L - - J L - - - - - 7 F 7 F 7 F 7 L - J I F 7 F J I F 7 L - 7 F L L - - - - 7 F J L L 7 F J L - - 7 F 7 I I I L 7 F 7 I F - - - J I I I F - J F 7 I I I I F 7 F 7 L J I I F 7 F 7 F - - J - J J L)
    (I L - - x F F J I L J L 7 F - 7 L J I L 7 L J L 7 F - - - J F 7 F J F - J L J I L 7 L 7 F - 7 F - 7 F - 7 L J L J L J L - - 7 L J L J L L J I F - J - x L L F - 7 L J L F J L J F - - - J I L J L J F J I I I L - - - 7 I I I I L F J L J L J L J L J L - 7 F J I I I L J J F J 7 J 7 J)
    (7 F I J x F L - J L F 7 L J F L - 7 L 7 I F 7 F J L - - - 7 I L J F L - - - 7 L - J F J L 7 I L 7 L J F J F 7 F 7 F 7 F 7 F J J J F I - I x L J J J - I I L L 7 L 7 F - 7 F - 7 I F - - 7 I F - - 7 L - J L J F - - - J I I I L - J F 7 F - 7 F - - - - - J L - J L J J I x I J J F - 7)
    (F - x I F 7 - F F F I L - - - 7 J L 7 L J I I L - - - - 7 L J F F - - - - - J F 7 - L - - J I F J F 7 L - J L J L J L J L J I F - - F - F 7 x I J I - F L 7 - L 7 I L 7 I I F J L J F - J I L - 7 I F 7 F 7 7 L - - - 7 I I L 7 F 7 I I L 7 I L - - - - - - - - - - - - - - - 7 - I L I)
    (I x F F - - - - L F J F - - 7 I F 7 L - - J L 7 F - - 7 L - - 7 L - - 7 F - - J L - - - - 7 I L - J L - - - - - 7 F - - 7 F 7 J I F J J I - I I L J L J L F - - J L - J I I I F - 7 L - - J F 7 I I I I I L 7 F 7 F - J L J - L J I I L 7 I I F 7 F 7 F - - - - 7 F - - 7 F - J J L - L)
    (L F L x I F 7 J L L - J F 7 I L J L - - - - 7 L J F - J F - 7 L - - - J I F - - - - - 7 F J L - 7 F 7 F 7 F - - J L 7 F J I I x L 7 L F 7 F I F F I F I F L - 7 F 7 F 7 L J I I F J F - 7 F J L J L J I L 7 L J L J L F - - - 7 I L J F J I L J L J I L - - - 7 I L - 7 I I I x F x I I)
    (I I L x - 7 L 7 I F F 7 I I L - - - - - - 7 L - 7 L - - J - L 7 F - - 7 I L - - - 7 - L J F F 7 L J L J I L - - 7 F J L - J I - I I x L J - 7 7 I I F - F F 7 L J L J I F 7 L J L 7 I F J L - - 7 F 7 L 7 L 7 F - 7 F J F - 7 L 7 F 7 L - J F - - 7 L - - - 7 I I F 7 I L J - L J x - -)
    (x I 7 L J J I L - F J L J L - - - - - 7 - L - 7 I 7 F - - - - J L 7 L L J F 7 F - J F - - - J L 7 J F 7 L - - - J I F - - 7 L - 7 J - F I J x I I I x x F J L 7 F 7 F J I L 7 F 7 I I L 7 F - 7 L J L 7 I F I I F J I F J x L 7 L J L - - - J F 7 L - - - 7 L J L J L J J x L I L F L J)
    (L - I I x - - 7 L L - - 7 F 7 F - - 7 I F 7 F J L 7 L - - 7 F 7 F J F - - J L J F 7 L - 7 F - 7 L 7 I I F 7 x F - J L 7 x L - - J I F I I J F L - F - L L - 7 L J I L 7 I I L J I I I F J L 7 I F - - J L 7 L J L - J L - - 7 L - - - 7 F - - J L - - - - J L I J 7 I J F F F L x I x I)
    (I 7 L - 7 7 L L 7 x F - J I L J F - J L J I L 7 F J F 7 J L J L J I L - - - 7 F J L 7 F J L 7 L - J I I I L 7 I F - 7 L - - - 7 J L J I F x I x x L F J I J I F 7 L - J I F - - J L J L - - J L J F - - 7 I I F 7 F - - - - J F - - 7 I L 7 F - - - - - - - - - 7 - 7 L 7 - F 7 F J L L)
    (L J J - - 7 - I I F I F 7 I F - J F - - 7 L - J L 7 I I F 7 F 7 F 7 J F 7 F J I F - J L - 7 L 7 F 7 I I I F J I I 7 L 7 F - - J x L L L J - L F - - F x L F J I L 7 F 7 I L - 7 F - - - 7 F - - - J F - J L - J L J F 7 F 7 F J F - J L - J I F 7 F - - - - - - J 7 7 F L x I F 7 J L I)
    (I 7 - F x I - I - F L J L J L - 7 I J F J F 7 F 7 L J L J L J I I L - J L J F J I F - 7 x L 7 L J I I L J L - J L - 7 I L - 7 x F x I L L F J J I 7 I 7 I L - J F J I L J F 7 L J F - - J I F - - - J F 7 F 7 F - - J L J L J F J F - 7 I F J I I I F 7 F 7 F 7 F 7 7 7 - J J F J F - 7)
    (I 7 - J - 7 L L L F - - 7 x F 7 L J F J F J L J L - - 7 F - 7 L J F - 7 F 7 I F J I F J F 7 I F - J I F - - - - - - J L - - J F 7 - I F F F 7 F 7 - J F - F 7 J L - J J F J L - - J F 7 F J I F 7 F 7 I L J L J F - - - - - - J F J F J F J F J L J I I I L J L J L 7 7 L I I x I - - J)
    (F J L J x L 7 x I L - 7 I F J L - - J F J F - - - - - J I F J F 7 L 7 L J I I L 7 I L 7 I I I L - - J I F 7 F - - - - - - - - J I 7 L F I L L F J L F J J I I F 7 F - 7 L - - - - 7 I L J L L J L J L J F 7 F 7 L 7 F 7 I F 7 7 L 7 I - I F J F 7 F J L J F 7 F - - J - J x I - F x F J)
    (L 7 L F 7 x L F - - - J L J F 7 F 7 F J F L 7 F 7 F 7 F J L - J L - J F 7 L J F J I F J I L J F - - - J I I I F 7 F - - - - 7 F J 7 - F 7 L I I x I I F F I I I I L 7 L - 7 F 7 F J L 7 F 7 F 7 F - - - J L J L - J I I F J L 7 L I L - J L 7 I I L 7 F - J I L - - 7 7 I I J L I 7 F J)
    (L L J x L 7 - L - - 7 F 7 F J L J I L 7 F 7 L J L J I I F - - - - - - J L - 7 L - J L - J F 7 L - - 7 F J L J I I L - - - 7 L J J - F J I - F 7 F 7 J - F J L J L 7 L 7 F J I I L - 7 I I L J L J F 7 F - - - - - - J L J F 7 L 7 I F - - - J I L - J I F 7 I F - - J 7 7 L 7 x L J I 7)
    (7 7 F J x I x J x F J I L J F 7 J L - J I I F - - 7 L J L - - - - - - - - 7 I F - 7 F 7 F J L 7 F 7 L J F - - J L 7 F 7 F J J J I J L 7 I - I L J I L F L - - - 7 L - J L - J L - - J I I F 7 F 7 I L J F - - - - - - - 7 I L 7 L J L - - - - J F - 7 L J I L J J J J I 7 F F I I L F 7)
    (L I J J F F J J F L 7 I F - J L - - - - J I I F - J F 7 F - - - - - - - - J I L 7 L J I L - 7 I I L - - J F - - 7 L J L J F L F - - 7 I I F J F 7 I 7 J F 7 J F J F - - 7 F 7 F - 7 F J L J L J L J F 7 L - - - - - - 7 I I I I F - - - 7 F - - J F I F - J F 7 7 I J I F - L L 7 I I I)
    (x I I x 7 J 7 x L F L J L - - - - - - - 7 I I I F 7 I L J F - 7 F - - - - 7 L 7 L - 7 L - 7 I I L - - - - J F 7 L 7 7 F - 7 7 L - 7 L J L J F J L J J F J L - J F J F - J I I L 7 L J F 7 F 7 F - - J L - - - - - - - J L J F J L - - 7 I L - - - 7 I L - - J L 7 7 F 7 J x I L F L - J)
    (- I 7 - F x F J I F - - - - - - - - - - J L J L J I L 7 F J F J I F 7 F 7 I F J F 7 L 7 F J I L 7 F 7 F - - J L 7 L 7 I F J F 7 I L - - 7 F J F 7 F 7 L - 7 F 7 I F L - 7 I L 7 I F - J L J I L - - 7 F 7 F 7 F - - - - - 7 I F 7 F - J I F 7 F - J L - - - 7 F J L F 7 x F 7 - - J J J)
    (x x I L L J 7 7 F L 7 F - - - - - - - - 7 F - - 7 L - J I F L 7 I I L J L J I F J L - J L 7 L 7 I I I L 7 I F 7 L 7 L J L 7 I L 7 F - 7 I L - J L J L 7 - L J I I F - - J I L I I I F 7 F 7 L - - 7 L J L J L J I F - - - J L J I L 7 L L J I L - - - - - 7 L J F 7 I I F - 7 I J F - -)
    (I F L - J I I F J - L J F - - - - - - - J I F - J F - - J F 7 L J L - - - 7 I L - - - 7 7 L - J I I L 7 L - J L 7 I F 7 F J I F J L 7 I I F - - - - 7 I F - - J I L 7 F 7 L 7 L J L J L J L 7 F 7 L - - - - - - - J F 7 F - - 7 L 7 I F 7 F L - 7 F 7 F - J F - J L J I I F J 7 - L J 7)
    (- L - 7 F 7 x L 7 x x I L - 7 F 7 F 7 F 7 I I F - J F - - J L 7 L F 7 F 7 I I F 7 F 7 L - - - 7 I I 7 I F - 7 F J L J L J 7 I I F 7 I L J L 7 I F 7 L J L - - 7 I L L J L 7 I F - - - 7 F 7 L J L - - - - - - - - - J L J F - J F J I I L - - 7 L J L J F 7 I F - - - J I I F 7 x F F J)
    (L L L - 7 I I 7 L L F F F 7 L J L J I I I I L J F 7 L - - 7 F J F J L J L J L J L J L 7 F - - J I L 7 I I F J L - 7 F 7 F - J I I I L - 7 F J F J L 7 F 7 x I I L - 7 F 7 I I L - - 7 L J L - - - - 7 F 7 7 F - - - 7 F 7 I F 7 L - J I F - - J F - - - J L J L 7 F 7 F J L J L 7 7 I I)
    (I I J L 7 - F L J L F 7 I L - - - 7 L J L J - F J I F 7 F J L - J F - 7 I F 7 F - 7 F J L - - 7 L - J I I L - - 7 I I I L - 7 I I L 7 I I L 7 L 7 F J I I F 7 L - 7 I I I L J F - 7 L - 7 F 7 F - 7 L J I F J F - 7 I I I L J L 7 F - J L - - - J F 7 F 7 F - 7 L J L J F - 7 F J I L 7)
    (F 7 - - - F - x L L I I L - - - 7 I L F - - 7 L 7 I I L J F - - - J 7 L - J L J F J I F - - - J F - 7 I L - - 7 L J I I L F J I I F J F J F J F J I 7 I L J L 7 I L J I I F - J F J F - J I L J J L - 7 I L - J F J L J L - - 7 L J F 7 F 7 F - 7 I L J I L 7 L - 7 F 7 L 7 I L - 7 7 7)
    (I I F - I L F J J - I I F 7 F - J I F J F 7 L - J L J F 7 I F - - - - - - - - 7 L 7 L J F 7 7 F J F J L - - - J L F J L 7 L 7 I I L 7 L 7 L 7 L 7 I F J F - - J F 7 F J I L 7 F J I L - - J F - - - - J L - - 7 L - - - - - 7 L - - J L J L J x I L - 7 I F J F - J I I F J I F - J - F)
    (L L J L I - J J x F J I I I L - 7 I I F J L 7 F - - - J L J L - - - - 7 F F 7 L - J F 7 I L 7 I F J F F - - - - 7 L - 7 L 7 I I I F J - L 7 L 7 I I L 7 L 7 F 7 I I L 7 I F J L 7 F - 7 F 7 I F - - 7 F - - 7 L - - 7 F - 7 L - - - 7 F F 7 F F J F 7 I L J I L 7 F J I I F J L 7 J - 7)
    (L I - x F 7 x I F L 7 L J I F 7 I I L J F 7 L J F - - 7 I F - 7 F 7 F L - J L 7 J F J I I F J I I F 7 I F - - - J F 7 L 7 I I I I I I F 7 L 7 L J L 7 L 7 L J I I I F J L J F - J I F J I I L J F - J I F - J F - 7 L J - I F 7 F 7 L - J L 7 L - J I I J x F - J L 7 I I L - - J I F I)
    (F I I x L L F - - 7 L - 7 L J L J I F 7 I I F - J F 7 L 7 L 7 L J L - - - - 7 L 7 I F J I L - J L J I I I F - 7 I I I F J I I L J I F J I - I F - 7 L 7 I F - J I I L - - 7 I F 7 I I F J I F - J F - J I F 7 L 7 L 7 F 7 L J I I I F 7 F 7 L - - 7 I I J F J F 7 F J I I J J I x F - 7)
    (F J F F 7 x L - 7 I F 7 L 7 F - 7 L J I I I L 7 F J L 7 L - J F 7 F - - - 7 L 7 L J L - J F - 7 F 7 I I L J F J F J I L 7 I I F - J I F J F J L 7 L - J I L - 7 I L - - - J L J L J L J F J L - - J F - J I L - J F J I I F 7 L J L J L J I F 7 F J L J L L 7 I L J 7 L J 7 J L - - - L )
    (I x I I 7 F I F I L J L - J I F J F 7 L J L - J L - 7 L - - 7 I L J I F - J - L - 7 F 7 F J I L J L J I F 7 L 7 L 7 I F J L J L - 7 I L 7 L 7 F J F - 7 I F - J L - - - - - - 7 F - - - J J F - - 7 L - - J L F - J L I L J L - 7 I F 7 x L J L J F 7 I I - L J J - L 7 - L - I F I L I )
    (F L L L F - 7 F J F - - 7 F J L - J L - 7 F - - - - J F - - J I F 7 F J F - - 7 J L J L J F 7 F 7 J F J I L - J - I I L - - - 7 F J L 7 I F J L 7 L 7 I I I F 7 F 7 F - - - - J I F F - - 7 L 7 F J F 7 F 7 F J F - - J F - - - J F J L 7 F 7 F 7 I I F 7 - I x I - J I I I F L I J F - )
    (J J L I L 7 L J F J L F J I F - 7 F - 7 I I F 7 F 7 F J F - - J I I L - J F - J F 7 F 7 - I I I L 7 I F J 7 F 7 F J I F 7 F 7 I L 7 F J L J F 7 I F J L J I I I I I L - - - - 7 L 7 L - 7 L - J I F J L J L J F J F - - J F - - 7 I F - J I L J I I I I I F 7 F 7 7 F I - L I x L - - J )
    (I 7 7 I J L 7 F J F - J F J L 7 I L 7 I L J I L J I L 7 L - 7 F J L - - 7 L - - J L J I F J I L 7 I I I F 7 I I L 7 I I L J I I F J L - 7 F J I I L - 7 F J I L J I F 7 F 7 x I F J F 7 L - - 7 I L - - 7 F - J x L - - - J F - J I I - F J F - J I L J L J L J L 7 J L 7 x I x J L J J )
    (- 7 - J I F J I F J F 7 I F - J L - J L 7 7 L - 7 L - J F - J L - 7 F 7 I F 7 F 7 F 7 I L 7 L 7 I L J I I L J I F J I L - 7 L J L 7 - F J L 7 L J F - J L 7 I F - J I I I I F J I F J L 7 F - J L 7 F 7 L J F - - - - - - 7 L 7 7 I L - J F J F 7 I F - - - 7 F 7 L 7 F 7 F F 7 7 L J x )
    (x 7 L I - L - J L 7 I L J I F 7 F - 7 F J F - 7 L - - 7 L 7 F - 7 L J I L J L J L J L J x I F J L - 7 I I F - J L 7 I F 7 L 7 F 7 L 7 I F - J F 7 L - - 7 I I I F 7 I I I I L 7 I L 7 F J L - - 7 L J I F 7 I F - - - - 7 L 7 L - J F - - J F J L J L - - 7 I I L 7 L 7 J - F F 7 I F I )
    (7 L F J - I J J F L J F - J I I I F J I F L 7 I F 7 F J F J L 7 I F 7 L - 7 F 7 I F 7 F 7 I I F 7 F J I I L 7 F 7 I I I I I L J L 7 L J L 7 F J L 7 F - J I I I I I I I I I F J I F J I x F 7 7 L - 7 L J I I I - F - - J 7 L 7 F - J F F 7 I F 7 F - - - J I I 7 L - J J I I J I - I J )
    (I F J 7 L I F F F F - J F - J L J L 7 L - - J L J L J F J F - J L J I F 7 I I L 7 I L J I I L J I I F J I F J I L J I I I F 7 F - J F - - J L 7 F J L - 7 I I I I I I L J I L 7 L J F J F J I F - - J F 7 I L J F J F 7 F 7 F I L - - 7 I I I I L J F - - 7 L J F 7 F 7 F L - F F - L F )
    (7 7 L F x I 7 L L L - - J F - - 7 F L 7 F 7 F - 7 F - J L L 7 F - 7 L J L J I F J I F 7 I L - 7 I I L 7 I L 7 I F 7 I I I I I L - 7 I I F - 7 I I F 7 F J I I I I I L 7 F J F J F 7 L - J F J L - 7 F J L J F 7 L 7 I L J L 7 L 7 F 7 L J L J L - 7 L - 7 L - - J L J L 7 L F J I 7 F J )
    (L J F I 7 L - 7 I L 7 F - J F 7 I F 7 L J L J J L J F - - - J I F J F 7 7 F J L 7 L J I I J F J I L 7 L J F J L J I I I I I L 7 F J L 7 I F J I I I I L 7 I I I I I F J L 7 I F J L - - 7 I F 7 F J I F 7 F J L - J I F 7 F J F L J I F 7 F - 7 F J F 7 L 7 F - 7 F 7 F J I J - L 7 7 J )
    (- I - L x 7 J I F I F L - - J I I I I F 7 7 F - - - J F - - 7 I L 7 I L 7 L - 7 L 7 F J L 7 I F J F L 7 F J F 7 F J L J I I F J I F - J I L 7 I I I I F J I I I I I I F - J L J F 7 F 7 I L J I I F J I I L - - - - J I I I F 7 F 7 L J I L 7 I L - J L - J L 7 L J L J x - x F x 7 7 x )
    (x I F I x - x F F 7 7 F - - 7 I L J L J L - J F 7 F 7 L - 7 L J L L J F J F 7 I F J L - 7 I I L 7 F 7 I L 7 I I I F - 7 I I L 7 I L 7 L L 7 L J I I I L 7 I I I I I I I F 7 F - J I I I I F 7 I I L 7 I L 7 F - - - - J I L J L J L - 7 I F J I F 7 F 7 F - 7 I L F L I - I F - 7 J F 7 )
    (F 7 - I 7 - F - J L 7 L - 7 I L - - - 7 F - 7 I L J L 7 F J F - - - - J I I I I I F 7 F I I I F J I I I F J I I L J J I I L 7 I I F J F 7 L 7 F J I I - I I I I I I I I I I L - 7 I I I I I L J L 7 I I F J L - - - - 7 I F 7 F - - - J L J F J I I I I L 7 I L 7 7 L - 7 L L x I x 7 7 )
    (J - - 7 7 I L 7 F 7 L - - J L - 7 F - J I F J I F - 7 L J 7 L - 7 F 7 F - J I I I I I F J I I I F J I I I F J L 7 F - J I F J I I L 7 I I F J L 7 I L 7 I I I I I I I I I L 7 F J L J I I I F - 7 I I I I F - 7 F - - J I I L J F 7 F - - 7 L - J L J L - J L 7 L 7 7 I 7 7 F - I 7 L J )
    (F J - F L - x I I I F - - - - 7 I L - 7 I L 7 I L 7 L - - 7 F - J I I L 7 F J I I I I I F J I I L 7 I I I L 7 F J L - 7 I L 7 I L 7 I I L J 7 F J I F J I I I L J I I I I F J L - 7 F J I L J F J I I I I I F J L - - 7 I L - - J L J F - J F 7 F - - - - - 7 L 7 I J J x L F F L I x I )
    (J J F - J 7 F L J L J F - - - J L - 7 L J F J L 7 L 7 F 7 I L 7 F J L 7 I L 7 I L J L J L 7 I I F J L J I F J L - 7 F J L 7 I L - J I L - - 7 I F J L 7 I I L - 7 I I I I I F - - J L 7 L 7 F J F J I I L J I F - - - J L 7 F 7 F - 7 L - - J L J F 7 F - 7 I - I I J I J - F J F - - - )
    (L F 7 - - 7 J F F 7 7 L 7 F - - - 7 L - 7 L - - J x L J I L - J I F - J I F J L - 7 F - - J I I I F - - J L - - 7 I I F L I I F - - J F 7 F J I L - 7 I I L - 7 I L J L J I I F - - - J F J L 7 L 7 L J F F J L - - - - 7 L J I I - L - 7 F 7 F - J I I x L J L L J F J J x 7 F J - I x )
    (x I J 7 I J x F J L - 7 L J F - - J F 7 I F 7 F 7 x F 7 L 7 F 7 I I F 7 I L 7 F - J L - - 7 I I I L 7 F 7 F - 7 I I L 7 F J I I F 7 F J I L 7 I F 7 I I I F - J L 7 F F - J I L 7 F 7 F J J I L 7 I F - - J F 7 F 7 F 7 L 7 L I L - 7 J I I I I F 7 L J F 7 F 7 F 7 J I F F F 7 J F F F )
    (F I I I L - - L - - 7 I F 7 L - 7 F J I L J L J L - J L - J I I I I I L J F J L - - - 7 J I I L J F J I L J F J I L 7 I I F J L J I L 7 L 7 I I I L J I I L 7 F 7 L 7 I F 7 I F J I I L - 7 F - J I L - - 7 I I I I I L - J F J F 7 L 7 I I I L J L - - J L J I I I 7 F I J J I 7 I J I )
    (- - L J L J J F F - J L J L - - J I F J F 7 F 7 F 7 F 7 F 7 I I I L J F - J F 7 F - - J F J I F 7 I F J F 7 L 7 L 7 I I I I F - - J F J I I I I I F - J L 7 I I L - J L J I I L - J I F 7 I L - 7 L 7 F - J I L J I I F - 7 L - J L 7 I I I L 7 F 7 F - - 7 F J I L - 7 - x - - - F - 7 )
    (F I F J I - F - L 7 F 7 F 7 F - 7 I L 7 I I I I I I I L J I I I L 7 L L 7 F J I L - - 7 L 7 I I L J I x I L - J F J I I I I L 7 F 7 L 7 F J I I I L 7 F 7 I I L - 7 F - - J L 7 F - J I I L 7 x L 7 I I F 7 L 7 F J L J F J F 7 F 7 L J L J - L J I L - 7 I L - J F 7 I J 7 7 F J L J I )
    (F L J 7 F x L J I I I I I L J F J I F J I I I L J I I F - J I I F J F - J L 7 I F - 7 L 7 I I I F 7 L 7 I F 7 F J I I I I I F J I I F J L 7 I I L 7 I I I I L 7 F J I F 7 F 7 I L 7 F J I F J F - J I I I L 7 I I F - 7 L - J L J L 7 F 7 - F - 7 I F - J L 7 F 7 I L J L L F J F J x - )
    (I F I F F F x F - L J I L 7 F J F J L 7 I I L 7 J L J L - - J I I 7 I F - 7 I L J F J F J I I L J I F J L J I L - 7 L J I I L 7 I I I F - J I L - J I I L J F J L 7 L J I I I I F J I 7 L J - L - 7 I I L 7 L J L J F J F 7 F 7 F 7 L J L 7 I F J I L - - 7 I I I L - - 7 x - - L - 7 L )
    (L 7 - F - J F F J F - J F J L 7 I F - J I L 7 L 7 F - - - - - J I F J I F J L - 7 L 7 I x I I F - J L 7 F 7 L 7 F J F 7 L J F J I I I I F 7 L - - 7 I L 7 F J F 7 L - 7 I I I I L 7 L - 7 F 7 J F J I L 7 L - 7 F - J F J L J I I I F - 7 L J L 7 I F 7 F J I I L 7 F - J 7 J L F J F 7 )
    (I I 7 J I F J - 7 L - 7 L - 7 I I L - - J F J F J I F - 7 F - 7 I L 7 I L 7 F - J F J L 7 L J L 7 F 7 L J L 7 I L - J L - 7 L 7 I I I I I L 7 F 7 I I F J L 7 I I F - J I I L J F J F 7 L J L 7 L - J F J F 7 I I F 7 L - - 7 L J I L 7 I F - - J L J L J J I I J I L 7 - - x 7 I - I F )
    (- L J - F 7 F L - L J L 7 F J I I F - - - J F J F J I F J I F J I F J L 7 I L 7 F J F 7 L 7 F - J I I F - 7 I L - - - 7 F J F J I I I L J F J I I I I L - 7 L J I L 7 F J I F - J F J L 7 F 7 I F 7 F J F J L J I I L 7 F 7 L - 7 I F J I L - - - - - - - 7 L J 7 L 7 I - 7 7 J F 7 F L )
    (F J J I x L I F F L 7 F I I - L J L 7 F - 7 L 7 L - J I F J L - J L 7 F J L - J L 7 I I F J I F 7 I I I F J L - - - 7 I L 7 L 7 I I I F - J F J I I I F 7 I F - J F J I F J L - 7 I 7 F J I L J I L J F J 7 F - J I L I I L 7 F J I I F I F 7 F - - - - - J J F J J L J I L 7 F 7 - 7 J )
    (7 x x I - 7 L J - L I - L J 7 F L F J I - I F J 7 F - J L 7 F - - - J L - - - - 7 L J I I F J I I I L J L - 7 F - - J L - J F J I I I L - 7 I I I I L J I I L - 7 I J L J F - - J L 7 L 7 L - 7 L - 7 I F 7 L - 7 L 7 I L 7 I I F J L 7 L J I L - - - - - 7 7 F x F L I 7 J L - F - J J )
    (F x F I L J 7 7 - I L L I 7 L 7 I L 7 L 7 I I x F J F - 7 I L 7 F - 7 F 7 F - 7 I F 7 L J L 7 I I I F - - - J L 7 L F 7 F 7 L - J I I I F J L 7 I I F - J L 7 F J L - - 7 I F - 7 F J F J F 7 I F - J L J L - 7 I F J L - J L J I F 7 L - 7 L 7 F - 7 F - J 7 I - - 7 x J x F x J J L J )
    (J F L x I 7 F L 7 J 7 L F J F L J - I F J I I F J F J F J L 7 L J 7 I I I L 7 I L J L 7 F 7 I I I I L 7 F 7 F 7 L - J L J L - - 7 L J F J F 7 I L J L 7 F 7 I L - 7 F 7 I L J 7 I L 7 L 7 I I I I F 7 F - - 7 I I L - - - 7 F - J I I F - J F J I J I I 7 x L I - I - - I 7 I L x x I x )
    (x 7 I F L - I 7 L L 7 x F x F 7 J F J I F J I I F J 7 L 7 F J - F - J I I F J I F 7 F J I L J I I L 7 L J I I I F 7 F 7 F 7 F 7 I F - J F J I L - 7 F J I I L 7 x I I I I F - - J F J - L J I I L J I L - 7 L J L 7 F - - J I F - J I L - 7 L - J - I L 7 - L J x J L - L 7 - 7 F - - 7 )
    (x L - F J I L J 7 F - 7 J - F - 7 L - J I F J L J L F - J I F - J F 7 I I I I I I L J F J F 7 I L - J F - J I I I L J L J L J I I I F 7 L 7 I F - J L 7 I L 7 I F J I I I I F - 7 L - - - 7 L J x F J F 7 L 7 F 7 L J L F 7 L J F - J F - J - J J - L - J x F L 7 J x I x L x L J x I I )
    (7 - I x x - - J I J x - - - I F J - L 7 L J 7 I 7 I L 7 F J I F 7 I I I I I F J L - 7 I F J I I x F 7 L - 7 I I L - - - - - 7 I I L J L 7 I L J 7 F 7 L J F J I L 7 I L J L J - L 7 F - - J F - 7 L 7 I L 7 L J L 7 J F J L - 7 I F 7 L - - 7 J I F L J - 7 - I I L 7 F x J - - I F F I )
    (L - 7 - I J 7 F J I I F I x L J 7 I I 7 F L I J - L J I I L L J L J I I L J L 7 F - J I L 7 L J F J I F 7 L J I F - - - - - J I I F - - J L - - - J L 7 - L - J F J L 7 F - - - 7 I L - - 7 L 7 L - J I J I F - 7 L 7 L 7 F - J L J I F 7 F J F F I 7 L F x x L - x F F - J J F L 7 L - )
    (L I L I L J 7 L x F - - - J 7 L I 7 - - L L J F L - - L J I L F - - J L - 7 F J I F 7 L 7 I F 7 L 7 L J I L F J L - - - - 7 J L J L - 7 F 7 F - 7 F - J F - - - J F 7 I L 7 F 7 L J F - 7 I F J F - 7 L 7 I L 7 L 7 L - J L - - - 7 I I I I L - F I J J L 7 x L J F F - x - F I - F 7 x )
    (F - - F L - - J F J L J - - J J I I F - 7 L I 7 J F L I J J x L 7 F 7 F - J L 7 L J L 7 I L J L 7 I F 7 L - J F - 7 F - 7 L - - - - 7 I I I I 7 L J J F L - 7 F - J I I 7 L J L - 7 I I L J L - J F J F J I F J x I F - 7 F - 7 F J I I I L 7 x F I F F 7 J x L F - F 7 7 L F L x L F 7 )
    (L L 7 J J F L F F J x I F 7 7 x L I - x J I F L 7 7 I I x I F 7 L J I L - 7 F J F 7 F J I F 7 F J L J L - - 7 I I I L 7 I F - - 7 F J L J I L 7 L J I L J x L J L F J L 7 F 7 - F J L - 7 L F - - J F J F J L - 7 I I F J I x L J L L J L 7 L - - 7 - L J F J F F 7 L F - 7 I I J - L 7 )
    (x x L J L J 7 I L I - F 7 F F F - J x L 7 7 I - L I 7 J x F I L - - J F 7 I I F J I L 7 I I I L 7 x F - - - J L 7 I F J I L - 7 L J I J L I F J J x F I I x F I F L - - J I I F J F - - J F J F 7 F J F L - 7 F J I I L 7 L - - - - - 7 L L 7 F - J - 7 I F x J L I - J F J - F x 7 J x )
    (7 x I - I L L J F - - I I - x J 7 x F - - 7 x I x L I x J 7 L - 7 F 7 I I I I L 7 I F J L J L - J - L - 7 F 7 F J I L 7 L 7 F J J L L - F J L 7 J x J - I - 7 F L x F - 7 I L J F J I F 7 L 7 I I L - - 7 L I I F J L 7 I F - 7 F - - J - F J I J L F L J I x I - L - L 7 J x I 7 x - J )
    (F 7 7 - 7 L I 7 F I 7 F J J - J J - - J L F F 7 - L I I 7 I L L L J L J I I I F J I I J x J I L x F F - J I I L 7 L 7 I F J L - 7 L I F L - - J - 7 I F - x J I x F L 7 L J F 7 L - 7 I L 7 L J I F 7 F J F J I L - 7 I I I F J I 7 I J L I F J I - J J x F - I J L 7 F 7 L 7 7 7 - F I )
    (L L J - I F 7 F - J I x L - J x x I F - F J x L 7 L - x F J 7 x L J I - L J L J L L J L F - F I - - L 7 F J L 7 I F I I L 7 F - J F L J F J I I J I 7 J J F - L 7 I L L 7 F J L 7 F J L 7 L - - J I I I - L 7 L - 7 I I I I L - J 7 I - - L J L L 7 J x F I x J 7 x L J J L J - - 7 L - )
    (7 F 7 F L J - - J x I 7 x x x F L J F I I F J 7 F x I 7 L L L x L F I x L J 7 I J L I J I I L J I L F J L 7 x I I F J L 7 I I I - F - L J J F I x J L F F I - 7 F - L F J I F - J L 7 F J F 7 F 7 I I I F - J F - J L J L J - J x L I I L I J J J I x F L - 7 L - 7 7 7 x x I x I I J x )
    (L J J 7 J L L 7 L F L L 7 F I 7 I F L - 7 x F - J - L J I L L - F I J F I F J - - x x F L J 7 F J 7 L 7 F J F L J L 7 F J I L 7 - L - L L - L J x F F F 7 7 7 L 7 7 x L 7 I L 7 F 7 I L - J L J I I L J L - 7 L - - 7 J I x x I F L J I J x I J 7 L 7 7 x L 7 x F I I 7 F F 7 7 7 L x x )
    (L x J 7 I F F J J J I I I 7 L - 7 I I L J - I F F - I L I I I F L F J F - F J 7 7 I - 7 J J F L 7 J J L J - I 7 L L L J L L - J 7 J x F I I I L - I F 7 I L F I J 7 x F J I F J I I L - 7 F - - J L 7 - F - J F - 7 I L J - 7 - J x F I - J J x 7 F L F 7 - - J - J J x F x I L L - 7 7 )
    (I - I - L 7 L 7 F F L I 7 - 7 L I L - - J x 7 - - - - J I I - F - I J - I I F J 7 J F - J F 7 J I I - I J x I 7 x x L I x x I - I x F L - L 7 J x I 7 I I x F J - F F L 7 I L - J I F - J L - - 7 F J - I F 7 I F J I - J 7 x F J 7 F J J I J - J J 7 J I L 7 - I L L - J 7 x F x L I J)
    (I - F 7 - - 7 7 7 F I I I L J 7 I L I x F - I F - 7 J L L L F J - I F 7 I F F - 7 - F J F 7 L 7 L I J L - I - 7 - 7 L 7 - F J I L 7 - - - L J L - F J F F x I J x L L J L J 7 L I L J J - L J 7 I L 7 - I I L J L - J x L I - I x 7 L L - J J F F J L - - L J x F - - 7 F L I x F - - 7)
    (7 7 I J x F F I - F I J - - L F I F L - - 7 L 7 7 J L - - L 7 x F I J I F - - - J I L - J J x I 7 F 7 I 7 I L x L L - I - - L I 7 J I x F L 7 I 7 L - F I - J F J 7 7 L L - L 7 I x I J F L F F L 7 L 7 L J x F I J I I F - x L J J F F I - - F - J F J J L F - F 7 I - J L L 7 I - I L)
    (L 7 J L L 7 x 7 - J x L I x L 7 L 7 J J L - - L L x L J 7 - L L J L x - 7 J x F J 7 x F - - F I 7 x L - 7 - - - J J x J x x J x 7 - J F F J I - F - L F J J - 7 - L F - - L J I J - J x L - - J L L - J J L - F I - - x F J x F J x F F 7 J - L I - J - - L J - L - J - J x L 7 J x I J)))
