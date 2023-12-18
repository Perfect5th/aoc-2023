(define EXAMPLE
  '((O e e e e c e e e e)
    (O e O O c e e e e c)
    (e e e e e c c e e e)
    (O O e c O e e e e O)
    (e O e e e e e O c e)
    (O e c e e O e c e c)
    (e e O e e c O e e O)
    (e e e e e e e O e e)
    (c e e e e c c c e e)
    (c O O e e c e e e e)))

;; To make things simpler, we rotate the map so that north is at the bottom. This will help both
;; when rolling rocks and when calculating load.
(define rotate
  (lambda (positions)
    (let f ([p (cdr positions)] [acc (map (lambda (x) (list x)) (car positions))])
      (if (null? p)
          acc
          (f (cdr p) (map (lambda (x a) (cons x a)) (car p) acc))))))

;; Produces a new list consisting of all the items in ls until an object equal to obj is found.
(define take-while
  (lambda (ls obj)
    (let f ([l ls] [acc '()])
      (cond
       [(null? l) (reverse acc)]
       [(eq? (car l) obj) (reverse acc)]
       [else (f (cdr l) (cons (car l) acc))]))))

;; Produces a new list sorted such that all round rocks (O) are moved to the end.
;; Assumes that there are no cubic rocks (c) in ls.
(define rock-sort
  (lambda (ls)
    (list-sort (lambda (a b) (eq? b 'e)) ls)))

;; Moves all rocks as far to the end of the list as they will go. This is done by sorting each
;; section of the list delimited by cubic rocks (c).
(define roll-rocks
  (lambda (ls)
    (let f ([l ls] [acc '()])
      (cond
       [(null? l) (reverse acc)]
       [(eq? (car l) 'c) (f (cdr l) (cons 'c acc))]
       [else (let ([tail (or (member 'c l) '())])
               (f tail (append (rock-sort (take-while l 'c)) acc)))]))))

;; Sums the load per single rounded rock (O), where such is defined as it's position in the ls
;; (1-indexed).
(define calculate-load
  (lambda (ls)
    (let f ([l ls] [acc 0] [pos 1])
      (cond
       [(null? l) acc]
       [(eq? (car l) 'O) (f (cdr l) (+ acc pos) (+ pos 1))]
       [else (f (cdr l) acc (+ pos 1))]))))

;; Calculates the total load of the rocks after they are rolled to the north.
(define solve1
  (lambda (input)
    (apply + 0 (map calculate-load (map roll-rocks (rotate input))))))

;; Tilts pattern in each direction, resulting in a pattern the same way up.
(define spin-cycle
  (lambda (pattern)
    (let f ([i 0] [p pattern])
      (if (= i 4)
          p
          (f (+ i 1) (rotate (map roll-rocks p)))))))

;; Produces the number of rotations until a load number repeats.
(define find-cycle
  (lambda (positions reps)
    (let ([ht (make-hashtable equal-hash equal?)])
      (let f ([i 0] [p positions] [found #f])
        (if (= i reps)
            p
            (let ([load (map calculate-load p)])
              (if (and (not found) (hashtable-contains? ht load))
                  (let ([size (remainder (- reps i) (- i (hashtable-ref ht load 0)))])
                    (f (+ (- reps size) 1) (spin-cycle p) #t))
                  (begin
                    (hashtable-set! ht load i)
                    (f (+ i 1) (spin-cycle p) found)))))))))

;; Calculates the total load of the rocks after they are rolled to each direction 1000000000 times.
(define solve2
  (lambda (input)
    (apply + 0 (map calculate-load (find-cycle (rotate input) 1000000000)))))

(printf "~a~n" (solve1 EXAMPLE))
(printf "~a~n" (solve2 EXAMPLE))

(define INPUT
  '((e e e c e c e O e e e e c e c e O O e O O O e O e e O e e e e e e e O c O O c e c e e c e e e e O e e e c e e e e e O e O c e e O e e O O e e O e c e e e c O e e c e e e O e e e c c O e e e O O e e e)
    (c O e e e e O c e e O e c e c e e O c e e e c O e O e e e e c O e c e c O c e e e e e e e e O e e e O e e c e O O O c O c e e e e e e e O O c c O e c e O O e O e c e e e c c e c c e e e e O c e e O c)
    (e O e e e e e e e O e O e O e O e e O e O e e e e e c e e e e O c e e c e c O e c O O e e e c O e O c e O c e e c O e e e c e e e O e O e e e e e e e c e O e e e e O e c e e c e O c c O c e e O e e e)
    (e e e O e e e e c e O O e e c e e O O c e e c c e e e e c O e e e e e e e O O O O O O e e e O O e e c e e e c e O e e O e e e e c O e e O O e c O e e e c O c e e e O e e O c e e O O e e e e c e e e e)
    (e e O e e e c c O O O e e e e c e e O O e O O c O c c e e c e c e e e e e e e e O c e c e e e e e c O O e e e c e e e e e e e e e e e e c e c c c e e c e c O e e e c e e O e c O c e O c O e e e e e c)
    (e e e e e e e e e e e e e e e O e e e e e c c e e O e e e O c e O c e e e e e e e e c e O e c e e c e e e c e e O e e O e c e e e c e e e e e e c e e e e e e O e O O e O O O e c e O e e e e e O c c e)
    (e e e e e e c e e c e e c c c e O O e c e e e e e e O e e O e O c c O e e O e e e e e O O e c e O O e e e e O e e e e e O O c O e e O e e e e c e e e e e c e c e O c e e O e e O e O O e O c O e O e c)
    (c c O e e e e e O e c e c O e e e c e c O c c c e e e e c e O c e c e e c e e O e c O e e e e e e e e O e O O e e c e e c O c e O c e e O O e e c e O e c c O e e e e c e c c O e O e e e e e e e c c e)
    (O c e e e c c c e e e O e e c e e e c e e e e e e O c e O O e e e e e c e e e O O O e e e c e c O e c e e e c c e e O O c e e e e e e e c O O e e e e e c e e e O e e e c e c O e e e e e e e e e O e O)
    (e e O e e e e O e e e e O O e e O e O e O e e e e e e e e c e O c e e O e c e e c e e O O c e O O e e c e O e e e O e e c e e e O e e c c e e O e e e e e e e e O c c e e e e e e e e e e O O O e e O O)
    (e e e e c c e e e e e e c e c e O e O c O c c e c e e e e c e e O c e e e e e e e e e e e e c e e c e e e c e e e e e e c O e O O c e e e e O e e e e e e e O c c c e e e e O e e e e e e e e e e e O O)
    (e O e O O O e e O c e e e e O e e e O e e c e O c e c O e O e e e c e O e e O c e e e O O c e c e e e e c O O e c e e e e e e e e c O e e e e e e e O e e e c O e e e e e e O e e e O e e e e c e e e c)
    (e c e e e e c O e e e c c e e c e c e e e e e e e e e e O e e c O e e e e e e e e O O O e e e e e e e O e O O e c c e e e c e e e e e e e e e e c e e e e e e c e O c c e c O c O O c O O e e c e e O e)
    (e e e e e e c O e e e c e c e e e e e e e O e e e e e e e e c O e e e e c O e e c c e e e e O e e e e e e O e c e O e e c O O e e e e e e e e e e e e O c c O e e e e c O e O O c e e O c e e O c e e e)
    (e e O O e e e e e O c O e e c O O O O c O e c c e e e e e e e e e O e e c O O c e e e e e e O e O O e e e e e O e e e e e e O O e e O e e e O e e O e e e e O e e O c O e e e e e e e O e e O e O c e e)
    (e e e e O e e e O O e e e e c c e e e e e O O O e e e O c O e c e O e e e O e e e e e c e e e e e e e e c e c O c e e e e e O e O e O O e c c c e c e O O c O O e e e e O e e e e e O e e c e c O e e e)
    (e O O c e e O c O c O e e e c e e O e O e e e e O c e O e c e e e e e e e e O e O e e O e e e e c O c e e e e c e c e O c e c e e e O e O e e O e e c e e c e e e O O c O e e e e e e c O e O e e O e c)
    (O O e e e O e O e c e e c e e e e c e e e c O e O c c O c e e e c e e e e e e e e e e e e c c e O e e O O O e e e e O e e O O e e e e c O O e e e e O e c e e e e O c e O e e e e e e e e O O e e c e e)
    (e e O c e O O c e c O c e e O e e e e e e e O e c c e O e c e e e e O O c e c O c e c c e O e e O c e e c c c c e e e e c e e c e e e e e e c e e e c c e O c c e e e O e e e e O e e c e c e e e e e e)
    (c O e O c e e e e e c e e e e O e e e O e O e e O e O e O c O e O c e e e e c e e O e e e c e O e c O e e e c e e c e c O e e e e e c O e e c c e e e O e e e e e e e O e e e e e O e e O O c O O c c e)
    (e e O e c e e c e e e O e e e e c e c c e c e O e e e e e e c e c e c c e e e e e e e c e O e e c c O c e c O e O O O e e O e e e c O e e e e e e c e e O e e e e e O e O O e e e O c e O e c O O c e O)
    (e c e e O O c e e e e e e e O O e e e e O e c c e e e O e e e e e e O e O O c e O e e e e e e c O e e O c e e c e e O e O e e O e c e e O O e O e c O e e O e e c e e e e e e O e e e O e e e e O c e e)
    (O e e e e c e c O e e O c e e e c e e c c e e c c e O e O e O e e O e O c e e e O c e e c O e O e e e e e e c O e e e e e e e O e e O e O e e e c e c O e e O O c e c e e e e e c O c e e e O e e c e O)
    (e e e O e e e O e O e O e O O e e O e e e e O e e O e e O c c e e O e e e e e e e e e O c O e e O e c e e e c e O e e O e c e e e e e e e e c e e e O e e e e e e e e O c e c e O e e O e e e e e c e e)
    (O O e e e e O c e e e e e e e e e e e c O O e e e e e e O e e e e e O e e e c e e e O O e e e e O O e e O e e e e e e e e e O e c e e e e e e c c O e e e e c e e e O c c e e e e O e c e e e c e e e c)
    (c O O c e e e e c e e e e e e O O e O e e e e O e e e c e e O e e e c e O O O O e e O O O e O e O e e e O c e e e e e e e O e e e e c O c O e O c O e e e e e e c e e e c e e e O O c e O e e e c e e c)
    (e e e e O c e c e e e O e e c e e e e e e e e e e e e e e e e e c e c e e e O e e e O e e e O e O c e c c e e O e c e c e e e e c c c c e e O O e e e e O e e e e e e e c e O O e O e e e e c e e e c e)
    (e e e e e O e c e c O e e c O e e e c O O e e e e O O e e e O c e e O e e e O e e c O e e O e e e e c e c O e e c e e e e e e e O e e e O e O e e e e e c e c c e c O e O O e e c e c e e e e c c c e e)
    (e c O e e c e e e c e e e c e c e O e c O e c O O e e c e c e e c O c e c e c O c e O c O c e e c c e e c e e e c O c e c e e c e O O c e e c e e c e e O e e e O e e e O e e O e e e c e O e e e e O e)
    (e e O e e e e e c e e e e c O c O O c O e e e e e O e e e e e O e e e O O e O e e e e e e O O e e O e e e e O e e e c e e e e O e e e e c e e e O O e e e e O e e O e e e c e e e c c c O e e e e e c e)
    (c c e e c c e e e c O O O e e e c e O O e e e c e c e O c e O e e e e e e e e e O e O e c e O e e O e O O c e O e e e e e e c e c c c e e e c e e c O e c e e e e c e e e c e O e e c c e e e c O e e e)
    (e O e c e e e O e O e e e e O e e e e e e e O e e e c O e e e e e O e e e e e e O O e e O O O c c e c e e e c c O O e O e e e e e e c e e e e O e e c e e c e c c e O O e e e e e e e c O e e e c O O e)
    (e e e c e e e O c e e O e c e O O e c e O O e e O e c O e c e O e c e e e O c e e e e O c e O c e e e e e e O O e c e e e e c e e c e e c e e c e e e e O e c e e e e c c e e e e e e e e e e e e e e e)
    (e O e O c e e e e c e c e e e e e O e e e c e O c O c e e e e e e e c e c O e e O O e O e e e c e e e e c e e e e e O e c e c e c e e c e e e c e e e c c e e O e O e e e c O e e O e c e e c O e e e e)
    (e c e e e e e e O e e O c e e e O e O e c c O e e c e O O O O e O c O e e e e O c e e e c O e e O O e c e c O e e e e e e c e e O e e O e O O c O O O c O c e O e e e e e e e O O e e e e e O e e e c O)
    (O c c e O e e O O O e c e e O e e e e e e c e e e c O c e O e O c e e O e O O e e c e e e e e e O e O e O e e O e O e O O e e c e e e e O e c O e e e O c e c e e c e e O e c e e e c c O e O e e e e e)
    (e c O O c e e e e c e e e e e c e e O O e O O e e e c c e O e e e e e e O O O e c O e e O c e O e O e e e e e e e e e O c e O e c e c e e e c e e c c e e e c O e O e e O O e O c e c e e O e e O e O e)
    (e O e e e e e c e O c e e e e O c e e e e e c e O e e O e e O c e e c e e O O O c O e e e e c O O O O O e c e c e e e e e e e e e O c e e O e c e e e e c e c e e e e O e c e e c c e e c e e e e e e e)
    (e e O e c O O e e e e c O e c e e e c O e e e c e O c e e c e O e e e e O e c e e O e e e e O c e e e e e O e O O e e O O e O O c e c e e c e e c O e O e c e e e O e e e e O O e e e O e c e e O e e O)
    (e O e O O c e e e O c e O O e e e e c e O e e e e O e c c c e e e c e e O e e e e e O e e e e c e O e e O e c O e O e e e e e c e e e e c O c O e e e c e e O e e e O e e e e e e e e O e e e c e e e e)
    (c e e e c e e e c e e O e e c e c c e c e e e e e O c e e c O e c c e O e e e O e e e c e e e e O e O e O e e O e O e e e e e e e e O e O e O e e e e e O e e c e e e e e e e c e e e O e e O e c e c O)
    (c O e e e c e e e O e c e e e e c e O e e e e e O O O e e O e e e O O c O O c e e e e e e e c O e c c e e e c e O c O c e e e e c e e e e e O c e e O O e c e c e O e O e e O e c O O e O e c e e O e e)
    (e e e e e e O e e e e e e e O e c e O e c e O e e e e e e e e e e e O e O e O O c e e e e c O O e e e e e e e e O c e e e e e e e O O e O e e c O e e e c e e e e O c e e O e e e e O O e e O c c e e e)
    (e c c e e O e e c O e e e O e O e e e e c O c e e O O c O e c e e e e c O e O O c O e e e e c e e e e e c e e e c e c O O c e O O e c e O e O e O e O O e c O e O e c e O e e e e c O e O e O e e O e e)
    (O c e e O c O c c e O e e e e O e e c e e e e e e O e e e e c e e O c c O e O c e e e O e e e O e e e O O c e e c O O e e e O c c e c e e e e O c e O e e O e e e e e e e c e e c e e e e e e c O e e c)
    (O e O O e e O e e c e O e e O c O O c O e e e e e c O e e O e e c e e e c e O O e O e e O e e e e e e e e e e e O e e e O e e e O O c c e c e e O e e c e c e O O e c e c e e e O e O e e e e e e O e O)
    (e O e O c c e O e e e e c O e O e e e O c e c e e O e e e e c O e e e e e O e e O c O O e e e e O O c c e c e O e e c O e O e e e O e e O e O e e e c O e O O e e e e e e e e e c e c c O O c e e c e e)
    (e e O O e e e e O e e e e e c e e e e O e e e e e O e e e O O O c e e e c e O e e e e e e e e e O e O e e O O O e c c c O e c e e e e e e e c e c e e e O e O O e c e e O e e e e e c O e e e e e c e e)
    (O e e e c c O e e e O e e c c e e c e c e e e c e c O c e O e e e c e c e O e e O e e e e c c e e e e e e c e e e c e O e e O e e e O O e c e e e O O O c O O e e e O e O c O e e O O e O c c O e e O e)
    (e e c e O O e c e c c e e O e e e e c e e e O O e c O e e e e e e e O e O c e e e O e e c e c O e e O e c c c O c e c e O e O e e e O e c O e c e e c e O e e O c O c e e O e e e e e e e e e c e c e e)
    (O e c e O c c e e e e e e e O O c e e e e O c e e e e e e e O e e O e e e e e c e e e e e e e e e O e O e e e e e c O e e e e e e e c O e e c e O e e e c e e e e O e c e e e e c e e e e e e O e O e c)
    (e O O e O e e e O e e e e e e e c O c e e e e c O c e e e c e e e e e e e e e O e e O e c e O O c e O O e O c e e e c O e e e O e e c e e O c c O e c e e e O c e e O e O O e O e e e c O O e c c e c e)
    (e e e O e O e e e O O O e O e e e e e e c e e O e e e c e c c e O e e O e e c e e e O e O e e e c e O e O e e O O e e O O e e c e e e e O e O c e e c e e e O e O O e c e e e e e c e e e O O c e e e c)
    (e c e e e e e e e e e e e e c O e O O e e O c e e e e e O e e c O e c O e c c O e e e e O e O e e e e O e e O c e e O e e O e c e e O e c e O e c e O e c e e c O e c e O O e O e e e e e e c c e e e O)
    (c e e e e e O e c e e O e O e e e e e e e e O c O c O O e e O O e e e e e c e c e e e e O O c c e e e e e e O e O e c c e e c c e c e O e e e O e c e c O e e e e e e O e e c c e c e c e c e e c O e e)
    (c c e c e O e e O e e e O e e O O e e e O e e e e O e O e e O c e e e O O e e e e O O c e c e e e c e c e e e e O O c e e e e e e e e e e O e e O e e e O e e e O e e e e e e e e c e e O c c e c e c e)
    (e e O O e c e e c e O O e e c O e e c e O e e e O O e O e e e O e e e e c e e O O e e e e c O O e e c e e e e e e e e c e e e O e e e e e e e e e e c e e O e e e O O e O e e e e e e e c e c e e c c e)
    (e c e c e e e c O e O e c e O e e O e O O e O O c e e O O c O O O c O e e e e O e e O c e e O O e O e e c c e e c e c e e e e O O O O O c e e e e O O e e O e e e e c e e O e e e e e e e e e e e e e e)
    (e e e O c e O O e e e e e e O e c e O e c e c O e O O O e e e c O O e e c e e e e c e O e O e O e e e e e e e c e e e c c e e e O e e e O O O e e c c e e e O e e e e O e e e O e c e e e O e O e e e O)
    (e e O e e e O e e c e O e e e e e c e c e e c c e e e e e e e c c c c e e e c c e e e e e e e O e e e O c e c c e e e e O e c e e O O e e e e c e c e O e e e c c O e c O e c c c e O O c e e e e O e O)
    (e O c e e c e e e e O O e e e O e O e e e e e e e O c O O e e O c e O e O e e c e e e e e e c e e O e c e e c O e e e e e e O O e O e e e O e e e e e O e e e e e e e e O c e c O e c e c e c c e O c e)
    (e e O e e e e O e c e e e c e e c c e e O e e e c e O c e e O O e e e O e e e e e e e c O O O e e e e e e c O e e O O e e e O e e e c e e O e e e c c O e c e e e c e c e c c e e e O e e e c e O O e e)
    (O e c e e c e e e O c O O e O c e O e e O e e e e e O e O e e c e c e c c e e e e O e O e e O e e e e e e O O e O e c e e e e e c e e e e c e c c e e e c e e e O e e O e e e e e e e e c e e e e e e c)
    (e O O e O O e e e e e e e e e c e e e c e c O e e e c e e c e e c e c c c O e e e e c e e e O O c e e e e e e c O e O e e e e e e e e e e c O e e e c e c e c O c e e e e e e e e e e c e O c e e e O O)
    (c c e c e e c O c O O e c e e e e e O e e e e e e c e e e e O e e c e O c e e c e O O e e e e e e e e e c e e e e e e e c e c e e c e e O e O c O O e c c c e O e c e c O e e c c e e e e O e O e e O c)
    (c e e e O e e e O e e e e e e e O c e O e c e e c O c e O e e e O e e e O e e e O e O e c e O e e e e O c c c e e c e O e e O e e e e e O e e O O c e e e O e c e e e c e e e e O e c O e e O O e O e e)
    (c O e e e O e e e O e c c e e c e c O O e e e e e c O e c e e O e e e e c O e O e e e e O e e e e e c e O e e e e e c e O e O e e e e c e e e e e e e O e c e O e e c e O e c c e e e e O e e O e e O c)
    (O e e O e e e e e c O e e O e e e O e e e e e e e e O e e c e c e e e e e e e e e e c c O c e e c e e O e c e c O O O e e c e e O e e e O e e O e e c e O e e e e O e e e O e e e c e e e c e e e O e e)
    (c e e e O e e e e e e e O c e e c e O c e O e e e e e c e e e e O c O e e e e e e O e e O e e e e e c e e e e e O e e e c e e e O e e e c O e e c e O e e c e e c e e e e O e e e e c c O c e e e e e c)
    (O e e e O e e c O c e O O e e c c e e c e e e e e e e O e e e e e e e e e c e e e e e e c e e e c e e O e O e e c e e c O e e O e e O e O O e e e e e e O c O O e c e e O c e e e e c e O e c e e e e e)
    (e e e e e c e O O O e e O O O e e e e O e e e c O e e e e O e c e O e c e c O e e c O e e e e e e e O c e e O e c O e c e c e e c e e e e e e O c O c c c e e c e c e O e e O O e O c e c c e O O c e c)
    (O e e e c e e O c e c c O e e c e e e c e e e e e O e e e O e O e e e e e e e e e e e e O c e e e e e c e O e e e e O c e e e e O c e c O c O e e O c e O O e e e e e e e e e e c e O e c O O O e O e e)
    (e e e e e e c e e c e e e O e e O e e e e O O c e O e e e O O e e O e e e e O e e c c O c e e O c e e O e e c c e O e e c e e e e c c O O e e e e e c e e O c e O e e e e c O e c e e e e c c e O e e e)
    (e c e e O e O e e c e e e e e e e e e e O e e e O e e e O e c e e O e c e e e e e e O e O e e e e e O O c e e c e e e O e e e e e O c c e e O e O e e c e O O e c O e O e e e e O e e e O e e c e e c e)
    (e O O e c e e O e e e e e e c e e e e e e e e c e c e O e e e c e e e e e e e e e e O e e O e O e e e e e e e O O e e c c O e e e e e c e e e O e e e c e e c O c c c e e O e e O e O O O O O e c e O e)
    (e O e e c e e O e e O e e e O e e c O e e e e e e O O O e O c e e O c e c O e e e O c O e c e e e O e e e e O e e O O e c e e e e c e e e O c e e e e e e e e e e e e e e e e O e c O e e O e c O e e c)
    (e e e e e e O e e O e e e c c e e O e e c c e O e c c c c e e O O e O e e e e e e e c e c O e e O c e e O c O e e e e e e e c e e O e e e e e c c e e e e e e e e c e e O e e O e c e e e O e e e e O O)
    (e e e e e O e O e O e e e e e O e e O O e e c O e e O c e c O O e e e e c O e e e c O c e c e O O e c e e O e e e O e c e e e e e c e c O c e e O c e e e O O c c e e e e e e e e e c c e e e c c e e c)
    (e e e e O e e O O O O e e O e O e e e c e e e e e c e c e e O O O e e e c O O e O c e e O e e O e e e e c O e e e e e e e c e O e e O O O e e e e e e O O e e e c e O c e e e e e e e e e e e e O c e O)
    (e e e e e e O e e e e c e e e c e c e e O e O c e c e e e O e e O e O O c c e O e e O c O O c c e e e c e c e e O e O O e e e e e e e e e c e e c c O c e e e O e c e e O e c O O e c O c e e e c c O e)
    (O e e c c e O e e e O e e c e O e O e O e e O e e e e e e e e e c e e O e e e c e e O e O e c c O c e O e e c e c e e e e e c e e c O c e e c e O O O c e O e e O e e e O O c e O e e e O c e e e e e e)
    (e O e e e e e O e e e e e e O e e e e e e e O c e e O e O O O O e e O e e O O e O c e c c e O O c O c e O e e e e e e c e e e c e e e e e e e e e O e e O c e e e c O e e c e e c e O e e e O e c O O e)
    (c O e O e e e c e e O O e c e e e O e O c e e e e c e e c e e e c O O O c c e O e O O c e e e e e c e O c e O e e O e c O c O e O e O e e e e e e e e e e O c e O c e c O c c c O e e c e e c e c O O c)
    (c O c c c e O e e O e e O e e e e e e c e O c e c e e e e O O e e O e O c e e e e c e e e c e e e c e c c e e e O O e e O e c O O e e e e e e e e O e e e e O e e O e e e e O O e e e e e e O e e O e c)
    (e O c O e e O e e e c O O O e e e O e O e e e c e O c e e e e e e c e e e e e e c O e e e e e O e c e c e O e c e e e e O c e c O e e c e e e c e e c e e e O O c e e e e e O O O e e e e O O e e e e e)
    (e e O e c e O e e e e O O e e e e e e O e e e O e e c c e O e e e e e e e e c e O e c O e c e e e O e e e O e c e c c O O e c O e e e e e e O e e e O e e O e O e O e c c e e O c e c c e c e c O e e e)
    (e c O O c e c e O c O e e e e O e O e e e e O O c e e e e c c e e e e e c e c c O e e O e e e O O e e e e e c e e O e c e e e e O c e e e e e e e O e e e e O O e O O O e c e e e c e e O e O e c e O O)
    (e O e e e O e e e e c e O e e O e c e e c e e e e e e e e e O e c O c e c e e e O e e c O e O e O e e O c c e e e e e O O e e O e O e e O O e e O e e c e e c e c e O e e c O e e O e O O c c e e e e O)
    (e c c e e e c O e O e e O O e e O e e c e c O e O e O O e c e e c e O O O O e e e e e O e e e O e O e O c e e e O c e c e O e e e e e e e e e e e e e O e e O e e e c e c e e e e e e e e e e O O c e c)
    (e c e e e e c c O e e e e c c O e e c e O e O e e c e e O c c e e e e c e c e O O e e e e e e e e O e O c e e c e e e e e e e e c O O e e e e e e e e O e e e c e O O e e c e e e e e e e e e c e e e e)
    (e c c O O e e c O e c e O e e O O e c e e e c O c e e e e e O e e e e O e c e e e e c O e e O c O e c e O e e c c e O e O e e e e e c e c e e e c e O e e O O e e e e O e e e e c e c e c e e e e e e e)
    (O e O e O e c e e e e c e O O e c e e e c e e e O e e c e O O e e c e e O e e c e e e O e O O e O e e e c c e e e O O e O c O e e O e O O e O c c e e e e e c e e O c e e c O e e e e O e O c O O e c e)
    (e c e O e e e e c e c e e e e c e e O e O e e e e O O e e e e e e O e e e e e e O e c e e e O e e e c e O e O e e c e c e e e e e O e e c O e O e O e e O O O c e O c e e O e e e O c e e O O e e e e e)
    (c e c O e e c e e e e c e e e e e O e c e e e e c e e e e e e O e O e e O e O e O O e O c e e e O e c e O e O e e e c e e e e O e e e c e e c e e e O e c e O e e c e O e e e O O e c e c O c c c e O O)
    (e e c e e O e e c O c c e e c e e e e e O e e e c e e O e e e O O e e c e c e O e c e O e O e e e e e O e e O c O c e e O e e e e e e c e e e c O e O e O O e e O e e e e e e e e e e e e c e e e e c e)
    (e e e e O c e e e e O c O c c c O e O e e c e c e e O e c e O e c e O e e e e O e e e e e e e e c O e O O O e e e e e e O e O O e e e e O e O e O e e e e e c c e O c e e e e c c O e c e O O e e O e c)
    (e e c e O e e e O O e c O e c e e O O e c c e e c e e e O c e e O c e e c O e O e e c e e e e e e O e e e e e c c e e e e O e e e O e O c O O c O e e e e e e e O c c O O O e O O e O e e e O e c e e c)
    (O e e e e e e c e O e e e e e O O e O O e O e e e O e c O e e O e e e O e c e c e e e c e e e O e e e c e O O e O e c e c O O e e e e e c e O e c e e e e O O O e e e e e e e e c c e e e c c e c c O e)
    (c O e e e c e e e e c e e e c c O e c e e e e e e O c e e e c O e e e e e e e e e e c c O e e c e O e c O c e e c e e e O O e O e e e e O e e c c e e c e e e c e e e c e e O e e O e e e e O e e O c e)
    (e e e O O e e e e O c e e O e e c e e e e O e c e c e e e e e O O e c e e c O e e O O e c e c e c e c e e e e e e O e e e O e e c e c e e c c e e e e e c e e e O e c c e e e e e O c e c e e O c O e e)))

(printf "~a~n" (solve1 INPUT))
(printf "~a~n" (solve2 INPUT))
