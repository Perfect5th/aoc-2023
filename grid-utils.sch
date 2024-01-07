;; Utilities for rectangular grids of objects.
(library (grid-utils)
  (export cardinals grid-find grid-ref string-list->grid)
  (import (rnrs))

  ;; (grid obj) :: (vector (vector obj))

  ;; cardinals : returns the coordinates in the four cardinal directions from loc.
  ;; (pair number number) -> (list (pair number number))
  (define cardinals
    (lambda (loc)
      (let ([x (car loc)] [y (cdr loc)])
        `((,x . ,(- y 1))
          (,(+ x 1) . ,y)
          (,x . ,(+ y 1))
          (,(- x 1) . ,y)))))

  ;; grid-dimensions : returns the height and width of grid.
  ;; grid -> (values number number)
  (define grid-dimensions
    (lambda (grid)
      (values (vector-length (vector-ref grid 0)) (vector-length grid))))

  ;; grid-ref : returns the object at loc in grid. If loc is outside the grid, returns default.
  ;; (grid obj) (pair number number) -> obj
  (define grid-ref
    (lambda (grid loc default)
      (let ([x (car loc)] [y (cdr loc)])
        (let-values ([(height width) (grid-dimensions grid)])
          (if (or (>= x width) (>= y height))
              default
              (vector-ref (vector-ref grid y) x))))))

  ;; grid-find : returns the coordinates of obj in the grid, or null if it is not present.
  ;; (grid obj) obj -> (pair number number) | '()
  (define grid-find
    (lambda (grid obj)
      (let-values ([(height width) (grid-dimensions grid)])
        (let f ([x 0] [y 0])
          (cond
           [(and (= x width) (= y height)) '()]
           [(= x width) (f 0 (+ y 1))]
           [(equal? (grid-ref grid (cons x y) '()) obj) (cons x y)]
           [else (f (+ x 1) y)])))))

  ;; string-list->grid : coverts ls to a grid of characters.
  ;; (list string) -> (grid char)
  (define string-list->grid
    (lambda (ls)
      (list->vector (map (lambda (l) (list->vector (string->list l))) ls)))))
