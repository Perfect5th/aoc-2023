;; A list of modules. Each module is an assoc list mapping a module type (and label) to a list of
;; destination modules.
(define EXAMPLE
  '((broadcaster a b c)
    (%a b)
    (%b c)
    (%c inv)
    (&inv a)))

(define EXAMPLE2
  '((broadcaster a)
    (%a inv con)
    (&inv b)
    (%b con)
    (&con output)))

;; symbol->label-type : splits sym into the label portion and the type:
;; symbol -> (values symbol symbol)
(define symbol->label-type
  (lambda (sym)
    (let ([chars (string->list (symbol->string sym))])
      (let f ([c chars] [label '()] [type 'broadcast])
        (cond
         [(null? c) (values (string->symbol (list->string (reverse label))) type)]
         [(char=? (car c) #\%) (f (cdr c) label 'flip-flop)]
         [(char=? (car c) #\&) (f (cdr c) label 'conjunction)]
         [else (f (cdr c) (cons (car c) label) type)])))))

;; make-modules-ht : produces a hashtable mapping a label to a pair of module type and a list of
;; destination modules.
;; module :: (list symbol list (list symbol))
;; list -> hashtable
(define make-modules-ht
  (lambda (input)
    (let ([ht (make-eq-hashtable)])
      (let f ([i input])
        (if (null? i)
            ht
            (let-values ([(label type) (symbol->label-type (caar i))])
              (hashtable-set! ht label `(,type () ,(cdar i)))
              (f (cdr i)))))
      (vector-for-each ; second pass to make sure we've got 'low readings on all conjunction types
       (lambda (key)
         (let* ([module (hashtable-ref ht key #f)]
                [targets (caddr module)])
           (for-each
            (lambda (t)
              (let ([target-module (hashtable-ref ht t #f)])
                (when (and target-module (eq? (car target-module) 'conjunction))
                  (hashtable-set! ht t `(,(car target-module)
                                         ,(cons (cons key 'low) (cadr target-module))
                                         ,(caddr target-module))))))
            targets)))
       (hashtable-keys ht))
      ht)))

;; queue-pop! : produces the first item in queue, simultaneously removing it from queue.
(define queue-pop!
  (lambda (queue)
    (let ([item (caar queue)])
      (set-car! queue (cdar queue))
      item)))

;; queue-empty? : produces #t if queue has no items, false otherwise.
;; queue -> boolean
(define queue-empty?
  (lambda (queue)
    (eq? (caar queue) 'ignored)))

;; queue-push! : mutates queue by pushing obj onto it.
;; queue obj -> #void
(define queue-push!
  (lambda (queue obj)
    (let ([end (cons 'ignored '())])
      (set-car! (cdr queue) obj)
      (set-cdr! (cdr queue) end)
      (set-cdr! queue end))))

;; make-queue : produces a queue containing pairs of targets and pulses, applying pulse to each
;; target.
;; (list symbol) symbol -> queue
(define make-queue
  (lambda (from targets pulse)
    (let* ([end (cons 'ignored '())]
           [queue (cons end end)])
      (for-each (lambda (t) (queue-push! queue `(,t ,from ,pulse))) targets)
      queue)))

;; update-conjunction-state : updates state by replacing the value at sender with pulse.
;; (list (pair symbol symbol)) -> (list (pair symbol symbol))
(define update-conjunction-state
  (lambda (state pulse sender)
    (if (assoc sender state)
        (map (lambda (s) (if (eq? (car s) sender) (cons sender pulse) s)) state)
        (cons (cons sender pulse) state))))

;; all-high? : returns #t if all members of state are 'high.
(define all-high?
  (lambda (state)
    (if (null? state)
        #f
        (let f ([s state])
          (cond
           [(null? s) #t]
           [(eq? (cdar s) 'high) (f (cdr s))]
           [else #f])))))

;; apply-pulse-conjunction : produces a new module state by applying pulse along conjunction rules:
;; remembers the last pulse from each input (using an assoc list). if all are high, sends low.
;; otherwise it sends a high pulse.
;; module symbol -> (values module symbol)
(define apply-pulse-conjunction
  (lambda (module pulse sender)
    (let* ([type (car module)]
           [new-state (update-conjunction-state (cadr module) pulse sender)])
      (if (all-high? new-state)
          (values `(,type ,new-state ,@(cddr module)) 'low)
          (values `(,type ,new-state ,@(cddr module)) 'high)))))

;; apply-pulse-flip-flop : produces a new module state by applying pulse following flip-flop rules:
;; if off and high pulse -> ignored
;; if off and low pulse -> on and sends high pulse
;; if on and high pulse -> ignored
;; if on and low pulse -> off and sends low pulse
;; module symbol -> (values module symbol)
(define apply-pulse-flip-flop
  (lambda (module pulse)
    (let ([type (car module)]
          [state (cadr module)])
      (cond
       [(eq? pulse 'high) (values module #f)]
       [(null? state) (values `(,type (on) ,@(cddr module)) 'high)]
       [else (values `(,type () ,@(cddr module)) 'low)]))))

;; apply-pulse : produces a new module state by applying pulse to module from sender.
;; (list symbol list symbol ...) symbol symbol -> (values (list symbol list symbol ...) symbol)
(define apply-pulse
  (lambda (module pulse sender)
    (let ([type (car module)])
      (cond
       [(eq? type 'flip-flop) (apply-pulse-flip-flop module pulse)]
       [(eq? type 'conjunction) (apply-pulse-conjunction module pulse sender)]))))

;; push-button : mutates modules by propagating the pulses starting from broadcaster. This follows
;; a breadth-first sort of pattern. Produces the number of low and high pulses produced.
;; hashtable -> (values number number)
(define push-button
  (lambda (modules push)
    ; (printf "~n")
    (let ([bcast (hashtable-ref modules 'broadcaster #f)]) ; Should never not exist, fine to error.
      (let f ([pulses (make-queue 'broadcaster (caddr bcast) 'low)] [low-acc 1] [high-acc 0])
        (if (queue-empty? pulses)
            (values low-acc high-acc)
            (let* ([next (queue-pop! pulses)]
                   [target (car next)]
                   [from (cadr next)]
                   [pulse (caddr next)]
                   [target-module (hashtable-ref modules target '(flip-flop () ()))])
              (let-values ([(new-target-module new-pulse) (apply-pulse target-module pulse from)])
                (hashtable-set! modules target new-target-module)
                (for-each (lambda (t)
                            (when new-pulse
                              (queue-push! pulses `(,t ,target ,new-pulse))))
                          (caddr target-module))
                (f pulses
                   (if (eq? pulse 'low) (+ low-acc 1) low-acc)
                   (if (eq? pulse 'high) (+ high-acc 1) high-acc)))))))))

;; push-button-n : produces the number of low and high pulses produced by pushing the button n
;; times
;; list number -> (values number number)
(define push-button-n
  (lambda (input n)
    (let ([modules (make-modules-ht input)])
      (let f ([acc-low 0] [acc-high 0] [i 0])
        (if (= i n)
            (values acc-low acc-high)
            (call-with-values
                (lambda () (push-button modules i))
              (lambda (low high) (f (+ acc-low low) (+ acc-high high) (+ i 1)))))))))

;; solve1 : produces the product of the number of low and high pulses produced after pushing the
;; button 1000 times.
;; list -> number
(define solve1
  (lambda (input)
    (let-values ([(low-pulses high-pulses) (push-button-n input 1000)])
      (* low-pulses high-pulses))))

;; solve2 : produces the number of button pushes required to send a low pulse to target rx.
;; list -> number
(define solve2
  (lambda (input)
    (let ([modules (make-modules-ht input)])
      (let f ([i 1])
        (if (push-button-target modules 'rx)
            i
            (f (+ i 1)))))))

(printf "~a~n" (solve1 EXAMPLE))
(printf "~a~n" (solve1 EXAMPLE2))

(define INPUT
  '((&ds qg db bm ft jk qs dz)
    (%cj pg)
    (%xz sx ds)
    (%kd cs jg)
    (%jk ds qs)
    (%nx qr)
    (&dt bx mg qb cl zb)
    (%vx bd)
    (%pz rv)
    (%ft dz)
    (%gz dt dx)
    (%ng ft ds)
    (%sh kn cs)
    (%kn jc cs)
    (%qg ls)
    (%sb lp cs)
    (%dz fg)
    (%dc bd nx)
    (&cs lp jg sb jc dr)
    (%tr bx dt)
    (%mg fj dt)
    (%sx ds lm)
    (%bx qb)
    (%dx bv dt)
    (%jc zf)
    (broadcaster sb dc jk mg)
    (&bd nx pz dc qr cj df tn)
    (%ll zb dt)
    (%fg ds xz)
    (%jg bj)
    (&bm vr)
    (%ls ds ng)
    (%qs db)
    (%bv bs dt)
    (%gq bd vx)
    (%db qg)
    (%zf lh cs)
    (%pg df bd)
    (%bs dt)
    (%lm ds)
    (%df rs)
    (&cl vr)
    (%qm dt ll)
    (&vr rx)
    (%fp bk cs)
    (%qr pz)
    (%mp cj bd)
    (&tn vr)
    (%bj cs fp)
    (%rs gq bd)
    (%qb qm)
    (%zb gz)
    (%bk cs)
    (&dr vr)
    (%lp sh)
    (%fj dt tr)
    (%rv bd mp)
    (%lh cs kd)))

(printf "~a~n" (solve1 INPUT))
