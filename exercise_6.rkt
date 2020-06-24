;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname exercise_6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require "./define_struct.rkt")

; Asteroids implemented using methods

;;; Type definitions

;; This is the base type of all objects on screen.
;; However, this is an "abstract" type.  We will never say (make-game-object ...), we'll
;; make different *subtypes* of game-object.
(define-struct game-object
  (position velocity orientation rotational-velocity)
  #:methods
  ;; update!: game-object -> void
  ;; Update object for the next frame.
  ;; This is a default method; it will be used by any classes that don't
  ;; define their own update! method.
  (define (update! me)
    ;; Do nothing
    (void))
  
  ;; destroy!: game-object -> void
  ;; Destroys the game object
  ;; This is a default method; it will be used by any classes that don't
  ;; define their own destory! method.
  (define (destroy! me)
    (set! all-game-objects
          (remove me all-game-objects)))
  
  ;; render: game-object -> image
  ;; Draws the game-object.
  ;; There is no default method for render, since there is no default
  ;; appearance for objects.  You must fill in a render method for your
  ;; subclass.
  
  ;; radius: game-object -> number
  ;; Size of the game object for purposes of detecting collisions.
  ;; There is no default method for raidus, since there's no default
  ;; size for objects.  You must fill in a radius method for your
  ;; subclass.
  )

;; This is the type for the player's ship.
;; There will always be exactly one of these, and it will be stored
;; in the global variable the-player.
(define-struct (player game-object)
  ()
  #:methods
  ;; FILL IN THE FOLLOWING METHODS
  
  ;; update!: player -> void
  ;; Accelerate if the engines are firing.
  (define update! (lambda (player)
                    (when firing-engines?
                      (set-game-object-velocity!
                       player
                       (posn-+ (game-object-velocity player) (forward-direction player))))))
 
  ;; render: player -> image
  ;; Draw the player's ship
  (define render (lambda (player)
                   (isosceles-triangle 30 40 "solid" "aquamarine")))
  
  ;; radius: player -> number
  ;; Size of the object (for collision detection)
  (define radius (lambda (player)
                   20))
  )

;; This is the type for the asteroids.
;; Asteroids come in different sizes, so they have a radius
;; field in addition to their color field.
(define-struct (asteroid game-object)
  (radius color)
  #:methods
  ;; FILL THESE IN
  
  ;; render: asteroid -> image
  ;; Draw the asteroid
  (define render (lambda (asteroid)
                   (circle (asteroid-radius asteroid) "solid" (asteroid-color asteroid))))
  
  ;; radius: asteroid -> number
  ;; Size of the asteroid
  (define radius (lambda (asteroid)
                   (asteroid-radius asteroid)))
  )

;; This is the type for normal missiles.
(define-struct (missile game-object)
  (lifetime)
  #:methods
  ;; FILL THESE IN
  
  ;; update!: missile -> void
  ;; Decrement missile lifetime and destroy if necessary.
  (define update! (lambda (missile)
                    (begin
                      (set-missile-lifetime! missile (- (missile-lifetime missile) 1))
                      (when (= (missile-lifetime missile) 0)
                        (destroy! missile)))))
  
  ;; render: missile -> image
  ;; Draw the missile
  (define render (lambda (missile)
                   (circle 1 "solid" "white")))
  
  ;; radius: missile -> number
  ;; Size of the missile
  (define radius (lambda (missile)
                   1))
  )

;; HEAT SEEKER MISSILE HERE
(define-struct (heat-seeker missile) ()
  #:methods

  ;; update!: heat-seeker -> void
  ;; updates the heat-seeker's velocity to point at the nearest asteroid
  (define update! (lambda (heat-seeker)
                    (set-game-object-velocity!
                     heat-seeker
                     (posn-+ (game-object-velocity heat-seeker)
                             (posn-* 5 (heading-of
                              (closest-asteroid-to heat-seeker)
                              heat-seeker))))))

  
  ;; render: heat-seeker -> image
  ;; draw the heat-seeking missile
  (define render (lambda (heat-seeker)
                   (circle 2 "solid" "red")))

  ;; radius: heat-seeker -> number
  ;; size of the heat-seeking missile
  (define radius (lambda (heat-seeker)
                   2))
  )

;; UFO HERE
(define-struct (ufo game-object) ()
  #:methods

  ;; update!: ufo -> void
  ;; updates the ufo's velocity to point at the player
  (define update! (lambda (ufo)
                    (set-game-object-velocity!
                     ufo
                     (posn-* 30 (heading-of the-player ufo)))))

  ;; render: ufo -> image
  ;; draw the ufo
  (define render (lambda (ufo)
                   (square 40 "solid" "red")))

  ;; radius: ufo -> number
  ;; size of the ufo
  (define radius (lambda (ufo)
                   20))

  ;; destroy!: ufo -> void
  ;; makes the ufo immortal
  (define destroy! (lambda (ufo)
                     (set-game-object-position!
                      ufo
                      (make-posn 100 100))))
  )



;; Creates the objects to be used in the game.
(define (create-game-objects!)
  (begin (set! the-player
               (make-player (make-posn (/ window-width 2)
                                       (/ window-height 2))
                            (make-posn 0 0)
                            0
                            0))
         (set! all-game-objects
               (cons the-player
                     (build-list asteroid-count
                                 (λ (ignore) (new-asteroid)))))
         ; UNCOMMENT these lines after you implement the ufo type.
         (set! all-game-objects
               (cons (make-ufo (make-posn 100 100)
                               (make-posn 0 0)
                               0
                               100)
                     all-game-objects))
         ))

;;;
;;; Event dispatch
;;;

;; on-key-press: keyname -> void
;; Take appropriate actions for key presses.
(define (on-key-press key)
  (cond [(equal? key "up")
         (set! firing-engines? true)]
        [(equal? key "left")
         (set-game-object-rotational-velocity! the-player
                                               -2)]
        [(equal? key "right")
         (set-game-object-rotational-velocity! the-player
                                               2)]
        [(equal? key " ")
         (fire-missile! make-missile)]
        ; UNCOMMENT these lines after you implement the heat seeker type.
        [(equal? key "s")
         (fire-missile! make-heat-seeker)]
        [else null]))

;; on-key-release: keyname -> void
;; Take appropriate actions for key releases.
(define (on-key-release key)
  (cond [(equal? key "up")
         (set! firing-engines? false)]
        [(or (equal? key "left")
             (equal? key "right"))
         (set-game-object-rotational-velocity! the-player 0)]
        [else null]))

;;;
;;; Don't modify the code below
;;;

;;;
;;; Turnable constants
;;;

(define (for-each proc list)
  (unless (empty? list)
    (begin (proc (first list))
           (for-each proc (rest list)))))

(define window-width 800)
(define window-height 600)
(define frame-rate 30)
(define inter-frame-interval (/ 1.0 frame-rate))
(define asteroid-count 10)

;;;
;;; Tracking game objects
;;;

(define all-game-objects '())  

(define the-player '())

;;;
;;; Control state
;;;

(define firing-engines? false)

;;;
;;; Object creation
;;;

;; new-asteroid: -> asteroid
;; Makes a new asteroid, but does not add it to all-game-objects
(define (new-asteroid)
  (make-asteroid (make-asteroid-position)
                 (random-velocity)
                 0
                 0
                 (random-float 10 30)
                 (random-color)))

;; make-asteroid-position: -> posn
;; Chooses a random position that isn't too close to the player's initial position.
(define (make-asteroid-position)
  (local [(define candidate (make-posn (random window-width)
                                       (random window-height)))]
    (if (> (distance-squared candidate (game-object-position the-player))
           (squared (* 5
                       (radius the-player))))
        candidate
        (make-asteroid-position))))

;; file-missile!: (posn posn number number number -> missile) -> missile
;; Fires a missle in front of the player.
;; Argument is the constructor procedure for the missile; use make-missile or make-heat-seeker.
(define (fire-missile! missile-maker)
  (local [(define forward (forward-direction the-player))]
    (set! all-game-objects
          (cons (missile-maker (posn-+ (game-object-position the-player)
                                       (posn-* (+ (radius the-player) 5)
                                               forward))
                               (posn-+ (game-object-velocity the-player)
                                       (posn-* 100 forward))
                               0
                               0
                               100)
                all-game-objects))))

;;;
;;; Driver loop
;;;

;; asteroids: -> void
;; Runs the asteroids game
(define (asteroids)
  (begin (create-game-objects!)
         (big-bang all-game-objects
           (on-key (λ (ignore key)
                     (begin (on-key-press key)
                            all-game-objects)))
           (on-release (λ (ignore key)
                         (begin (on-key-release key)
                                all-game-objects)))
           (on-tick (lambda (game-objects)
                      (begin (for-each update! game-objects)
                             (update-physics!)
                             all-game-objects))
                    inter-frame-interval)
           (to-draw (lambda (game-objects)
                      (foldl (lambda (object scene)
                               (place-image (rotate (radians->rotation (game-object-orientation object))
                                                    (render object))
                                            (posn-x (game-object-position object))
                                            (posn-y (game-object-position object))
                                            scene))
                             (rectangle window-width window-height "solid" "black")
                             game-objects))
                    800
                    600))))

;;;
;;; Rendering (drawing on the screen)
;;;

(define radians->rotation-coefficient
  (/ -360.0
     (* 2 pi)))

(define (radians->rotation radians)
  (+ (* radians radians->rotation-coefficient)
     -90))

;;;
;;; State update
;;;

;; update-physics!: -> void
;; Updates the orientation and position of every game object based on
;; velocity and angular velocity, and then does collision detection.
;; EFFECT: updates positions and orientations
;; EFFECT: objects destroyed because of collisions
;; EFFECT: updates velocities of asteroids if they bounce off one another
(define (update-physics!)
  (begin (for-each (λ (object)
                     (begin (set-game-object-orientation! object
                                                          (+ (game-object-orientation object)
                                                             (* inter-frame-interval
                                                                (game-object-rotational-velocity object))))
                            (set-game-object-position! object
                                                       (local [(define new-position
                                                                 (posn-+ (posn-* inter-frame-interval
                                                                                 (game-object-velocity object))
                                                                         (game-object-position object)))]
                                                         (make-posn (wrap (posn-x new-position) window-width)
                                                                    (wrap (posn-y new-position) window-height))))))
                   all-game-objects)
         (handle-collisions! all-game-objects)))

;;;
;;; Collision handling
;;;

;; handle-collisions!: (listof game-object) -> void
;; Checks the objects in the list for collisions.
;; EFFECT: calls handle-collision! when a collision is found.
(define (handle-collisions! objects)
  (unless (empty? objects)
    (local [(define head (first objects))
            (define tail (rest objects))]
      (begin (for-each (λ (object)
                         (when (collided? head object)
                           (handle-collision! head object)))
                       tail)
             (handle-collisions! tail)))))

(define (collided? a b)
  (< (distance-squared (game-object-position a)
                       (game-object-position b))
     (squared (+ (radius a)
                 (radius b)))))

;; handle-collision!: game-object game-object -> void
;; Do whatever is necessary to handle a pair of colliding objects.
;; EFFECT: objects may bounce
;; EFFECT: objects may be destroyed.
(define (handle-collision! a b)
  (if (and (asteroid? a)
           (asteroid? b))
      (bounce! a b)
      (begin (destroy! a)
             (destroy! b))))

;; bounce!: game-object game-object -> void
;; update velocities of game-objects to simulate elastic collision.
(define (bounce! a b)
  (local [(define mass-a (mass a))
          (define mass-b (mass b))
          (define vel-a (game-object-velocity a))
          (define vel-b (game-object-velocity b))
          (define one-over-mass (/ 1 (+ mass-a mass-b)))]
    (begin (set-game-object-velocity! a
                                      (posn-* one-over-mass
                                              (posn-+ (posn-* (- mass-a mass-b)
                                                              vel-a)
                                                      (posn-* (* 2 mass-b)
                                                              vel-b))))
           (set-game-object-velocity! b
                                      (posn-* one-over-mass
                                              (posn-+ (posn-* (- mass-b mass-a)
                                                              vel-b)
                                                      (posn-* (* 2 mass-a)
                                                              vel-a)))))))

(define (mass asteroid)
  (squared (radius asteroid)))

;;;
;;; Vector arithmetic
;;;

;; posn-+: posn posn -> posn
;; Adds two posns.
(define (posn-+ a b)
  (make-posn (+ (posn-x a)
                (posn-x b))
             (+ (posn-y a)
                (posn-y b))))

;; posn--: posn posn -> posn
;; Subtracts two posns
(define (posn-- a b)
  (make-posn (- (posn-x a)
                (posn-x b))
             (- (posn-y a)
                (posn-y b))))

;; heading-of: game-object game-object -> posn
;; Heading (unit vector) of the second object with respect to the first.
(define (heading-of object viewer)
  (unit-vector (posn-- (game-object-position object)
                       (game-object-position viewer))))

(define (unit-vector posn)
  (posn-* (/ 1 (posn-magnitude posn))
          posn))

(define (posn-magnitude posn)
  (sqrt (+ (squared (posn-x posn))
           (squared (posn-y posn)))))

;; posn-*: number posn -> posn
;; Multiplies posn by a scalar.
(define (posn-* k p)
  (make-posn (* k (posn-x p))
             (* k (posn-y p))))

(define (distance-between-game-objects go1 go2)
  (sqrt (distance-squared (game-object-position go1)
                          (game-object-position go2))))

(define (distance-squared p1 p2)
  (+ (squared (- (posn-x p1)
                 (posn-x p2)))
     (squared (- (posn-y p1)
                 (posn-y p2)))))

;; forward-direction: game-object -> posn
;; Returns a unit vector in the forward direction fo the game object.
(define (forward-direction object)
  (local [(define o (game-object-orientation object))]
    (make-posn (cos o)
               (sin o))))

;; closest-asteroid-to: game-object -> asteroid
;; Returns the nearest asteroid to the specified game objecct
(define (closest-asteroid-to game-object)
  (arg-min (lambda (x)
             (distance-squared (game-object-position game-object)
                               (game-object-position x)))
           (filter asteroid?
                   all-game-objects)))

(define (arg-min f list)
  (local [(define (loop best best-score remaining)
            (if (empty? remaining)
                best
                (local [(define score (f (first remaining)))]
                  (if (< score best-score)
                      (loop (first remaining)
                            score
                            (rest remaining))
                      (loop best best-score
                            (rest remaining))))))]
    (loop (first list)
          (f (first list))
          (rest list))))

;;;
;;; Randomization
;;;

(define random-color
  (local [(define colors
            (list (color 255 0 0)
                  (color 0 255 0)
                  (color 0 0 255)
                  (color 128 128 0)
                  (color 128 0 129)
                  (color 0 128 128)))]
    (λ () (random-element colors))))

(define (random-element list)
  (list-ref list
            (random (length list))))

(define (random-float min max)
  (+ min
     (* (random)
        (- max min))))

(define (random-velocity)
  (make-posn (random-float -10 10)
             (random-float -10 10)))

;;;
;;; Other arithmetic utilities
;;;

(define (wrap number limit)
  (cond [(< number 0)
         (+ number limit)]
        [(> number limit)
         (- number limit)]
        [else
         number]))

(define (squared x)
  (* x x))
