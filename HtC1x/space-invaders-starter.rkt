;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right
(define I4 (make-invader 200 150 -12))          ;not landed, moving left
(define I5 (make-invader WIDTH 100  12))        ;not landed, moving right but reached the edge
(define I6 (make-invader 0 120     -12))        ;not landed, moving left but reached the edge
(define I7 (make-invader (- WIDTH 1) 100   12)) ;not landed, moving right almost reaching the edge
(define I8 (make-invader (+     0 1) 100  -12)) ;not landed, moving left almost reaching the edge


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;; - empyt
;; (const Invader ListOfInvader)
;; interp. list of invaders
(define LOI1 empty)
(define LOI2 (list I1))
(define LOI3 (list I1 I3 I2))
#;
(define (fn-for-loinvaders loinvaders)
  (cond [(empty? loinvaders) (...)]
        [else
         (... (fn-for-invader (first loinvaders))
              (fn-for-loinvaders (rest loinvaders)))]))

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1
(define M4 (make-missile 300 250))                               ;not hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;; - empty
;; (const Missile ListOfMissile)
;; interp. list of missiles
(define LOM1 empty)
(define LOM2 (list M1))
(define LOM3 (list M1 M4))
#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (fn-for-missile (first lom))
                   (fn-for-lom (rest lom)))]))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T2))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))
(define G4 (make-game (list I1 I2) (list M1 M4) T1))


;; =================
;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main g)
  (big-bang g                           ; Game
    (on-tick   next-game)               ; Game -> Game
    (to-draw   render-game)             ; Game -> Image
    (on-key    handle-key)              ; Game KeyEvent -> Game
    (stop-when game-over? last-scene))) ; Game -> Game

;; Game -> Game
;; produce the next Game:
;; - update all invaders, missile and tank positions,
;; - identify if missile hit invaders
;; - generate new invaders on top screen randomly
(check-random (next-game G0)
              (make-game (list (update-invader (make-invader (random WIDTH) 0 1))) empty (update-tank T0)))
(check-random (next-game G1)
              (make-game (list (update-invader (make-invader (random WIDTH) 0 1))) empty (update-tank T2)))
(check-random (next-game G2)
              (make-game (list (update-invader (make-invader (random WIDTH) 0 1))
                               (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))
                         (update-tank T1)))
(check-random (next-game G3)
              (make-game (list (update-invader (make-invader (random WIDTH) 0 1))
                               (update-invader I2))
                         (list (update-missile M1))
                         (update-tank T1)))

;(define (next-game g) g) ;stub

(define (next-game g)
  (detect-colitions (update-positions (generate-invaders g))))

;; Game -> Game
;; produce new invaders randomly
(check-random (generate-invaders G0)
              (make-game (list (make-invader (random WIDTH) 0 1))
                         empty
                         T0))

;(define (generate-invaders g) g) ; stub

(define (generate-invaders s)
  (make-game (cons (make-invader (random WIDTH) 0 1) (game-invaders s))
             (game-missiles s)
             (game-tank s)))


;; Game -> Game
;; produce next Game with positions updated for invaders, missile, and tank
(check-expect (update-positions G0)
              (make-game empty empty (update-tank T0)))
(check-expect (update-positions G1)
              (make-game empty empty (update-tank T2)))
(check-expect (update-positions G2)
              (make-game (list (update-invader I1))
                         (list (update-missile M1))
                         (update-tank T1)))
(check-expect (update-positions G4)
              (make-game (list (update-invader I1) (update-invader I2))
                         (list (update-missile M1) (update-missile M4))
                         (update-tank T1)))
              

;(define (update-positions g) g) ; stub

(define (update-positions s)
  (make-game (update-invaders (game-invaders s))
             (update-missiles (game-missiles s))
             (update-tank (game-tank s))))

;; ListOfInvaider -> ListOfInvaider
;; produce list of next invaiders with positions updated for invaders
(check-expect (update-invaders empty) empty)
(check-expect (update-invaders (list I1)) (list (update-invader I1)))
(check-expect (update-invaders (list I1 I2)) (list (update-invader I1) (update-invader I2)))

;(define (update-invaders loinvader) loinvader) ;stub

(define (update-invaders loinvaders)
  (cond [(empty? loinvaders) empty]
        [else
         (cons (update-invader (first loinvaders))
               (update-invaders (rest loinvaders)))]))

;; Invader -> Invader
;; produce a invader with position updated
(check-expect (update-invader I1) (make-invader (+ 150 INVADER-X-SPEED) (+ 100 INVADER-Y-SPEED) 12))    ;; moving right
(check-expect (update-invader I4) (make-invader (- 200 INVADER-X-SPEED) (+ 150 INVADER-Y-SPEED) -12))   ;; moving left

(check-expect (update-invader I5) (make-invader WIDTH (+ 100 INVADER-Y-SPEED) -12)) ;; moving  left after reach edge
(check-expect (update-invader I6) (make-invader     0 (+ 120 INVADER-Y-SPEED)  12)) ;; moving  right after reach edge

(check-expect (update-invader I7) (make-invader WIDTH (+ 100 INVADER-Y-SPEED) -12))                     ;; moving right and reaching the edge
(check-expect (update-invader I8) (make-invader     0 (+ 100 INVADER-Y-SPEED)  12))                     ;; moving left and reaching the edge
                                   
;(define (update-invader invader) invader); stub

(define (update-invader invader)
  (if (> (invader-dx invader) 0)
      (if (<= (+ (invader-x invader) INVADER-X-SPEED) WIDTH)
          (make-invader (+ (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED) (invader-dx invader))
          (make-invader WIDTH (+ (invader-y invader) INVADER-Y-SPEED) (- (invader-dx invader))))
      (if (>= (- (invader-x invader) INVADER-X-SPEED) 0)
          (make-invader (- (invader-x invader) INVADER-X-SPEED) (+ (invader-y invader) INVADER-Y-SPEED) (invader-dx invader))
          (make-invader 0 (+ (invader-y invader) INVADER-Y-SPEED) (- (invader-dx invader))))))

;; ListOfMissile -> ListOfMissile
;; produce list of next missiles with positions updated for missiles
(check-expect (update-missiles empty) empty)
(check-expect (update-missiles (list M1)) (list (update-missile M1)))
(check-expect (update-missiles (list M1 M4))
              (list (update-missile M1) (update-missile M4)))

;(define (update-missiles lom) lom) ;stub

(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (update-missile (first lom))
                    (update-missiles (rest lom)))]))

;; Missile -> Missile
;; produce missile with position updated
(check-expect (update-missile M1) (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (update-missile M4) (make-missile 300 (- 250 MISSILE-SPEED)))

;(define (update-missile m) m) ;stub

(define (update-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; produce next tank with position updated for tank
(check-expect (update-tank T0) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)) ;moving to right
(check-expect (update-tank T2) (make-tank (- 50 TANK-SPEED) -1))          ;moving to left

(check-expect (update-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))     ;reached right edge
(check-expect (update-tank (make-tank 0 -1)) (make-tank 0 -1))           ;reached left edge

;(define (update-tank t) t) ;stub

(define (update-tank t)
  (if (> (tank-dir t) 0)
      (if (< (+ (tank-x t) TANK-SPEED) WIDTH)
          (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))
          (make-tank WIDTH (tank-dir t)))
      (if (> (- (tank-x t) TANK-SPEED) 0)
          (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))
          (make-tank 0 (tank-dir t)))))

;; Game -> Game
;; dectect with any missile colided with some ships, if true, remove both
(check-expect (detect-colitions G0)
              (make-game (remove-invaders empty empty)
                         (remove-missiles empty empty)
                         (make-tank (/ WIDTH 2) 1)))
(check-expect (detect-colitions G2)
              (make-game (remove-invaders (list I1) (list M1))
                         (remove-missiles (list I1) (list M1))
                         (make-tank 50 1)))
(check-expect (detect-colitions G3)
              (make-game (remove-invaders (list I1 I2) (list M1 M2))
                         (remove-missiles (list I1 I2) (list M1 M2))
                         (make-tank 50 1)))

;(define (detect-colitions g) g) ; stub

(define (detect-colitions s)
  (make-game (remove-invaders(game-invaders s) (game-missiles s))
             (remove-missiles(game-invaders s) (game-missiles s))
             (game-tank s)))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; produce the list of remained invaders after missiles collitions
(check-expect (remove-invaders empty empty)     empty)
(check-expect (remove-invaders empty (list M1)) empty)
(check-expect (remove-invaders (list I1) empty) (list I1))
(check-expect (remove-invaders (list I1) (list M1)) (list I1))
(check-expect (remove-invaders (list I1) (list M2)) empty)
(check-expect (remove-invaders (list I1) (list M1 M4)) (list I1))
(check-expect (remove-invaders (list I1) (list M1 M3)) empty)
(check-expect (remove-invaders (list I1 I2) (list M1 M4)) (list I1 I2))
(check-expect (remove-invaders (list I1 I2) (list M1 M2)) (list I2))

;(define (remove-invaders loinvaders lom) loinvaders) ;stub

(define (remove-invaders loinvaders lom)
  (cond [(empty? loinvaders) empty]
        [else
         (if (destroied-invader? (first loinvaders) lom)
             (remove-invaders (rest loinvaders) lom)
             (cons (first loinvaders)
                   (remove-invaders (rest loinvaders) lom)))]))

;; Invader ListOfMissile -> Boolean
;; produce true if any missile are 10 pixel from the invader. False otherwise.
(check-expect (destroied-invader? I1 empty) false)
(check-expect (destroied-invader? I1 (list M1)) false)
(check-expect (destroied-invader? I1 (list M2)) true)
(check-expect (destroied-invader? I1 (list M1 M4)) false)
(check-expect (destroied-invader? I1 (list M1 M2)) true)

;(define (destroied-invader? invader lom) false) ;stub

(define (destroied-invader? i lom)
  (cond [(empty? lom) false]
        [else (if (and (<= (abs (- (invader-x i) (missile-x (first lom)))) 10)
                       (<= (abs (- (invader-y i) (missile-y (first lom)))) 10))
                  true
                  (destroied-invader? i (rest lom)))]))

;; ListOfInvader ListOfMissile -> ListOfMissile
;; produce the list the remained missiles after invaders collitions
(check-expect (remove-missiles empty empty) empty)
(check-expect (remove-missiles (list I1) empty) empty)
(check-expect (remove-missiles empty (list M1)) (list M1))
(check-expect (remove-missiles (list I1) (list M4)) (list M4))
(check-expect (remove-missiles (list I1) (list M2)) empty)
(check-expect (remove-missiles (list I1 I4) (list M1 M4)) (list M1 M4))
(check-expect (remove-missiles (list I1 I4) (list M1 M2)) (list M1))

;(define (remove-missiles loinvaders lom) lom) ;stub

(define (remove-missiles loinvaders lom)
  (cond [(empty? lom) empty]
        [else (if (destroied-missile? (first lom) loinvaders)
                  (remove-missiles loinvaders (rest lom))
                  (cons (first lom) (remove-missiles loinvaders (rest lom))))]))

;; Missile ListOfInvader -> Boolean
;; produce true if any invaders are 10 pixels from missile
(check-expect (destroied-missile? M1 empty) false)
(check-expect (destroied-missile? M1 (list I1)) false)
(check-expect (destroied-missile? M2 (list I1)) true)
(check-expect (destroied-missile? M1 (list I1 I4)) false)
(check-expect (destroied-missile? M2 (list I1 I4)) true)

;(define (destroied-missile? m loinvaders) false) ;stub

(define (destroied-missile? m loinvaders)
  (cond [(empty? loinvaders) false]
        [else
         (if (and (<= (abs (- (missile-x m) (invader-x (first loinvaders)))) 10)
                  (<= (abs (- (missile-y m) (invader-y (first loinvaders)))) 10))
             true
             (destroied-missile? m (rest loinvaders)))]))


;; Game -> Image
;; render all imagens on screen for invaders, missiles and tank
(check-expect (render-game G0)
              (render-invaders empty (render-missiles empty (render-tank T0))))
(check-expect (render-game G1)
              (render-invaders empty (render-missiles empty (render-tank T2))))
(check-expect (render-game G2)
              (render-invaders (list I1) (render-missiles (list M1) (render-tank T1))))
(check-expect (render-game G3)
              (render-invaders (list I1 I2) (render-missiles (list M1 M2) (render-tank T1))))

;(define (render-game s) BACKGROUND) ;stub

(define (render-game s)
  (render-invaders (game-invaders s)
                   (render-missiles (game-missiles s)
                                    (render-tank (game-tank s)))))


;; ListOfInvader -> Image
;; render the invanders in the list
(check-expect (render-invaders empty BACKGROUND) BACKGROUND)
(check-expect (render-invaders (list I1) BACKGROUND) (render-invader I1 BACKGROUND))
(check-expect (render-invaders (list I1 I4) BACKGROUND) (render-invader I1 (render-invader I4 BACKGROUND)))

;(define (render-invaders loinvaders s) s)

(define (render-invaders loinvaders s)
  (cond [(empty? loinvaders) s]
        [else
         (render-invader (first loinvaders)
                         (render-invaders (rest loinvaders) s))]))

;; Invader Image -> Image
;; render the invader on the img in position x, y
(check-expect (render-invader I1 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-invader I4 BACKGROUND) (place-image INVADER 200 150 BACKGROUND))

;(define (render-invader i img) img)

(define (render-invader invader img)
  (place-image INVADER (invader-x invader) (invader-y invader) img))

;; ListOfMissile -> Image
;; render the missiles in the list
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)
(check-expect (render-missiles (list M1) BACKGROUND) (render-missile M1 BACKGROUND))
(check-expect (render-missiles (list M1 M4) BACKGROUND) (render-missile M1 (render-missile M4 BACKGROUND)))

; (define (render-missiles lom s) s) ;stub

(define (render-missiles lom s)
  (cond [(empty? lom) s]
        [else (render-missile (first lom)
                              (render-missiles (rest lom) s))]))

;; Missile Image -> Image
;; render the mission on the imag in position x,y
(check-expect (render-missile M1 BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-expect (render-missile M4 BACKGROUND) (place-image MISSILE 300 250 BACKGROUND))

;(define (render-missile m i) i) ;stub

(define (render-missile m i)
  (place-image MISSILE (missile-x m) (missile-y m) i))

;; Tank -> Image
;; render the tank on the screen in position x,y
(check-expect (render-tank T0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank T1) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; Game KeyEvent -> Game
;; change the tank direction by pressing the left and right arrow keys
;; and shot missiles pressing space key
(check-expect (handle-key G0 "left")  (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key G1 "right") (make-game empty empty (make-tank          50  1)))
(check-expect (handle-key G0 " ") (make-game empty (list (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2))) T0))
(check-expect (handle-key G1 " ") (make-game empty (list (make-missile          50 (- HEIGHT TANK-HEIGHT/2))) T2))

(define (handle-key g ke)
  (cond [(key=? ke "left")  (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g))  1))]
        [(key=? ke " ") (shot-missile g)]
        [else g]))

;; Game -> Game
;; include a new missile in the game in same x that tank and y as HEIGHT
(check-expect (shot-missile G0) (make-game empty (list (make-missile (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2))) T0))
(check-expect (shot-missile G1) (make-game empty (list (make-missile          50 (- HEIGHT TANK-HEIGHT/2))) T2))

;(define (shot-missile g) g) ;stub

(define (shot-missile s)
  (make-game (game-invaders s)
             (cons (make-missile (tank-x (game-tank s)) (- HEIGHT TANK-HEIGHT/2)) (game-missiles s))
             (game-tank s)))

;; Game -> Boolean
;; produce true if any invaders landed
(check-expect (game-over? (make-game empty empty T0)) false)
(check-expect (game-over? (make-game (list I1) empty T0)) false)
(check-expect (game-over? (make-game (list I2) empty T0)) true)
(check-expect (game-over? (make-game (list I3) empty T0)) true)
(check-expect (game-over? (make-game (list I1 I4) empty T0)) false)
(check-expect (game-over? (make-game (list I1 I2) empty T0)) true)

;(define (game-over? g) false) ;stub

(define (game-over? s)
  (any-invader-landed? (game-invaders s)))

;; ListOfInvader -> Boolean
;; produce true if any invader landed
(check-expect (any-invader-landed? empty) false)
(check-expect (any-invader-landed? (list I1)) false)
(check-expect (any-invader-landed? (list I2)) true)
(check-expect (any-invader-landed? (list I3)) true)
(check-expect (any-invader-landed? (list I1 I4)) false)
(check-expect (any-invader-landed? (list I1 I2)) true)

; (define (any-invader-landed? loinvaders) false) ;stub
(define (any-invader-landed? loinvaders)
  (cond [(empty? loinvaders) false]
        [else
         (if (>= (invader-y (first loinvaders)) HEIGHT)
             true
             (any-invader-landed? (rest loinvaders)))]))

;; Game -> Image
;; produce the image of current game with "Game Over" text
(check-expect (last-scene G0) (place-image (text "Game Over" 34 "black")
                                           (/ WIDTH 2)
                                           (/ HEIGHT 2)
                                           (render-game G0)))

;(define (last-scene g) g) ;stub

(define (last-scene g)
  (place-image (text "Game Over" 34 "black")
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (render-game g)))