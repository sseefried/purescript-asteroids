{- -*- Mode: Haskell -*- -}

module Asteroids.Main where


import Prelude
import DOM
import Math
import Debug.Trace
import Control.Monad
import Control.MonadPlus
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.ST
import Graphics.Canvas


import Data.Array
import Data.Tuple
import Data.Foldable
import Data.Maybe.Unsafe
import Data.Maybe
import qualified Data.Int as I

import Data.DOM.Simple.Types
import Data.DOM.Simple.Window
import Data.DOM.Simple.Events

import Debug.Trace

-- import Audio.WebAudio.Types

import Asteroids.Types
-- import Asteroids.Sounds

--main :: forall eff. (Eff eff Unit)
main = do
  w <- innerWidth globalWindow
  h <- innerHeight globalWindow

  let ship = defaultShip w h
      controls = { thrust: 0.0, left: false, right: false }

  asteroids <- replicateM 10 (randomAsteroid w h)

--  sounds <- makeSounds
  st <- newSTRef { w: w, h: h
                 , phase: GameOver
                 , nships: 3
                 , score: 0
                 , asteroids: asteroids
                 , missiles: [ ]
                 , controls: controls
                 --, sounds: sounds-
                 }

  resize st
  addUIEventListener ResizeEvent (resize0 st) globalWindow

  addKeyboardEventListener KeydownEvent (keydown st) globalWindow
  addKeyboardEventListener KeyupEvent (keyup st) globalWindow
  addKeyboardEventListener KeypressEvent (keypress st) globalWindow

  setInterval globalWindow 33.0 $ tick st
--  startSounds st
  return unit

-- |Returns a new ship in the middle of the canvas.
defaultShip :: Number -> Number -> Ship
defaultShip w h = { x: w / 2.0, y: w / 2.0, dir: 0.0, dx: 0.0, dy: 0.0, r: 10.0 }

-- |The maximum speed we'll allow for a ship.
maxSpeed = 6.0

twoPi :: Number
twoPi = 2.0 * pi

-- |Creates a randomly located asteroid somewhere on the canvas.
randomAsteroid :: forall e. Number -> Number -> Eff ( random :: RANDOM | e ) Asteroid
randomAsteroid w h = do
  x  <- randomRange 0.0 w
  y  <- randomRange 0.0 h
  dx <- (*) <$> (randomRange 1.0 2.0) <*> randomSign
  dy <- (*) <$> (randomRange 1.0 2.0) <*> randomSign
  path <- replicateM 12 (randomRange 0.7 1.1)
  spin <- randomRange (-0.1) 0.1
  return { x: x, y: y, dx: dx, dy: dy, r: 50.0, path: path, spin: spin, dir: 0.0 }
      where randomRange lo hi = (\n -> lo + n * (hi - lo)) <$> random
            randomSign = (\n -> if n < 0.5 then (-1.0) else 1.0) <$> random

-- |Thunk to attach resize to the ResizeEvent
-- XXX WTF? Why do I need this?
resize0 :: forall s e. STRef s State -> DOMEvent -> (Eff (st :: ST s, dom :: DOM, canvas :: Canvas | e) Unit)
resize0 st _ = resize st

-- |Handles window resizing by stretching the canvas to fit the
-- viewport and updating our notion of how large the canvas is.
resize :: forall s e. STRef s State -> (Eff (st :: ST s, dom :: DOM, canvas :: Canvas | e) Unit)
resize st = do
  state <- readSTRef st
  w <- innerWidth globalWindow
  h <- innerHeight globalWindow
  (Just canvas) <- getCanvasElementById "canvas"
  setCanvasWidth w canvas
  setCanvasHeight h canvas
  modifySTRef st $ (\state -> state { w = w, h = h })
  return unit

-- |Handles key presses for keys that can be held down.
keydown :: forall s e. STRef s State -> DOMEvent -> Eff (dom :: DOM, st :: ST s | e) Unit
keydown st event = do
--  preventDefault event
  code <- keyCode event
  modifySTRef st $ \state ->
      let controls = case code of
                       37  -> state.controls { left = true }
                       39  -> state.controls { right = true }
                       38  -> state.controls { thrust = 0.5 }
                       _   -> state.controls
      in state { controls = controls }

  return unit

-- |Handles key releases for keys that can be held down.
keyup :: forall s e. STRef s State -> DOMEvent -> Eff (dom :: DOM, st :: ST s | e) Unit
keyup st event = do
  code <- keyCode event
  modifySTRef st $ \state ->
      let controls = case code of
                       37  -> state.controls { left = false }
                       39  -> state.controls { right = false }
                       38  -> state.controls { thrust = 0.0 }
                       _   -> state.controls
      in state { controls = controls }

  return unit

-- |Handles single key presses.
keypress :: forall s e. STRef s State -> DOMEvent -> Eff (dom :: DOM, st :: ST s,  random :: RANDOM | e) Unit
keypress st event = do
  state <- readSTRef st
  k <- keyCode event
  case state.phase of
    Playing ship | (k == 32) -> do
      let x = ship.x
          y = ship.y
          dx = ship.dx + 8.0 * cos (ship.dir - pi/2.0)
          dy = ship.dy + 8.0 * sin (ship.dir - pi/2.0)
          missile = { x: x, y: y, dx: dx, dy: dy, r: 1.0, fuse: 50.0 }

      writeSTRef st $ state { missiles = (missile : state.missiles) }
--      playBufferedSound state.sounds state.sounds.shootBuffer
      return unit

    GameOver | (k == 32) -> do
      asteroids <- replicateM 10 (randomAsteroid state.w state.h)
      writeSTRef st $ state { phase     = Respawning 11.0
                            , nships    = 3
                            , score     = 0
                            , asteroids = asteroids
                            }
      return unit

    _ -> return unit

-- |Handles a single animation frame.
tick :: forall s e. STRef s State -> Eff (st :: ST s, random :: RANDOM, canvas :: Canvas | e ) Unit
tick st = do
  state <- readSTRef st
  state' <- update state
  writeSTRef st state'
  render state

-- |Updates the world state.
update :: forall s e. State -> Eff (st :: ST s, random :: RANDOM | e) State
update state = do
  let -- These asteroids haven't been hit.
      asteroids = do
        a <- state.asteroids
        guard $ not (any (flip hittest $ a) state.missiles)
        [move state.w state.h $ spin a]

      -- These asteroids have been hit: split 'em.
      asteroids' = do
        a <- state.asteroids
        case uncons $ filter (flip hittest $ a) state.missiles of
          Just { head: m, tail: ms } ->
              if a.r >= 20.0
              then let dx = m.dx / 10.0
                       dy = m.dy / 10.0
                   in [ a { dx = (dx + 1.5 * a.dy), dy = (dy + 1.5 * a.dx), r = a.r / 2.0 },
                        a { dx = (dx - 1.5 * a.dy), dy = (dy - 1.5 * a.dx), r = a.r / 2.0 } ]
              else [ ]

          _ -> [ ]

      asteroids'' = asteroids ++ asteroids'

      -- Remove any missiles that hit something.
      missiles = do
        m <- state.missiles
        guard $ not (any (hittest m) state.asteroids)
        let m' = burn m
        guard $ m'.fuse > 0.0
        [move state.w state.h m']

  case state.phase of
    Playing ship | length asteroids'' > 0 -> do
      let c = state.controls

          -- Update the ship based on any controls.
          dir = ship.dir + (if c.right then (pi / 16.0) else 0.0) + (if c.left then (-pi / 16.0) else 0.0)
          dx = clamp (-maxSpeed) maxSpeed (ship.dx + c.thrust * cos (dir - pi/2.0))
          dy = clamp (-maxSpeed) maxSpeed (ship.dy + c.thrust * sin (dir - pi/2.0))
          x = (state.w + ship.x + ship.dx) % state.w
          y = (state.h + ship.y + ship.dy) % state.h
          ship' = ship { x = x, y = y, dx = dx, dy = dy, dir = dir }

          -- Accumulate points for any hit asteroids
          points = sum $ do
            a <- state.asteroids
            guard $ any (flip hittest $ a) state.missiles
            [if a.r >= 50.0 then 20 else if a.r >= 25.0 then 50 else 100]

          -- Any blowed up asteroids?
          hits = do
                 a <- state.asteroids
                 m <- state.missiles
                 [hittest m a]

          -- Did the ship crash?
          crash = any (hittest ship') asteroids

--      when (crash || any id hits) $ playBufferedSound state.sounds state.sounds.explosionBuffer

      return $ state { phase     = if crash then Crashing ship' 0.0 else Playing ship'
                     , nships    = if crash then state.nships - 1 else state.nships
                     , missiles  = missiles
                     , asteroids = asteroids''
                     , score     = state.score + points
                     }

    Playing ship -> do
      asteroids <- replicateM 10 (randomAsteroid state.w state.h)
      return $ state { phase     = Respawning 33.0
                     , asteroids = asteroids
                     , missiles  = [ ] }

    Crashing ship step -> do
      let ship' = ship { x = (state.w + ship.x + ship.dx) % state.w,
                         y = (state.h + ship.y + ship.dy) % state.h }
          phase = if step > 33.0 then
                      if state.nships > 0 then Respawning 33.0 else GameOver
                  else Crashing ship' (step + 1.0)

      return $ state { phase     = phase
                     , missiles  = missiles
                     , asteroids = asteroids'' }

    Respawning n | n > 0.0 -> do
      return $ state { phase = Respawning (n-1.0)
                     , missiles  = missiles
                     , asteroids = asteroids'' }

    Respawning n -> do
      let center = (defaultShip state.w state.h) { r = 40.0 }
          phase = if any (hittest center) state.asteroids
                  then Respawning n
                  else Playing (defaultShip state.w state.h)

      return $ state { phase     = phase
                     , missiles  = missiles
                     , asteroids = asteroids'' }

    GameOver -> do
      return $ state { missiles  = missiles
                     , asteroids = asteroids'' }

-- |Ticks down the fuse on a missile.
burn :: forall m. { fuse :: Number | m } -> { fuse :: Number | m }
burn missile = missile { fuse = missile.fuse - 1.0 }

-- |Checks to see if two Moveable's have collided.
hittest :: forall m n. Moveable m -> Moveable n -> Boolean
hittest a b =
    let dx = a.x - b.x
        dy = a.y - b.y
    in sqrt ((dx * dx) + (dy * dy)) <= (a.r + b.r)

-- |Clamps the value of `x` between `lo` and `hi`.
clamp :: Number -> Number -> Number -> Number
clamp lo hi x = max lo (min hi x)

spin :: Asteroid -> Asteroid
spin a = let spin = a.dir + a.spin in a { dir = if spin > twoPi then spin - twoPi else spin }

-- |Moves a Moveable by updating it's `x` and `y` coordinates by `dx`
-- and `dy`, respectively.
move :: forall m. Number -> Number -> Moveable m -> Moveable m
move w h obj = obj { x = (w + obj.x + obj.dx) % w
                   , y = (h + obj.y + obj.dy) % h }

-- |Renders the current state.
render :: forall e. State -> Eff ( canvas :: Canvas | e ) Unit
render state = do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  setFillStyle "#000000" ctx
  fillPath ctx $ rect ctx { x: 0.0, y: 0.0, w: state.w, h: state.h }

  -- Draw the score
  save ctx
  setFillStyle "#ffffff" ctx
  setFont "16px Hyperspace" ctx
  fillText ctx (show state.score) 10.0 20.0
  restore ctx

  -- Draw the remaining ships
  forE 1.0 (I.toNumber state.nships) $ \i -> do
      let ship = (defaultShip 0.0 0.0) { x = 20.0 * i, y = 40.0 }
      renderShip ctx ship "#ffffff" false
      return unit

  case state.phase of
    Playing ship    -> renderShip ctx ship "#ffffff" (state.controls.thrust /= 0.0)
    Respawning _    -> renderShip ctx (defaultShip state.w state.h) "#555555" false
    Crashing ship i -> renderCrash ctx ship i
    GameOver        -> renderGameOver ctx state.w state.h

  foldM (renderAsteroid ctx) unit state.asteroids
  foldM (renderMissile ctx) unit state.missiles
  return unit

-- |Renders a ship.
renderShip :: forall e. Context2D -> Ship -> String -> Boolean -> Eff ( canvas :: Canvas | e ) Unit
renderShip ctx ship color engines = do
  save ctx
  translate { translateX: ship.x, translateY: ship.y } ctx
  rotate ship.dir ctx
  setLineWidth 1.0 ctx
  setStrokeStyle color ctx
  beginPath ctx
  moveTo ctx 0.0 (-10.0)
  lineTo ctx (-7.0) 10.0
  lineTo ctx 0.0 8.0
  lineTo ctx 7.0 10.0
  lineTo ctx 0.0 (-10.0)
  stroke ctx

  when engines $ do
    moveTo ctx (-4.0) 11.0
    lineTo ctx 0.0 15.0
    lineTo ctx 4.0 11.0
    stroke ctx
    return unit

  restore ctx
  return unit

-- |Renders an exploding ship.
renderCrash :: forall e. Context2D -> Ship -> Number -> Eff ( canvas :: Canvas | e ) Unit
renderCrash ctx ship i = do
  save ctx
  translate { translateX: ship.x, translateY: ship.y } ctx
  rotate ship.dir ctx
  setLineWidth 1.0 ctx
  setStrokeStyle "#ffffff" ctx

  save ctx
  translate { translateX: -i, translateY: -i } ctx
  rotate (i * pi / 60.0) ctx
  beginPath ctx
  moveTo ctx 0.0 (-10.0)
  lineTo ctx (-7.0) 10.0
  stroke ctx
  restore ctx

  save ctx
  translate { translateX: i, translateY: -i } ctx
  rotate ((-i) * pi / 30.0) ctx
  beginPath ctx
  moveTo ctx 0.0 (-10.0)
  lineTo ctx 7.0 10.0
  stroke ctx
  restore ctx

  save ctx
  translate { translateX: -i, translateY: i } ctx
  rotate ((-i) * pi / 20.0) ctx
  beginPath ctx
  moveTo ctx (-7.0) 10.0
  lineTo ctx 0.0 8.0
  stroke ctx
  restore ctx

  save ctx
  translate { translateX: i, translateY: i } ctx
  rotate (i * pi / 15.0) ctx
  beginPath ctx
  moveTo ctx 0.0 8.0
  lineTo ctx 7.0 10.0
  stroke ctx
  restore ctx

  restore ctx

  return unit

-- |Renders an asteroid.
renderAsteroid :: forall e. Context2D -> Unit -> Asteroid -> Eff ( canvas :: Canvas | e ) Unit
renderAsteroid ctx _ asteroid = do
  save ctx

  setLineWidth 1.0 ctx
  translate { translateX: asteroid.x, translateY: asteroid.y } ctx
  rotate asteroid.dir ctx

  setStrokeStyle "#ffffff" ctx
  beginPath ctx

  let n :: Int
      n = length asteroid.path
      foo :: Int -> Number
      foo i = I.toNumber i * (twoPi / (I.toNumber n))
      steps :: Array Number
      steps = map foo (1 .. n)
      theta = fromJust $ last steps
      off = fromJust $ last asteroid.path

  moveTo ctx (asteroid.r * off * cos theta) (asteroid.r * off * sin theta)

  for_ (zip asteroid.path steps) $ \(Tuple off theta) -> do
                                  let x = asteroid.r * off * cos theta
                                      y = asteroid.r * off * sin theta
                                  lineTo ctx x y


  stroke ctx

  restore ctx
  return unit

-- |Renders a missile.
renderMissile :: forall e. Context2D -> Unit -> Missile -> Eff ( canvas :: Canvas | e ) Unit
renderMissile ctx _ missile = do
  save ctx
  setFillStyle "#ffffff" ctx
  fillPath ctx $ arc ctx { x: missile.x, y: missile.y, r: 2.0, start: 0.0, end: twoPi }
  restore ctx
  return unit

-- |Renders the GAME OVER screen.
renderGameOver :: forall e. Context2D -> Number -> Number -> Eff ( canvas :: Canvas | e ) Unit
renderGameOver ctx w h = do
  save ctx
  setFillStyle "#ffffff" ctx

  centerText "Game Over" 64.0 (-40.0)
  centerText "Controls" 20.0 0.0
  centerText "Left arrow = rotate left, Right arrow = rotate right" 14.0 20.0
  centerText "Up arrow = thrust, Space = fire" 14.0 40.0
  centerText "Press the space bar to start" 16.0 120.0

  restore ctx
  return unit
      where centerText text pixels y = do
              setFont ((show pixels) ++ "px Hyperspace") ctx
              metrics <- measureText ctx text
              fillText ctx text (w / 2.0 - metrics.width / 2.0) (y + h / 2.0)
