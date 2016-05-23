module Asteroids.Types where

import Data.List
import Data.Maybe
-- import Audio.WebAudio.Types

data Phase = GameOver | Playing Ship | Crashing Ship Number | Respawning Number

type Moveable m = {
      x    :: Number
    , y    :: Number
    , dx   :: Number
    , dy   :: Number
    , r    :: Number | m }

type Ship = {
      x    :: Number
    , y    :: Number
    , dx   :: Number
    , dy   :: Number
    , r    :: Number
    , dir  :: Number
    }

type Asteroid = {
      x    :: Number
    , y    :: Number
    , dx   :: Number
    , dy   :: Number
    , r    :: Number
    , path :: Array Number
    , spin :: Number
    , dir  :: Number
    }

type Missile = {
      x    :: Number
    , y    :: Number
    , dx   :: Number
    , dy   :: Number
    , r    :: Number
    , fuse :: Number
    }

type Controls = {
      thrust :: Number
    , left   :: Boolean
    , right  :: Boolean
    }

-- type Sounds = {
--       context         :: AudioContext
--     , beepOscillator  :: OscillatorNode
--     , beepGain        :: GainNode
--     , shootBuffer     :: Maybe AudioBuffer
--     , explosionBuffer :: Maybe AudioBuffer
--     , masterGain      :: GainNode
--     }

type State = {
      w         :: Number
    , h         :: Number
    , phase     :: Phase
    , nships    :: Int
    , score     :: Int
    , asteroids :: Array Asteroid
    , missiles  :: Array Missile
    , controls  :: Controls
--    , sounds    :: Sounds
    }

