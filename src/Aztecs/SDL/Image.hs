{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Aztecs.SDL.Image
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Aztecs.SDL.Image
  ( -- * Components
    Texture (..),
    Image (..),
    Sprite (..),
    SpriteAnimation (..),
    spriteAnimation,
    spriteAnimationGrid,

    -- * Systems
    setup,
    load,
    draw,

    -- ** Primitive systems
    drawImages,
    drawSprites,
    animateSprites,
    animateSpritesQuery,
  )
where

import Aztecs.Asset (Asset (..), AssetServer, Handle, lookupAsset)
import qualified Aztecs.Asset as Asset
import Aztecs.ECS
import qualified Aztecs.ECS.Access as A
import qualified Aztecs.ECS.Query as Q
import qualified Aztecs.ECS.System as S
import Aztecs.SDL (Surface (..))
import Aztecs.Time
import Control.Arrow
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Word
import GHC.Generics
import SDL hiding (Surface, Texture, Window, windowTitle)
import qualified SDL
import qualified SDL.Image as IMG

-- | Setup image assets
--
-- @since 0.6.0
setup :: (MonadAccess b m) => m ()
setup = Asset.setup @_ @_ @Texture

-- | Load image assets
--
-- @since 0.6.0
load :: (MonadIO m, ArrowQuery m q, MonadSystem q s) => s ()
load = Asset.loadAssets @Texture

-- | Draw images and sprites to their target windows.
--
-- @since 0.6.0
draw ::
  ( ArrowDynamicQueryReader qr,
    ArrowQueryReader qr,
    MonadReaderSystem qr s,
    ArrowQuery m q,
    MonadSystem q s,
    MonadAccess b ma
  ) =>
  s (ma ())
draw = do
  access <- drawImages
  animateSprites
  access' <- drawSprites
  return (access >> access')

-- | Texture asset.
--
-- @since 0.6.0
newtype Texture = Texture
  { -- | Texture surface.
    --
    -- @since 0.6.0
    textureSurface :: SDL.Surface
  }

-- | @since 0.6.0
instance Asset Texture where
  type AssetConfig Texture = ()
  loadAsset path _ = Texture <$> IMG.load path

-- | Image component.
--
-- @since 0.6.0
newtype Image = Image
  { -- | Image texture handle.
    --
    -- @since 0.6.0
    imageTexture :: Handle Texture
  }
  deriving (Generic)
  deriving newtype (Show, NFData)

instance Component Image

-- | Draw images to their target windows.
--
-- @since 0.6.0
drawImages :: (ArrowDynamicQueryReader q, ArrowQueryReader q, MonadReaderSystem q s, MonadAccess b m) => s (m ())
drawImages = do
  imgs <- S.filter () (Q.entity &&& Q.fetch @_ @Image) (without @Surface)
  assets <- S.single () (Q.fetch @_ @(AssetServer Texture))
  let newAssets =
        mapMaybe (\(eId, img) -> (,img,eId) <$> lookupAsset (imageTexture img) assets) imgs
  return $ mapM_ go newAssets
  where
    go (texture, _, eId) =
      A.insert eId $ bundle Surface {sdlSurface = textureSurface texture, surfaceBounds = Nothing}

-- | Sprite component.
--
-- @since 0.6.0
data Sprite = Sprite
  { -- | Sprite texture handle.
    --
    -- @since 0.6.0
    spriteTexture :: !(Handle Texture),
    -- | Sprite bounds.
    --
    -- @since 0.6.0
    spriteBounds :: !(Maybe (Rectangle Int))
  }
  deriving (Show)

-- | @since 0.6.0
instance Component Sprite

-- | @since 0.6.0
instance NFData Sprite where
  rnf (Sprite texture bounds) = fmap (fmap rnf) bounds `seq` rnf texture

-- | Draw images to their target windows.
--
-- @since 0.6.0
drawSprites :: (ArrowQueryReader q, ArrowDynamicQueryReader q, MonadReaderSystem q s, MonadAccess b m) => s (m ())
drawSprites = do
  sprites <- S.all () $ Q.entity &&& Q.fetch
  assets <- S.single () $ Q.fetch @_ @(AssetServer Texture)
  let loadedAssets =
        mapMaybe (\(eId, sprite) -> (,sprite,eId) <$> lookupAsset (spriteTexture sprite) assets) sprites
  return $ mapM_ go loadedAssets
  where
    go (texture, sprite, eId) =
      A.insert eId $ bundle Surface {sdlSurface = textureSurface texture, surfaceBounds = spriteBounds sprite}

-- | Sprite animation component.
--
-- @since 0.6.0
data SpriteAnimation = SpriteAnimation
  { -- | Animation steps.
    --
    -- @since 0.6.0
    spriteAnimationSteps :: ![Rectangle Int],
    -- | Animation step index.
    --
    -- @since 0.6.0
    spriteAnimationIndex :: !Int,
    -- | Animation duration (in milliseconds).
    --
    -- @since 0.6.0
    spriteAnimationMS :: !Word32,
    -- | Animation start time.
    --
    -- @since 0.6.0
    spriteAnimationStart :: !Word32
  }
  deriving (Generic)

-- | @since 0.6.0
instance Component SpriteAnimation

-- | @since 0.6.0
instance NFData SpriteAnimation where
  rnf (SpriteAnimation steps index ms start) = fmap (fmap rnf) steps `seq` rnf index `seq` rnf ms `seq` rnf start

-- | Empty sprite animation.
--
-- @since 0.6.0
spriteAnimation :: SpriteAnimation
spriteAnimation =
  SpriteAnimation
    { spriteAnimationSteps = [],
      spriteAnimationIndex = 0,
      spriteAnimationMS = 100,
      spriteAnimationStart = 0
    }

-- | Create a sprite animation from a grid of sprites,
-- given the grid's tile size, and a list of tile indices.
--
-- @since 0.6.0
spriteAnimationGrid :: V2 Int -> [V2 Int] -> SpriteAnimation
spriteAnimationGrid (V2 w h) tiles =
  spriteAnimation
    { spriteAnimationSteps =
        map (\(V2 x y) -> Rectangle (P $ V2 (x * w) (y * h)) (V2 w h)) tiles
    }

-- | Query to animate sprites based on the inputted `Time`.
--
-- @since 0.6.0
animateSpritesQuery :: (ArrowQuery m q) => q Time SpriteAnimation
animateSpritesQuery = proc currentTime -> do
  sprite <- Q.fetch @_ @Sprite -< ()
  animation <- Q.fetch @_ @SpriteAnimation -< ()
  let step = spriteAnimationSteps animation !! spriteAnimationIndex animation
      sprite' = sprite {spriteBounds = Just $ step}
      animation' =
        if elapsedMS currentTime - spriteAnimationStart animation > spriteAnimationMS animation
          then
            animation
              { spriteAnimationIndex =
                  (spriteAnimationIndex animation + 1)
                    `mod` length (spriteAnimationSteps animation),
                spriteAnimationStart = elapsedMS currentTime
              }
          else animation
  Q.set -< sprite'
  Q.set -< animation'

-- | Animate sprites based on the current `Time`.
--
-- @since 0.6.0
animateSprites :: (ArrowQueryReader qr, ArrowQuery m q, MonadReaderSystem qr s, MonadSystem q s) => s ()
animateSprites = do
  currentTime <- S.single () $ Q.fetch @_ @Time
  void $ S.map currentTime animateSpritesQuery
