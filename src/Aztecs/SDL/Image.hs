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
import Control.Arrow (Arrow (..))
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import GHC.Generics (Generic)
import SDL hiding (Surface, Texture, Window, windowTitle)
import qualified SDL
import qualified SDL.Image as IMG

setup :: (MonadAccess b m) => m ()
setup = Asset.setup @_ @_ @Texture

load :: (MonadIO m, ArrowQuery m q, MonadSystem q s) => s ()
load = Asset.loadAssets @Texture

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
newtype Texture = Texture {textureSurface :: SDL.Surface}

instance Asset Texture where
  type AssetConfig Texture = ()
  loadAsset path _ = Texture <$> IMG.load path

-- | Image component.
newtype Image = Image {imageTexture :: Handle Texture}
  deriving (Generic)
  deriving newtype (Show, NFData)

instance Component Image

-- | Draw images to their target windows.
drawImages :: (ArrowDynamicQueryReader q, ArrowQueryReader q, MonadReaderSystem q s, MonadAccess b m) => s (m ())
drawImages = do
  imgs <- S.filter () (Q.entity &&& Q.fetch @_ @Image) (without @Surface)
  assets <- S.single () (Q.fetch @_ @(AssetServer Texture))
  let newAssets =
        mapMaybe (\(eId, img) -> (,img,eId) <$> lookupAsset (imageTexture img) assets) imgs
  return $ mapM_ go newAssets
  where
    go (texture, _, eId) =
      A.insert eId Surface {sdlSurface = textureSurface texture, surfaceBounds = Nothing}

-- | Sprite component.
data Sprite = Sprite
  { spriteTexture :: !(Handle Texture),
    spriteBounds :: !(Maybe (Rectangle Int))
  }
  deriving (Show)

instance Component Sprite

instance NFData Sprite where
  rnf (Sprite texture bounds) = fmap (fmap rnf) bounds `seq` rnf texture

-- | Draw images to their target windows.
drawSprites :: (ArrowQueryReader q, ArrowDynamicQueryReader q, MonadReaderSystem q s, MonadAccess b m) => s (m ())
drawSprites = do
  sprites <- S.all () $ Q.entity &&& Q.fetch
  assets <- S.single () $ Q.fetch @_ @(AssetServer Texture)
  let loadedAssets =
        mapMaybe (\(eId, sprite) -> (,sprite,eId) <$> lookupAsset (spriteTexture sprite) assets) sprites
  return $ mapM_ go loadedAssets
  where
    go (texture, sprite, eId) =
      A.insert eId Surface {sdlSurface = textureSurface texture, surfaceBounds = spriteBounds sprite}

-- | Sprite animation component.
data SpriteAnimation = SpriteAnimation
  { spriteAnimationSteps :: ![Rectangle Int],
    spriteAnimationIndex :: !Int,
    spriteAnimationMS :: !Word32,
    spriteAnimationStart :: !Word32
  }
  deriving (Generic)

instance Component SpriteAnimation

instance NFData SpriteAnimation where
  rnf (SpriteAnimation steps index ms start) = fmap (fmap rnf) steps `seq` rnf index `seq` rnf ms `seq` rnf start

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
spriteAnimationGrid :: V2 Int -> [V2 Int] -> SpriteAnimation
spriteAnimationGrid (V2 w h) tiles =
  spriteAnimation
    { spriteAnimationSteps =
        map (\(V2 x y) -> Rectangle (P $ V2 (x * w) (y * h)) (V2 w h)) tiles
    }

-- | Query to animate sprites based on the inputted `Time`.
animateSpritesQuery :: (ArrowQuery m q) => q Time SpriteAnimation
animateSpritesQuery = proc currentTime -> do
  sprite <- Q.fetch @_ @Sprite -< ()
  animation <- Q.fetch @_ @SpriteAnimation -< ()
  let sprite' = sprite {spriteBounds = Just $ spriteAnimationSteps animation !! spriteAnimationIndex animation}
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
animateSprites :: (ArrowQueryReader qr, ArrowQuery m q, MonadReaderSystem qr s, MonadSystem q s) => s ()
animateSprites = do
  currentTime <- S.single () $ Q.fetch @_ @Time
  void $ S.map currentTime animateSpritesQuery
