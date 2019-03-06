module Freedom.Virtualized
  ( Config
  , virtualList
  ) where

import Prelude

import Control.Monad.Free.Trans (FreeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (concat, length, slice)
import Data.Int (ceil, floor, toNumber)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Freedom.Markup as H
import Freedom.VNode (VNode(..), VElement(..), VRender, renderChildren)
import Web.DOM.Element as E
import Web.Event.Event (Event, target)

-- | The type of virtual list config.
-- |
-- | - `height`: The height of container
-- | - `rowHeight`: The height of a row view
-- | - `rowView`: The renderer of a row
-- | - `rows`: The data for row view
type Config f state a =
  { height :: Number
  , rowHeight :: Number
  , rowView :: a -> VNode f state
  , rows :: Array a
  }

-- | Render a virtual list.
virtualList :: forall f state a. Functor (f state) => Config f state a -> VNode f state
virtualList config =
  H.op $ H.div
    # H.style scrollerStyle
    # H.didCreate (didCreate config)
    # H.didUpdate (didUpdate config)
    # H.didDelete (didDelete config)
    # H.onScroll (onScroll config)
  where
    scrollerStyle =
      "overflow-x:hidden;overflow-y:scroll;height:" <> show config.height <> "px;"

didCreate
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> E.Element
  -> FreeT (f state) (VRender f state) Unit
didCreate config element = do
  scrollTop <- liftEffect $ E.scrollTop element
  lift $ renderChildren (E.toNode element) $ calcVNodes config scrollTop

didUpdate
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> E.Element
  -> FreeT (f state) (VRender f state) Unit
didUpdate config element = do
  scrollTop <- liftEffect $ E.scrollTop element
  lift $ renderChildren (E.toNode element) $ calcVNodes config scrollTop

didDelete
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> E.Element
  -> FreeT (f state) (VRender f state) Unit
didDelete config element =
  lift $ renderChildren (E.toNode element) []

onScroll
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> Event
  -> FreeT (f state) (VRender f state) Unit
onScroll config evt =
  case E.fromEventTarget <$> target evt of
    Just (Just element) -> do
      scrollTop <- liftEffect $ E.scrollTop element
      lift $ renderChildren (E.toNode element) $ calcVNodes config scrollTop
    _ -> pure unit

calcVNodes :: forall f state a. Functor (f state) => Config f state a -> Number -> Array (VNode f state)
calcVNodes config scrollTop =
  let rowLength = length config.rows
      start = startIndex config scrollTop
      end = start + renderingSize config
      bufferedStart = if start <= 0 then 0 else start - 1
      bufferedEnd = if end >= rowLength then rowLength else end + 1
      rowVNodes = styledView config <$> slice bufferedStart bufferedEnd config.rows
      topPadding = padding config bufferedStart
      bottomPadding = padding config $ rowLength - bufferedEnd
   in concat [ [ topPadding ], rowVNodes, [ bottomPadding ] ]

startIndex :: forall f state a. Config f state a -> Number -> Int
startIndex { rowHeight } scrollTop = floor $ scrollTop / rowHeight

renderingSize :: forall f state a. Config f state a -> Int
renderingSize { height, rowHeight } = ceil $ height / rowHeight

styledView :: forall f state a. Functor (f state) => Config f state a -> a -> VNode f state
styledView { rowHeight, rowView } x =
  withStyle $ rowView x
  where
    style = "height: " <> show rowHeight <> "px;"
    withStyle (VNode key (Element r)) = VNode key $ Element $ r # H.style style
    withStyle (VNode key (OperativeElement bf r)) = VNode key $ OperativeElement bf $ r # H.style style
    withStyle vnode = vnode

padding :: forall f state a. Functor (f state) => Config f state a -> Int -> VNode f state
padding { rowHeight } units =
  H.el $ H.div # H.style ("height: " <> show height <> "px;")
  where
    height = toNumber units * rowHeight
