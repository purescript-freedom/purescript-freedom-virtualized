module Freedom.Virtualized
  ( Config
  , virtualList
  ) where

import Prelude

import Control.Monad.Free.Trans (FreeT)
import Control.Monad.Trans.Class (lift)
import Data.Array (concat, snoc)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (takeRight)
import Effect.Class (liftEffect)
import Foreign.Object (alter)
import Freedom.Markup as H
import Freedom.VNode (VElement(..), VNode(..), VRender, operations)
import Web.DOM.Element as E
import Web.Event.Event (Event, target)

-- | The type of virtual list config.
-- |
-- | - `height`: The px height of container
-- | - `rows`: The data for row view
-- | - `rowView`: The renderer of a row
-- | - `calcRowHeight`: The calculator of a row view's px height
-- | - Lifecycles
type Config f state a =
  { height :: Number
  , rows :: Array a
  , rowView :: a -> VNode f state
  , calcRowHeight :: a -> Number
  , didCreate :: E.Element -> FreeT (f state) (VRender f state) Unit
  , didUpdate :: E.Element -> FreeT (f state) (VRender f state) Unit
  , didDelete :: E.Element -> FreeT (f state) (VRender f state) Unit
  }

type RenderingParams f state =
  { topPadding :: Number
  , bottomPadding :: Number
  , renderingHeight :: Number
  , renderingTargets :: Array (VNode f state)
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
  renderRows config element
  config.didCreate element

didUpdate
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> E.Element
  -> FreeT (f state) (VRender f state) Unit
didUpdate config element =do
  renderRows config element
  config.didUpdate element

didDelete
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> E.Element
  -> FreeT (f state) (VRender f state) Unit
didDelete config element = do
  r <- lift operations
  liftEffect $ r.renderChildren (E.toNode element) []
  config.didDelete element

onScroll
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> Event
  -> FreeT (f state) (VRender f state) Unit
onScroll config evt =
  case E.fromEventTarget <$> target evt of
    Just (Just element) -> renderRows config element
    _ -> pure unit

renderRows
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> E.Element
  -> FreeT (f state) (VRender f state) Unit
renderRows config element = do
  scrollTop <- liftEffect $ E.scrollTop element
  r <- lift operations
  liftEffect $ r.renderChildren (E.toNode element) $ calcVNodes config scrollTop

calcVNodes :: forall f state a. Functor (f state) => Config f state a -> Number -> Array (VNode f state)
calcVNodes config scrollTop =
  let params = calcRenderingParams config scrollTop
      topPadding = H.keyed "_virtualized_top_padding" $ padding params.topPadding
      bottomPadding = H.keyed "_virtualized_bottom_padding" $ padding params.bottomPadding
   in concat [ [ topPadding ], params.renderingTargets, [ bottomPadding ] ]

styledView :: forall f state a. Functor (f state) => Config f state a -> Number -> a -> VNode f state
styledView { rowView } rowHeight x =
  withStyle $ rowView x
  where
    style = "height: " <> show rowHeight <> "px;"
    addStyle Nothing = Just style
    addStyle (Just style') = Just $ style' <> if takeRight 1 style' == ";" then style else ";" <> style
    withStyle (VNode key (Element r)) = VNode key $ Element $ r { props = alter addStyle "style" r.props }
    withStyle (VNode key (OperativeElement bf r)) = VNode key $ OperativeElement bf $ r { props = alter addStyle "style" r.props }
    withStyle vnode = vnode

padding :: forall f state. Functor (f state) => Number -> VNode f state
padding height =
  H.el $ H.div # H.style ("height: " <> show height <> "px;")

calcRenderingParams
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> Number
  -> RenderingParams f state
calcRenderingParams config scrollTop =
  foldl
    (calcRenderingParamsByItem config scrollTop)
    { topPadding: 0.0
    , bottomPadding: 0.0
    , renderingHeight: 0.0
    , renderingTargets: []
    }
    config.rows

calcRenderingParamsByItem
  :: forall f state a
   . Functor (f state)
  => Config f state a
  -> Number
  -> RenderingParams f state
  -> a
  -> RenderingParams f state
calcRenderingParamsByItem config scrollTop params x =
  if params.topPadding + rowHeight < scrollTop then
    params { topPadding = params.topPadding + rowHeight }
  else if params.renderingHeight < config.height + rowHeight then
    params
      { renderingHeight = params.renderingHeight + rowHeight
      , renderingTargets = snoc params.renderingTargets $ styledView config rowHeight x
      }
  else
    params { bottomPadding = params.bottomPadding + rowHeight }
  where
    rowHeight = config.calcRowHeight x
