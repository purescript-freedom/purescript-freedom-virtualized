module Freedom.Virtualized
  ( Config
  , virtualList
  ) where

import Prelude

import Data.Array (concat, snoc)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (takeRight)
import Effect (Effect)
import Foreign.Object (alter)
import Freedom.Markup as H
import Freedom.UI (Operation, VNode, modifyVObject)
import Web.DOM.Element as E
import Web.Event.Event (Event, target)

-- | The type of virtual list config.
-- |
-- | - `height`: The px height of container
-- | - `rows`: The data for row view
-- | - `rowView`: The renderer of a row
-- | - `calcRowHeight`: The calculator of a row view's px height
-- | - Lifecycles
type Config state a =
  { height :: Number
  , rows :: Array a
  , rowView :: a -> VNode state
  , calcRowHeight :: a -> Number
  , didCreate :: E.Element -> Operation state -> Effect Unit
  , didUpdate :: E.Element -> Operation state -> Effect Unit
  , didDelete :: E.Element -> Operation state -> Effect Unit
  }

type RenderingParams state =
  { topPadding :: Number
  , bottomPadding :: Number
  , renderingHeight :: Number
  , renderingTargets :: Array (VNode state)
  }

-- | Render a virtual list.
virtualList :: forall state a. Config state a -> VNode state
virtualList config =
  H.div
    # H.renderingManually
    # H.style scrollerStyle
    # H.didCreate (didCreate config)
    # H.didUpdate (didUpdate config)
    # H.didDelete (didDelete config)
    # H.onScroll (onScroll config)
  where
    scrollerStyle =
      "overflow:auto;height:" <> show config.height <> "px;"

didCreate
  :: forall state a
   . Config state a
  -> E.Element
  -> Operation state
  -> Effect Unit
didCreate config element operation = do
  renderRows config element operation
  config.didCreate element operation

didUpdate
  :: forall state a
   . Config state a
  -> E.Element
  -> Operation state
  -> Effect Unit
didUpdate config element operation = do
  renderRows config element operation
  config.didUpdate element operation

didDelete
  :: forall state a
   . Config state a
  -> E.Element
  -> Operation state
  -> Effect Unit
didDelete config element operation = do
  operation.renderer.renderChildren (E.toNode element) []
  config.didDelete element operation

onScroll
  :: forall state a
   . Config state a
  -> Event
  -> Operation state
  -> Effect Unit
onScroll config evt operation =
  case E.fromEventTarget <$> target evt of
    Just (Just element) -> renderRows config element operation
    _ -> pure unit

renderRows
  :: forall state a
   . Config state a
  -> E.Element
  -> Operation state
  -> Effect Unit
renderRows config element operation = do
  scrollTop <- E.scrollTop element
  operation.renderer.renderChildren (E.toNode element) $ calcVNodes config scrollTop

calcVNodes :: forall state a. Config state a -> Number -> Array (VNode state)
calcVNodes config scrollTop =
  let params = calcRenderingParams config scrollTop
      topPadding = H.key "_virtualized_top_padding" $ padding params.topPadding
      bottomPadding = H.key "_virtualized_bottom_padding" $ padding params.bottomPadding
   in concat [ [ topPadding ], params.renderingTargets, [ bottomPadding ] ]

styledView :: forall state a. Config state a -> Number -> a -> VNode state
styledView { rowView } rowHeight x =
  withStyle $ rowView x
  where
    style = "height: " <> show rowHeight <> "px;"
    addStyle Nothing = Just style
    addStyle (Just style') = Just $ style' <> if takeRight 1 style' == ";" then style else ";" <> style
    withStyle = modifyVObject \r -> r { props = alter addStyle "style" r.props }

padding :: forall state. Number -> VNode state
padding height =
  H.div # H.style ("height: " <> show height <> "px;")

calcRenderingParams
  :: forall state a
   . Config state a
  -> Number
  -> RenderingParams state
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
  :: forall state a
   . Config state a
  -> Number
  -> RenderingParams state
  -> a
  -> RenderingParams state
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
