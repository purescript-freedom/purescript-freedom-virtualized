module Main where

import Prelude

import Data.Array ((..))
import Effect (Effect)
import Freedom as Freedom
import Freedom.Markup as H
import Freedom.TransformF.Simple (VQueryF, transformF, reduce)
import Freedom.Virtualized (virtualList)
import Freedom.VNode (VNode)

type State = Int

type Html = VNode VQueryF State

main :: Effect Unit
main = Freedom.run
  { selector: "#app"
  , initialState
  , subscriptions: []
  , transformF
  , view
  }

-- State

initialState :: State
initialState = 999

addMany :: State -> State
addMany = (_ + 1000)

-- View

view :: State -> Html
view state =
  H.el $ H.div # H.kids
  [ H.el $ H.h1 # H.kids [ H.t "Virtual list demo" ]
    , H.el $ H.button
        # H.onClick (const $ reduce addMany)
        # H.kids [ H.t "Add 1000" ]
    , H.el $ H.div # H.css css # H.kids
        [ virtualList
            { height: 500.0
            , rowHeight: 50.0
            , rows: 0 .. state
            , rowView: item
            }
        ]
    ]
  where
    css = ".&{width: 800px;border: 1px solid #EEE;}"

item :: Int -> Html
item i =
  H.keyed (show i) $ H.el $ H.div
    # H.css css
    # H.kids [ H.t $ "Item " <> show i ]
  where
    css =
      """
      .& {
        display: flex;
        justify-content: flex-start;
        align-items: center;
        padding: 0 16px;
        width: 100%;
        border-bottom: 1px solid #EEE;
      }
      .&:last-of-type {
        border-bottom: none;
      }
      """
