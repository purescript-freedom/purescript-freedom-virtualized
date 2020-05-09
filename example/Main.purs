module Main where

import Prelude

import Data.Array ((..))
import Effect (Effect)
import Freedom as Freedom
import Freedom.Markup as H
import Freedom.UI (VNode, Operation)
import Freedom.Virtualized (virtualList)

type State = Int

main :: Effect Unit
main = Freedom.run
  { selector: "#app"
  , initialState
  , subscriptions: []
  , view
  }

-- State

initialState :: State
initialState = 999

addMany :: Operation State -> Effect Unit
addMany { query } = query.reduce (_ + 1000)

-- View

view :: State -> VNode State
view state =
  H.div # H.kids
    [ H.h1 # H.kids [ H.t "Virtual list demo" ]
    , H.button
        # H.onClick (const addMany)
        # H.kids [ H.t "Add 1000" ]
    , H.div # H.css css # H.kids
        [ virtualList
            { height: 500.0
            , rows: 0 .. state
            , rowView: item
            , calcRowHeight: const 50.0
            , didCreate: const $ const $ pure unit
            , didUpdate: const $ const $ pure unit
            , didDelete: const $ const $ pure unit
            }
        ]
    ]
  where
    css = ".&{width: 800px;border: 1px solid #EEE;}"

item :: Int -> VNode State
item i =
  H.div
    # H.key (show i)
    # H.css css
    # H.style "width: 100%;" -- For add style test
    # H.kids [ H.t $ "Item " <> show i ]
  where
    css =
      """
      .& {
        display: flex;
        justify-content: flex-start;
        align-items: center;
        padding: 0 16px;
        border-bottom: 1px solid #EEE;
        box-sizing: border-box;
      }
      .&:last-of-type {
        border-bottom: none;
      }
      """
