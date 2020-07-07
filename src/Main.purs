module Main where


import Prelude (Unit, bind, discard, map, unit, ($), (<>), (>>=))

import Data.Array (snoc)
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Halogen.HTML.Properties as P
import Halogen.HTML.Events as E
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML.HTMLElement (focus, fromElement)
import Web.HTML.HTMLDocument (toNonElementParentNode, setTitle)
import Web.HTML.Window (document)
import Web.HTML (window)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type AppState = { names :: Array String
                , newName :: String
                }


data Action = Append | NewName String
-- Web.HTML  window :: Effect Window
-- document :: Window -> Effect HTMLDocument
-- fromDocument :: Document -> Maybe HTMLDocument
-- foreign import setTitle :: String -> HTMLDocument -> Effect Unit
-- toNonElementParentNode :: HTMLDocument -> NonElementParentNode
-- getElementById :: String -> NonElementParentNode -> Effect (Maybe Element)
-- Web.HTML.HTMLElement.purs:fromElement :: Element -> Maybe HTMLElement
-- foreign import focus :: HTMLElement -> Effect Unit
component :: forall query input output. H.Component HH.HTML query input output Aff
component =
  H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
  where
    newInputId = "newNameField"
    initialState _ = { names: ["Daniel", "Agata"]
                     , newName: ""
                     }
    render state =
      HH.div_
        [ HH.div_
          [ HH.input
            [ P.value state.newName
            , P.id_ newInputId
            , P.type_ P.InputText
            , P.autofocus true
            , E.onValueChange \s -> Just $ NewName s
            ]
          , HH.button [E.onClick \_ -> Just Append] [ HH.text "add" ]
          ]
        , HH.div_ (map (\n -> HH.div_ [HH.text n]) state.names)
        ]

    focusOnInput = do
      w <- window
      d <- document w
      setTitle "HELLO WORLD" d
      me <- getElementById newInputId (toNonElementParentNode d)
      case me >>= fromElement of
        Nothing -> log $ "input with id [" <> newInputId <> "] is not found"
        Just e -> focus e

    handleAction ev = do
      st <- H.get
      case ev of
        Append -> do
          case st.newName of
            "" -> liftEffect $ log "Empty new name"
            _  -> do
              liftEffect focusOnInput
              H.put st { newName = "", names = snoc st.names st.newName }
        NewName s -> do
          liftEffect $ log ("Update new name: [" <> s <> "]")
          H.put st { newName = s }
