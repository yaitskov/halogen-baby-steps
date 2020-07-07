module Main where


import Prelude (Unit, bind, discard, unit, show, ($), (<>), (>>=), identity)
import Data.Array (snoc, deleteAt, mapWithIndex, (!!))
import Effect (Effect)
import Effect.Console (log)
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..), maybe)
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
import Web.UIEvent.KeyboardEvent (key)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

type AppState = { names :: Array String
                , newName :: String
                }


data Action = Append | NewName String | Remove Int

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
    mapEnterToAdd ke =
      case key ke of
        "Enter" -> Just Append
        _ -> Nothing


    listItem i n =
      HH.div_
        [ HH.button [E.onClick \_ -> Just $ Remove i] [ HH.text "x" ]
        , HH.text n
        ]
    render state =
      HH.div_
        [ HH.div_
          [ HH.input
            [ P.value state.newName
            , P.id_ newInputId
            , P.type_ P.InputText
            , P.autofocus true
            , E.onKeyUp mapEnterToAdd
            , E.onValueChange \s -> Just $ NewName s
            ]
          , HH.button [E.onClick \_ -> Just Append] [ HH.text "add" ]
          ]
        , HH.div_ $ mapWithIndex listItem state.names
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
        Remove i -> do
          liftEffect $ log ("Remove Item [" <> show (st.names !! i)
                            <> "] at index " <> show i)
          H.put st { names = maybe [] identity $ deleteAt i st.names }
        NewName s -> do
          liftEffect $ log ("Update new name: [" <> s <> "]")
          H.put st { newName = s }
