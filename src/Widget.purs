module Widget where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Console (log)
import Web.DOM (Element, Node, Document)
import Web.DOM.Document (createElement, toParentNode)
import Web.DOM.Element (ShadowRootInit, attachShadow, tagName, toNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.DOM.ShadowRoot (ShadowRootMode(..))
import Web.DOM.ShadowRoot (host, mode, toNode) as SR
import Web.Event.CustomEvent (toEvent)
import Web.Event.EventTarget (dispatchEvent)
import Web.HTML (window)
import Web.HTML.HTMLCustomElement (Constructor, CustomElementInit, Method, PropertyOps, createCustomElement, defaultCustomElementInit, setProperty, getProperty, addManagedProperty, addMethod, makeCustomEvent)
import Web.HTML.HTMLDocument (HTMLDocument, toDocument)
import Web.HTML.HTMLElement (toElement, toEventTarget)
import Web.HTML.Window (document)

htmlDocument :: Effect HTMLDocument
htmlDocument = do
  win <- window
  doc <- document win
  pure doc

domDocument :: Effect Document
domDocument = map toDocument htmlDocument

maybeRoot :: Effect (Maybe Element)
maybeRoot = do
  doc <- domDocument
  root <- querySelector (QuerySelector "#root") (toParentNode doc)
  pure root

shadowRootInit :: ShadowRootInit
shadowRootInit = { mode: Open, delegatesFocus: false }

helloWorld :: Method String String
helloWorld name elem = do
  prop <- getProperty "testProp" elem :: Effect { ciao :: String }
  pure $ "hello " <> name <> " from " <> prop.ciao

propGetter :: Method Unit String
propGetter _ el = do
  log "Getting property..."
  event <- makeCustomEvent "altro_eventoz" Nothing
  eventResult <- dispatchEvent (toEvent event) (toEventTarget el)
  getProperty "_innerProp" el

propSetter :: Method String Unit
propSetter arg el = do
  log $ "Setting property to: " <> arg
  setProperty "_innerProp" arg el
  event <- makeCustomEvent "eventoz" (Just "CIAO!!!")
  eventResult <- dispatchEvent (toEvent event) (toEventTarget el)
  log $ "Event dispatched result: " <> (show eventResult)

propOps :: PropertyOps String
propOps = {
  getter: propGetter,
  setter: propSetter
}

makeBuilderWithShadow :: (Node -> Effect Unit) -> Constructor
makeBuilderWithShadow f = \el -> do
  addManagedProperty "mngProp" propOps el
  setProperty "testProp" { ciao: "Sono io" } el
  addMethod "testMethod" helloWorld el
  shadowRoot <- attachShadow shadowRootInit (toElement el)
  f $ SR.toNode shadowRoot
  theHost <- (SR.host shadowRoot)
  log (tagName theHost)
  log (maybe "No mode found" show $ SR.mode shadowRoot)

loggingProperties :: CustomElementInit
loggingProperties = defaultCustomElementInit {
  onConnected = \_ -> log "Connected!",
  onDisconnected = \_ -> log "Disconnected!",
  onAttributeChanged = \_ _ _ _ -> log "Attribute changed!",
  observedAttributes = ["", ""]
}

addStuffToShadowRoot :: Node -> Effect Unit
addStuffToShadowRoot root = do
  doc <- domDocument
  p <- createElement "p" doc
  let pNode = toNode p
  setTextContent "Sono nell'ombra" pNode
  appendChild pNode root 

makeAutonomousCustomElement :: String -> Effect Unit
makeAutonomousCustomElement elementName = do
  let ctorWithShadow = makeBuilderWithShadow addStuffToShadowRoot
      props = defaultCustomElementInit { constructor = ctorWithShadow }
  log elementName
  createCustomElement elementName props

makeDerivedCustomElement :: String -> String -> Effect Unit
makeDerivedCustomElement parentElName elName = 
  let props = defaultCustomElementInit { extends = Just parentElName }
  in createCustomElement elName props

foreign import createDerivedElement :: String -> String -> Document -> Effect Element