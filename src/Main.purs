module Main where

import Prelude (Unit, bind, discard, pure, unit)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(Just))
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element (Element, toNode)
import Web.DOM.Node (appendChild)
import Widget (createDerivedElement, domDocument, makeAutonomousCustomElement, makeDerivedCustomElement, maybeRoot)

main :: Effect Unit
main = do
  doc <- domDocument
  root <- maybeRoot
  -- addCustomElement1 doc root
  addCustomElement2 doc root

addCustomElement1 :: Document -> Maybe Element -> Effect Unit
addCustomElement1 doc (Just root) = do
  let elementName = "test-element"
  log "About to create autonomous custom element"
  makeAutonomousCustomElement elementName
  custom <- createElement elementName doc
  appendChild (toNode custom) (toNode root)
addCustomElement1 _ _ = pure unit

addCustomElement2 :: Document -> Maybe Element -> Effect Unit
addCustomElement2 doc (Just root) = do
  let elementName = "test-p"
  makeDerivedCustomElement "p" elementName
  custom <- createDerivedElement elementName "p" doc
  appendChild (toNode custom) (toNode root)
addCustomElement2 _ _ = pure unit
