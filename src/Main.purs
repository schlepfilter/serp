module Main where

import Prelude

import Data.Array (filterA, head, take)
import Data.FoldableWithIndex (traverseWithIndex_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null)
import Effect (Effect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Web.DOM (Document, Element)
import Web.DOM.Document (createElement, createTextNode, getElementsByTagName)
import Web.DOM.Element (fromNode, getAttribute, setAttribute, tagName)
import Web.DOM.Element as Element
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Node (appendChild, parentNode)
import Web.DOM.Text as Text
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (HTMLDocument, window)
import Web.HTML.HTMLDocument (activeElement, toDocument)
import Web.HTML.HTMLElement (fromElement, getBoundingClientRect, toElement)
import Web.HTML.Location (setHref)
import Web.HTML.Window (document, location, scrollY, toEventTarget)
import Web.UIEvent.KeyboardEvent (fromEvent, key)

foreign import insertAdjacentElementImpl :: EffectFn3 Element String Element Unit

insertAdjacentElement :: Element -> String -> Element -> Effect Unit
insertAdjacentElement = runEffectFn3 insertAdjacentElementImpl

effectHTMLDocument :: Effect HTMLDocument
effectHTMLDocument = window >>= document

effectDocument :: Effect Document
effectDocument = toDocument <$> effectHTMLDocument

handle :: Int -> String -> Event -> Effect Unit
handle i href e = case fromEvent e of
    Nothing -> pure unit
    Just keyboardEvent
        | show i == key keyboardEvent -> do
            htmlDoc <- effectHTMLDocument
            loc <- window >>= location
            maybeActiveElement <- activeElement htmlDoc
            case maybeActiveElement of
                Nothing -> pure unit
                Just htmlElement
                    | tagName (toElement htmlElement) == "INPUT" -> pure unit
                    | otherwise -> setHref href loc
        | otherwise -> pure unit

effectHref :: Element -> Effect String
effectHref h3 = do
    maybeA <- parentNode (Element.toNode h3)
    case maybeA >>= fromNode of
        Nothing -> pure ""
        Just element -> fromMaybe "" <$> getAttribute "href" element

insertDiv :: Int -> Element -> Effect Unit
insertDiv i result = do
    doc <- effectDocument
    div <- createElement "div" doc
    text <- createTextNode (show $ i + 1) doc
    appendChild (Text.toNode text) (Element.toNode div)
    y <- window >>= scrollY
    case getBoundingClientRect <$> fromElement result of
        Nothing -> pure unit
        Just rectEffect -> do
            rect <- rectEffect
            -- TODO adjust the color depending on the background-color
            setAttribute "style" ("color: white; left: " 
            <> show (rect.left - 12.0)
            <> "px;" <> "top: " 
            <> show (y + rect.top)
            <> "px; position: absolute;") div
    tags <- getElementsByTagName "body" doc >>= toArray
    case head tags of
        Nothing -> pure unit
        Just body -> insertAdjacentElement body "beforebegin" div
    href <- effectHref result
    listener <- eventListener $ handle (i + 1) href
    target <- toEventTarget <$> window
    addEventListener (EventType "keydown") listener false target

hasHref :: Element -> Effect Boolean
hasHref h3 = null >>> not <$> (effectHref h3)

main :: Effect Unit
main = do
    doc <- effectDocument
    h3s <- getElementsByTagName "h3" doc >>= toArray
    results <- filterA hasHref $ h3s
    traverseWithIndex_ insertDiv $ take 9 results
    -- TODO reinsert divs on click
