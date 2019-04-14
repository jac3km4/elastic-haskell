{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                 (void)
import           Data.Text                     (Text)
import           GI.Gtk                        (ApplicationWindow (..),
                                                Box (..), Label (..),
                                                MenuBar (..), MenuItem (..),
                                                Orientation (..))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

newtype State = Message Text

data Event = Closed

view' :: State -> AppView ApplicationWindow Event
view' (Message msg) =
  bin
      ApplicationWindow
      [ #title := "MenuBar"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 400
      , #heightRequest := 300
      ]
    $ container
        Box
        [#orientation := OrientationVertical]
        [ BoxChild defaultBoxChildProperties { expand = True }
          $ widget Label [#label := msg]
        ]

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = Message "Click a button in the menu."
  }
