module Html.Layout where

import Data.Text qualified as T
import Html.Utils qualified as Utils
import Route (Route (..), routeToPath)
import Text.Blaze.Html5 (Html, (!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr

navbar :: Route -> Html
navbar activeRoute =
  let menuItem route =
        Html.li $ Html.a ! Attr.href (routeToPath route) ! Utils.classes (["text-blue-700" | route == activeRoute]) $ Html.text (T.pack $ show route)
   in Html.nav
        ! Utils.classes
          [ "bg-white"
          , "border-gray-200"
          ]
        $ do
          Html.div
            ! Utils.classes
              [ "max-w-screen-xl"
              , "flex"
              , "flex-wrap"
              , "items-center"
              , "space-x-4"
              , "mx-auto"
              , "p4"
              ]
            $ do
              Html.a
                ! Utils.classes ["flex", "items-center"]
                $ Html.span
                ! Utils.classes
                  [ "self-center"
                  , "text-2xl"
                  , "font-semibold"
                  , "whitespace-nowrap"
                  ]
                $ Html.text "Budget"
              Html.div
                $ Html.ul
                ! Utils.classes
                  [ "font-medium"
                  , "flex"
                  , "p-4"
                  , "space-x-2"
                  ]
                $ do
                  mapM_ menuItem [Home .. Archive]

withLayout :: Route -> Html -> Html
withLayout activeRoute content = Html.docTypeHtml do
  Html.head do
    Html.title "Budget App"
    Html.link ! Attr.rel "stylesheet" ! Attr.href "/app.css"
  Html.body do
    navbar activeRoute
    Html.div do
      content