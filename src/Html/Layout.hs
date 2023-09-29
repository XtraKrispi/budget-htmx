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
        Html.li
          $ Html.a
          ! Attr.href (routeToPath route)
          ! Html.customAttribute "hx-boost" "true"
          ! Utils.classesPred
            [ ("lowercase", True)
            , ("text-blue-500", activeRoute == route)
            ]
          $ Html.text (T.pack $ show route)
   in Html.nav
        ! Utils.classes
          [ "bg-white"
          , "border-gray-200"
          ]
        $ do
          Html.div
            ! Utils.classes
              [ "container"
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
                  , "text-4xl"
                  , "font-semibold"
                  , "whitespace-nowrap"
                  ]
                $ Html.text "budgit$"
              Html.div
                $ Html.ul
                ! Utils.classes
                  [ "font-medium"
                  , "flex"
                  , "p-4"
                  , "space-x-2"
                  ]
                $ do
                  mapM_ menuItem [HomeR .. ArchiveR]

withLayout :: Route -> Html -> Html
withLayout activeRoute content = Html.docTypeHtml do
  Html.head do
    Html.title "Budget App"
    Html.link ! Attr.rel "stylesheet" ! Attr.href "/app.css"
  Html.body do
    navbar activeRoute
    Html.div ! Utils.classes ["container"] $ do
      content

    Html.script ! Attr.src "https://unpkg.com/htmx.org@1.9.6" $ mempty