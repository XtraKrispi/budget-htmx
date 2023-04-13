{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Admin.Definitions where

import Data.Foldable (traverse_)
import Data.Maybe (isJust)
import Data.Text (pack)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), formatShow)
import Helpers (asCurrency, emptyAttribute, tshow)
import Htmx
import Lucid
import Types

editCreatePanel :: Maybe Definition -> Html ()
editCreatePanel mDefinition =
  let formLabelClass = "peer-focus:font-medium absolute text-sm text-gray-500 duration-300 transform -translate-y-6 scale-75 top-3 -z-10 origin-[0] peer-focus:left-0 peer-focus:text-blue-600 peer-placeholder-shown:scale-100 peer-placeholder-shown:translate-y-0 peer-focus:scale-75 peer-focus:-translate-y-6"
      formInputClass = "block py-2.5 px-0 w-full text-sm text-gray-900 bg-transparent border-0 border-b-2 border-gray-300 appearance-none focus:outline-none focus:ring-0 focus:border-blue-600 peer"
      formRoute = case mDefinition of
        Just def -> hxPut $ "/api/definitions/" <> tshow def.id
        Nothing -> hxPost "/api/definitions"
   in form_
        [ formRoute
        , hxSwap "outerHTML"
        , class_ "flex flex-col"
        ]
        do
          div_ [class_ "flex gap-2"] do
            div_ [class_ "relative z-0 w-full mb-6 group"] do
              input_
                [ maybe emptyAttribute (value_ . (.description)) mDefinition
                , name_ "description"
                , type_ "text"
                , class_ formInputClass
                , placeholder_ " "
                , required_ "required"
                ]
              label_
                [ for_ "description"
                , class_ formLabelClass
                ]
                "Description"
            div_ [class_ "relative z-0 w-full mb-6 group"] do
              input_
                [ maybe emptyAttribute (value_ . tshow . (.amount)) mDefinition
                , name_ "amount"
                , type_ "number"
                , class_ formInputClass
                , placeholder_ " "
                , required_ "required"
                , step_ "0.01"
                ]
              label_
                [ for_ "amount"
                , class_ formLabelClass
                ]
                "Amount"
            div_ [class_ "relative z-0 w-full mb-6 group flex flex-col justify-end"] do
              select_
                [ name_ "frequency"
                , class_ "bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-1.5"
                ]
                do
                  traverse_
                    ( \f ->
                        option_
                          [ maybe
                              emptyAttribute
                              ( \d ->
                                  if d.frequency == f
                                    then selected_ "selected"
                                    else emptyAttribute
                              )
                              mDefinition
                          ]
                          (toHtml $ frequencyToText f)
                    )
                    [OneTime .. BiWeekly]
              label_
                [ for_ "frequency"
                , class_ formLabelClass
                ]
                "Frequency"
            div_ [class_ "relative z-0 w-full mb-6 group"] do
              input_
                [ maybe emptyAttribute (value_ . pack . formatShow iso8601Format . (.startDate)) mDefinition
                , name_ "startDate"
                , type_ "date"
                , class_ formInputClass
                , required_ "required"
                ]
              label_
                [ for_ "startdate"
                , class_ formLabelClass
                ]
                "Start Date"
          div_ [class_ "flex gap-2"] do
            button_
              [type_ "submit", class_ "transition-all bg-blue-500 hover:bg-blue-700 text-white p-2 rounded-lg"]
              (if isJust mDefinition then "Save" else "Create")
            button_
              [ type_ "button"
              , hxGet "/api/definitions/clearform"
              , hxTarget "closest form"
              , hxSwap "outerHTML"
              , class_ "transition-all text-white bg-gray-400 hover:bg-gray-600 border rounded-lg p-2"
              ]
              "Clear"

renderDefinition :: Definition -> Html ()
renderDefinition def =
  div_ [class_ "group p-4 border rounded-lg drop-shadow-lg bg-white flex flex-col"] do
    button_
      [ hxDelete ("/api/definitions/" <> tshow def.id)
      , class_ "transition-all duration-100 flex opacity-0 group-hover:opacity-100 hover:bg-gray-200 absolute border right-0 -top-[13px] bg-white p-1 rounded-full w-8 h-8 text-xs justify-center items-center"
      ]
      do
        "X"

    div_ [class_ "flex gap-2 justify-between"] do
      a_
        [ href_ ""
        , hxGet ("/api/definitions/" <> tshow def.id)
        , hxTarget "#edit-create-panel form"
        , hxSwap "outerHTML"
        , class_ "hover:underline"
        ]
        (strong_ $ toHtml def.description)
      span_ $ strong_ (toHtml $ "$" <> asCurrency def.amount)
    div_ [class_ "flex justify-between"] do
      div_ [] (toHtml $ frequencyToText def.frequency)
      span_ (toHtml $ tshow def.startDate)
