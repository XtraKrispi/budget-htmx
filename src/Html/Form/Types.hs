module Html.Form.Types where

import Text.Blaze.Html5 (Attribute, AttributeValue, Html)

type Form = FormData

data FormData
  = HBox [FormData]
  | VBox [FormData]
  | Field FormField

data FormField
  = Input FormInput
  | Select FormSelect
  | SubmitButton FormButton
  | OtherButton FormButton

data FormInput = FormInput
  { formInputInputType :: AttributeValue
  , formInputId :: Maybe AttributeValue
  , formInputName :: Maybe AttributeValue
  , formInputLabel :: Html
  , formInputIsReadonly :: Bool
  , formInputIsRequired :: Bool
  , formInputValue :: Maybe AttributeValue
  }

data FormSelectOption = FormSelectOption
  { formSelectOptionText :: Html
  , formSelectOptionValue :: AttributeValue
  , formSelectOptionSelected :: Bool
  }

data FormSelect = FormSelect
  { formSelectId :: Maybe AttributeValue
  , formSelectName :: Maybe AttributeValue
  , formSelectLabel :: Html
  , formSelectOptions :: [FormSelectOption]
  }

data FormButton = FormButton
  { formButtonLabel :: Html
  , formButtonIsDisabled :: Bool
  , formButtonAttribute :: Attribute
  }

input :: FormInput -> FormData
input = Field . Input

select :: FormSelect -> FormData
select = Field . Select

submitButton :: FormButton -> FormData
submitButton = Field . SubmitButton

otherButton :: FormButton -> FormData
otherButton = Field . OtherButton