module Html.Form.Types where

import Text.Blaze.Html5 (AttributeValue, Html)

type Form = [FormField]

data FormField
  = Input FormInput
  | Select FormSelect
  | SubmitButton FormSubmit

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

data FormSubmit = FormSubmit
  { formSubmitLabel :: Html
  , formSubmitIsDisabled :: Bool
  }

input :: FormInput -> FormField
input = Input

select :: FormSelect -> FormField
select = Select

submitButton :: FormSubmit -> FormField
submitButton = SubmitButton