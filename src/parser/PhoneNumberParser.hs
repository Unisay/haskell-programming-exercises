module PhoneNumberParser where

import Text.Trifecta

type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
                   deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = undefined
