module ConsDecons where

data GuessWhat = Chickenbutt
  deriving (Eq, Show)

data Id a = MkId a
  deriving (Eq, Show)

data Product a b = Product a b
  deriving (Eq, Show)

data Sum a b = First a | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b }
  deriving (Eq, Show)


newtype NumCow = NumCow Int
  deriving (Eq, Show)

newtype NumPig = NumPig Int
  deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)


type Name = String
type Age = Int
type LovesMud = Bool

-- Sheep can produce between 2 and 30
-- pounds (0.9 and 13 kilos) of wool per year!
-- Icelandic sheep don't produce as much
-- wool per year as other breeds but the
-- wool they do produce is a finer wool.

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age
  deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo
  deriving (Eq, Show)

-- Alternately
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)


data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os   :: OperatingSystem
             , lang :: ProgrammingLanguage }
             deriving (Eq, Show)

-- Write a function that generates all possible values of Programmer. Use the
-- provided lists of inhabitants of OperatingSystem and ProgrammingLanguage.

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = o, lang = l } | o <- allOperatingSystems, l <- allLanguages ]
