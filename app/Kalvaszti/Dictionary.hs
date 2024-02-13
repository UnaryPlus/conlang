{-
This module is not up-to-date and will be revised entirely.
-}

module Kalvaszti.Dictionary where

-- kal: green
-- rus: white
-- sei: island
-- not: archipelago
-- ras: person
-- nofxa: harbor
-- kadai: castle
-- faʃti: language
-- təx: town
-- ŋʷat: city
-- kʷouda: outer/exterior/outside (adj/v)
-- kef: floor
-- pau: land/country

-- hʷom: world/universe
-- sedem: western continent (loan)
-- terba: eastern continent (loan: kǀʰerəbaʔ)

-- kalvaʃti
-- kauzei
-- kalnot
-- kalnofxa
-- ruskadai
-- kʷoudazeine
-- hʷoŋwa kef
-- rasxe

-- kauzei < kalnot < "kal"

data Class = N | V
type Definition = String

data Affix
  = Pre String
  | Suf String
  | PreS String

data Variant = With [Affix] Definition

data Entry
  = Section String
  | E String Class Definition [Variant]

dictionary :: [Entry]
dictionary =
  [ Section "Kinship"

  , E "paana" N "mother" []
  , E "talaa" N "father" []

    --ancestors
  , E "kʷosito" N "mother's mother"
      [ With [PreS "tasa"] "father's mother"
      , With [Suf "helo"] "mother's grandmother"
      , With [PreS "tasa", Suf "helo"] "father's grandmother" ]
  , E "laera" N "mother's father"
      [ With [PreS "tasa"] "father's father"
      , With [Suf "helo"] "mother's grandfather"
      , With [PreS "tasa", Suf "helo"] "father's grandfather" ]

    --siblings
  , E "kʷoheta" N "sister"
      [ With [Suf "simi"] "brother's wife"
      , With [Suf "helo"] "older sister"
      , With [Suf "fea"] "younger sister" ]
  , E "narofi" N "brother"
      [ With [Suf "simi"] "sister's husband"
      , With [Suf "helo"] "older brother"
      , With [Suf "fea"] "younger brother" ]

    --aunts and uncles
  , E "kʷalile" N "mother's sister"
      [ With [PreS "tasa"] "father's sister"
      , With [Suf "simi"] "mother's brother's wife"
      , With [PreS "tasa", Suf "simi"] "father's brother's wife" ]
  , E "xʷinefe" N "mother's brother"
      [ With [PreS "tasa"] "father's brother"
      , With [Suf "simi"] "mothers's sister's husband"
      , With [PreS "tasa", Suf "simi"] "father's sister's husband" ]

    --cousins
  , E "miefehi" N "mother's niece"
      [ With [PreS "tasa"] "father's niece"
      , With [Suf "simi"] "mother's nephew's wife"
      , With [PreS "tasa", Suf "simi"] "father's nephew's wife" ]
  , E "xʷoeno" N "mother's nephew"
      [ With [PreS "tasa"] "father's nephew"
      , With [Suf "simi"] "mother's niece's husband"
      , With [PreS "tasa", Suf "simi"] "father's niece's husband" ]

  ]


{-
  , E "kʷoheta" N "sister" []
  , E "narofi" N "brother"
      [ With ["simi"] "sister's husband" ]
  , E "miefehi" N "mother's sister's daughter" []
  , E "xʷoeno" N "mother's sister's son"
      [ With ["simi"] "mother's sister's daughter's husband" ]

  , E "paana" N "mother"
      [ With ["helo"] "mother's mother" ]
  , E "talaa" N "father"
      [ With ["helo"] "mother's father" ]
  , E "kʷalile" N "mother's sister"
      [ With ["helo"] "mother's mother's sister" ]
  , E "xʷinexʷe" N "mother's brother"
      [ With ["simi"] "mother's sister's husband"
      , With ["helo"] "mother's mother's brother"
      , With ["helo", "simi"] "mother's mother's sister's husband" ]

  , E "ŋanise" N "sister's daughter"
      [ With ["fea"] "sister's daughter's daughter" ]
  , E "hʷeitekʷo" N "sister's son"
      [ With ["simi"] "sister's daughter's husband"
      , With ["fea"] "sister's daugher's son"
      , With ["fea", "simi"] "sister's daughter's daughter's husband" ]

  , E "toexa" N "daughter"
      [ With ["fea"] "daughter's daughter" ]
  , E "nosa" N "son"
      [ With ["simi"] "daughter's husband"
      , With ["fea"] "daughter's son"
      , With ["fea", "simi"] "daughter's daughter's husband" ]

  , E "risote" N "wife" []
  , E "hʷofiti" N "husband" []
  , E "kamao" N "family" []

  --PEOPLE
  --NOTE: -fea can be added to any of these words

  , E "siaka" N "baby" []
  , E "noto" N "child" []
  , E "nosai" N "young woman (not yet married)" []
  , E "haxiana" N "young man (not yet married)" []
  , E "hʷafa" N "woman"
      [ With ["xʷa"] "old woman" ]
  , E "soŋʷo" N "man"
      [ With ["xʷa"] "old man" ]

  --GEOGRAPHY
  , E "peefe" N "small island" []
  , E "noko" N "large island" []
  , E "kalaha" N "archipelago" []
  , E "maisia" N "part of an island; also 'neck'" []
  , E "xʷarane" N "isthmus, peninsula, cap"
  ]

  -}
