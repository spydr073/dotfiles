{-# LANGUAGE GADTs, FlexibleInstances #-}

-----------------------------------------------------------------------------------------[ Module ]
--{1

module XMobarUtils where

import Text.Printf

--}

-------------------------------------------------------------------------------------[ Formatting ]
--{1

quote :: String -> String
quote x = "\"" ++ x ++ "\""

color :: String -> String -> String -> String
color fg bg x = "<fc=" ++ fg ++ "," ++ bg ++ ">" ++ x ++ "</fc>"

icon :: String -> String
icon i = "<icon=" ++ i ++".xbm/>"
--
--}

-----------------------------------------------------------------------------------------[ Xmobar ]
--{1

data Command where
  XBarCmd :: String -> [String] -> Int -> Command
  UsrCmd  :: String -> [String] -> String -> Int -> Command
  DateCmd :: String -> Int -> Command
  STDIN   :: Command

instance Show Command where
  show STDIN               = " Run StdinReader\n"
  show (DateCmd fmt i)     = " Run Date " ++ quote fmt ++ " \"date\" " ++ show i ++ "\n"
  show (UsrCmd s args n i) = " Run Com " ++ quote s ++ " " ++ show args ++ " " ++
                             quote n ++ " " ++ show i ++ "\n"
  show (XBarCmd n args i)  = " Run " ++ n ++ " " ++ show args ++ " " ++ show i ++ "\n"

class ToString a where
    toString :: a -> String

instance ToString Char where
  toString c = c:[]

instance ToString (Char,Char) where
  toString p = (fst p) : (snd p) : []


data XMobar = XMobar { font             :: String
                     , additionalFonts  :: [String]
                     , bgColor          :: String
                     , fgColor          :: String
                     , alpha            :: Int
                     , position         :: String
                     , textOffset       :: Int
                     , iconOffset       :: Int
                     , lowerOnStart     :: Bool
                     , hideOnStart      :: Bool
                     , allDesktops      :: Bool
                     , overrideRedirect :: Bool
                     , pickBroadest     :: Bool
                     , persistent       :: Bool
                     , border           :: String
                     , borderColor      :: String
                     , borderWidth      :: Int
                     , iconRoot         :: String
                     , commands         :: [Command]
                     , sepChar          :: Char
                     , alignSep         :: (Char,Char)
                     , template         :: [String]
                     }

instance ToString XMobar where
  toString b = "Config { font             = " ++ quote (font b)
            ++ foldl (\x y -> x ++ "\n       , " ++ y) ""
                     [ "additionalFonts  = " ++ show (additionalFonts b)
                     , "bgColor          = " ++ quote (bgColor b)
                     , "fgColor          = " ++ quote (fgColor b)
                     , "alpha            = " ++ show (alpha b)
                     , "position         = " ++ position b
                     , "textOffset       = " ++ show (textOffset b)
                     , "iconOffset       = " ++ show (iconOffset b)
                     , "lowerOnStart     = " ++ show (lowerOnStart b)
                     , "hideOnStart      = " ++ show (hideOnStart b)
                     , "allDesktops      = " ++ show (allDesktops b)
                     , "overrideRedirect = " ++ show (overrideRedirect b)
                     , "pickBroadest     = " ++ show (pickBroadest b)
                     , "persistent       = " ++ show (persistent b)
                     , "border           = " ++ border b
                     , "borderColor      = " ++ quote (borderColor b)
                     , "borderWidth      = " ++ show (borderWidth b)
                     , "iconRoot         = " ++ show (iconRoot b)
                     , "sepChar          = " ++ quote (toString (sepChar b))
                     , "alignSep         = " ++ quote (toString (alignSep b))
                     , "template         = " ++ quote (foldl (++) "" (template b))
                     , "commands         = " ++ show (commands b)
                     ]
            ++ "\n}"

instance Show XMobar where
  show = toString

--}



