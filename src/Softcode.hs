{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module Softcode where
import qualified Data.Text as T
import Data.Monoid
import Data.Foldable

class AsText a where
  toText :: a -> T.Text
  toTextExtraBrace :: a -> T.Text
  toTextExtraBrace = toText

data ObjName = ObjName { objName :: T.Text } | ObjRef { objRefId :: Int } | ObjMe | ObjHere |
               ObjExpr { objExpr :: SoftExpr }
instance AsText ObjName where
  toText (ObjName n) = n
  toText (ObjRef n) = "#" <> (T.pack . show $ n)
  toText ObjMe = "me"
  toText ObjHere = "here"
  toText (ObjExpr e) = "[" <> toText e <> "]"

data Attr = Attr { attrName :: T.Text }
instance AsText Attr where
  toText (Attr n) = n

data FlagType = FlagAbode | FlagAccents | FlagANSI | FlagASCII
  | FlagAudible | FlagAuditorium | FlagBleed | FlagBlind | FlagCommands
  | FlagChownOk | FlagColor256 | FlagConnected | FlagDark | FlagDestroyOk
  | FlagEnterOk | FlagFixed | FlagFloating | FlagGagged | FlagGoing
  | FlagHalted | FlagHaven | FlagHead | FlagHtml | FlagImmortal
  | FlagInherit | FlagJumpOk | FlagKeepalive | FlagKey | FlagLight
  | FlagLinkOk | FlagMarker0 | FlagMarker1 | FlagMarker2 | FlagMarker3
  | FlagMarker4 | FlagMarker5 | FlagMarker6 | FlagMarker7 | FlagMarker8
  | FlagMarker9 | FlagMonitor | FlagMyopic | FlagNoCommand | FlagNoBleed
  | FlagNoSpoof | FlagOpaque | FlagOpenOk | FlagParentOk | FlagPuppet
  | FlagQuiet | FlagRobot | FlagRoyalty | FlagSafe
  | FlagSitemon | FlagSpoof | FlagStaff | FlagSticky | FlagTerse
  | FlagTrace | FlagTransparent | FlagUnfindable | FlagUnicode
  | FlagUninspected | FlagVacation | FlagVerbose | FlagVisual | FlagWizard
instance AsText FlagType where
  toText FlagAbode = "ABODE"
  toText FlagAccents = "ACCENTS"
  toText FlagANSI = "ANSI"
  toText FlagASCII = "ASCII"
  toText FlagAudible = "AUDIBLE"
  toText FlagAuditorium = "AUDITORIUM"
  toText FlagBleed = "BLEED"
  toText FlagBlind = "BLIND"
  toText FlagCommands = "COMMANDS"
  toText FlagChownOk = "CHOWN_OK"
  toText FlagColor256 = "COLOR256"
  toText FlagConnected = "CONNECTED"
  toText FlagDark = "DARK"
  toText FlagDestroyOk = "DESTROY_OK"
  toText FlagEnterOk = "ENTER_OK"
  toText FlagFixed = "FIXED"
  toText FlagFloating = "FLOATING"
  toText FlagGagged = "GAGGED"
  toText FlagGoing = "GOING"
  toText FlagHalted = "HALTED"
  toText FlagHaven = "HAVEN"
  toText FlagHead = "HEAD"
  toText FlagHtml = "HTML"
  toText FlagImmortal = "IMMORTAL"
  toText FlagInherit = "INHERIT"
  toText FlagJumpOk = "JUMP_OK"
  toText FlagKeepalive = "KEEPALIVE"
  toText FlagKey = "KEY"
  toText FlagLight = "LIGHT"
  toText FlagLinkOk = "LINK_OK"
  toText FlagMarker0 = "MARKER0"
  toText FlagMarker1 = "MARKER1"
  toText FlagMarker2 = "MARKER2"
  toText FlagMarker3 = "MARKER3"
  toText FlagMarker4 = "MARKER4"
  toText FlagMarker5 = "MARKER5"
  toText FlagMarker6 = "MARKER6"
  toText FlagMarker7 = "MARKER7"
  toText FlagMarker8 = "MARKER8"
  toText FlagMarker9 = "MARKER9"
  toText FlagMonitor = "MONITOR"
  toText FlagMyopic = "MYOPIC"
  toText FlagNoCommand = "NO_COMMAND"
  toText FlagNoBleed = "NOBLEED"
  toText FlagNoSpoof = "NOSPOOF"
  toText FlagOpaque = "OPAQUE"
  toText FlagOpenOk = "OPEN_OK"
  toText FlagParentOk = "PARENT_OK"
  toText FlagPuppet = "PUPPET"
  toText FlagQuiet = "QUIET"
  toText FlagRobot = "ROBOT"
  toText FlagRoyalty = "ROYALTY"
  toText FlagSafe = "SAFE"
  toText FlagSitemon = "SITEMON"
  toText FlagSpoof = "SPOOF"
  toText FlagStaff = "STAFF"
  toText FlagSticky = "STICKY"
  toText FlagTerse = "TERSE"
  toText FlagTrace = "TRACE"
  toText FlagTransparent = "TRANSPARENT"
  toText FlagUnfindable = "UNFINDABLE"
  toText FlagUnicode = "UNICODE"
  toText FlagUninspected = "UNINSPECTED"
  toText FlagVacation = "VACATION"
  toText FlagVerbose = "VERBOSE"
  toText FlagVisual = "VISUAL"
  toText FlagWizard = "WIZARD"

data LockType = LockDefault | LockEnter | LockGetFrom | LockGive |
                LockLeave | LockLink | LockMail | LockOpen | LockPage |
                LockParent | LockReceive | LockSpeech | LockTelout |
                LockTport | LockUse | LockDrop | LockVisible
instance AsText LockType where
  toText LockDefault = ""
  toText LockEnter = "/enter"
  toText LockGetFrom = "/get"
  toText LockGive = "/give"
  toText LockLeave = "/leave"
  toText LockLink = "/link"
  toText LockMail = "/mail"
  toText LockOpen = "/open"
  toText LockPage = "/page"
  toText LockParent = "/parent"
  toText LockReceive = "/receive"
  toText LockSpeech = "/speech"
  toText LockTelout = "/telout"
  toText LockTport = "/tport"
  toText LockUse = "/use"
  toText LockDrop = "/drop"
  toText LockVisible = "/visible"

data LockAttrType = LockAttrAll | LockAttrInv | LockAttrPlayer
instance AsText LockAttrType where
  toText LockAttrAll = ""
  toText LockAttrInv = "+"
  toText LockAttrPlayer = "="

data LockExpr = LockENormal ObjName | LockEIs ObjName | LockECarry ObjName |
  LockEOwns ObjName | LockEIndirect ObjName |
  LockETheirAttribute LockAttrType Attr T.Text |
  LockEMyAttribute Attr T.Text |
  LockEAnd LockExpr LockExpr | LockEOr LockExpr LockExpr | LockENot LockExpr
instance AsText LockExpr where
  toText (LockENormal o) = toText o
  toText (LockEIs o) = "=" <> toText o
  toText (LockECarry o) = "+" <> toText o
  toText (LockEOwns o) = "$" <> toText o
  toText (LockEIndirect o) = "@" <> toText o
  toText (LockETheirAttribute t a p) = toText t <> toText a <> ":" <> p
  toText (LockEMyAttribute a p) = toText a <> "/" <> p
  toText (LockEAnd e1 e2) = "(" <> toText e1 <> ")&(" <> toText e2 <> ")"
  toText (LockEOr e1 e2) = "(" <> toText e1 <> ")|(" <> toText e2 <> ")"
  toText (LockENot e) = "!(" <> toText e <> ")"

data AttrValue = AttrValueText T.Text |
  AttrValueCommand T.Text [Softcode] |
  AttrValueListen T.Text [Softcode] |
  AttrValueProg [Softcode] |
  AttrValueExpr SoftExpr
instance AsText AttrValue where
  toText (AttrValueText t) = t
  toText (AttrValueCommand t p) = "$" <> t <> ":" <> escapeSoftExpr (T.intercalate ";" (map toText p))
  toText (AttrValueListen t p) = "^" <> t <> ":" <> escapeSoftExpr (T.intercalate ";" (map toText p))
  toText (AttrValueProg p) = T.intercalate ";" (map toText p)
  toText (AttrValueExpr e) = "[" <> toText e <> "]"

  toTextExtraBrace (AttrValueCommand t p) = "$" <> t <> ":" <> (T.intercalate ";" (map toTextExtraBrace p))
  toTextExtraBrace (AttrValueProg p) = T.intercalate ";" (map toTextExtraBrace p)
  toTextExtraBrace (AttrValueExpr e) = toTextExtraBrace e
  toTextExtraBrace v = toText v


data LocateFlag = LocateAll | LocateAbs | LocateExit | LocateHere | LocateInv | LocateMe |
                  LocateNeighbours | LocatePlayer
instance AsText LocateFlag where
  toText LocateAll = "*"
  toText LocateAbs = "a"
  toText LocateExit = "e"
  toText LocateHere = "h"
  toText LocateInv = "i"
  toText LocateMe = "m"
  toText LocateNeighbours = "n"
  toText LocatePlayer = "p"

data SoftExpr = SoftEString T.Text | SoftEObj ObjName | SoftEOwner SoftExpr |
  SoftEGet ObjName Attr | SoftEName SoftExpr | SoftENum SoftExpr |
  SoftELocate SoftExpr SoftExpr [LocateFlag] | SoftEVar T.Text |
  SoftERand SoftExpr | SoftELt SoftExpr SoftExpr | SoftEGt SoftExpr SoftExpr |
  SoftELte SoftExpr SoftExpr | SoftEGte SoftExpr SoftExpr |
  SoftEInt Int | SoftESwitch SoftExpr [(SoftExpr, SoftExpr)] SoftExpr
instance AsText SoftExpr where
  toText (SoftEString t) = t
  toText (SoftEObj o) = toText o
  toText (SoftEOwner e) = "owner(" <> toText e <> ")"
  toText (SoftEGet o a) = "get(" <> toText o <> "/" <> toText a <> ")"
  toText (SoftEName e) = "name(" <> toText e <> ")"
  toText (SoftENum e) = "num(" <> toText e <> ")"
  toText (SoftEVar n) = "%" <> n
  toText (SoftELocate looker expr flags) =
    "locate(" <> toText looker <> ","
    <> (toText expr) <> "," <>
    fold (map toText flags) <> ")"
  toText (SoftERand e) = "rand(" <> (toText e) <> ")"
  toText (SoftELt e1 e2) = "lt(" <> (toText e1) <> "," <> (toText e2) <> ")"
  toText (SoftEGt e1 e2) = "gt(" <> (toText e1) <> "," <> (toText e2) <> ")"
  toText (SoftELte e1 e2) = "lte(" <> (toText e1) <> "," <> (toText e2) <> ")"
  toText (SoftEGte e1 e2) = "gte(" <> (toText e1) <> "," <> (toText e2) <> ")"
  toText (SoftEInt n) = T.pack . show $ n
  toText (SoftESwitch e1 l def) = "switch(" <> toText e1 <> "," <> (T.intercalate "," $ map (\(vk,vv) -> toText vk <> "," <> toText vv) l) <> "," <> toText def <> ")"

                                                                   
data Softcode =
    SoftcodeCreate ObjName
  | SoftcodeSetDesc ObjName T.Text
  | SoftcodeLock ObjName LockType LockExpr
  | SoftcodeSetAttr Attr ObjName AttrValue
  | SoftcodeSwitch SoftExpr [(SoftExpr, [Softcode])] [Softcode]
  | SoftcodeSetName ObjName T.Text
  | SoftcodeSet ObjName [(Bool, FlagType)]
  | SoftcodePEmit ObjName T.Text
  | SoftcodeGet ObjName
  | SoftcodeChown ObjName ObjName
  | SoftcodeForwardlist ObjName [SoftExpr]
  | SoftcodeListen ObjName T.Text
  | SoftcodeWait Int [Softcode]
  | SoftcodeDestroyInstant ObjName
  | SoftcodeRaw T.Text

escapeSoftExpr :: T.Text -> T.Text
escapeSoftExpr = T.concatMap $
  \case
    '[' -> "\\["
    ']' -> "\\]"
    '\\' -> "\\\\"
    '%' -> "\\%"
    '\n' -> "%r"
    c -> T.singleton c
                                              
instance AsText Softcode where
  toText (SoftcodeCreate n) = "@create " <> (toText n)
  toText (SoftcodeSetDesc n d) = "@desc " <> (toText n) <> "=" <> escapeSoftExpr d
  toText (SoftcodeLock n t e) =
    "@lock" <> (toText t) <> " " <> (toText n) <> "=" <> (toText e)
  -- The longer eval set form parses reliably with commands etc... in general.
  toText (SoftcodeSetAttr a o v) = "@eval set(" <> (toText o) <> "," <> (toText a) <> ":{" <>
                                   (toText v) <> "})"
  toText (SoftcodeSwitch e cases elsecase) =
          "@switch [" <> (toText e) <> "]=" <>
             fold (map (\(mex, vex) -> "{" <> (toText mex) <> "},{" <>
                         (T.intercalate ";" $ map toTextExtraBrace vex) <> "},") cases)
             <> "{" <> (T.intercalate ";" $ map toTextExtraBrace elsecase) <> "}"
  toText (SoftcodeSetName o t) = "@name " <> (toText o) <> "=" <> t
  toText (SoftcodeSet o v) = "@set " <> (toText o) <> "=" <>
    (T.intercalate " " $ map (\(i, f) -> (if i then "" else "!") <> toText f) v)
  toText (SoftcodePEmit o t) = "@pemit " <> (toText o) <> "=" <> t
  toText (SoftcodeGet o) = "get " <> (toText o)
  toText (SoftcodeChown obj owner) = "@chown " <> (toText obj) <> "=" <> (toText owner)
  toText (SoftcodeForwardlist obj list) = "@forwardlist " <> (toText obj) <> "=" <>
                                          (T.intercalate " " $ map toText list)
  toText (SoftcodeRaw t) = t
  toText (SoftcodeListen o t) = "@listen " <> (toText o) <> "=" <> t
  toText (SoftcodeWait n p) = "@wait " <> (T.pack . show $ n) <> "={" <>
                              (T.intercalate ";" $ map toText p) <> "}"
  toText (SoftcodeDestroyInstant o) = "@destroy/instant " <> (toText o)

  toTextExtraBrace (SoftcodeSetAttr a o v) = "&" <> (toText a) <> " " <> (toText o) <> "={" <> (toText v) <> "}"
  toTextExtraBrace x = toText x
