{-# LANGUAGE OverloadedStrings #-}
module Pregnancy (pregnancyCode) where
import Softcode
import qualified Data.Text as T

oBlank = ObjName "Blank Pregnancy Risk"
me = ObjName "me"

successChance :: Int
successChance = 40

pregSpeed :: Int
pregSpeed = 60

descInitial :: T.Text
descInitial = "This object lets you roleplay a pregnancy risk scene.\n\nAs the potential father, type \"start pregnancy risk scene with <name>\". It will drop and become a Pregnancy Risk Setup. The recipient should read the description for next steps."

descMotherSetup :: T.Text
descMotherSetup = "This object lets you roleplay a pregnancy risk scene with [name(get(me/FATHER))].\n\nBy setting up this object, you will enable [name(get(me/FATHER))] to creampie you. 1 minute after being creampied, you will be able to do a pregnancy test. If you do get pregnant, over the next 9 minutes after taking the test, you will grow an embryo, then a fetus, then a baby. The baby will leave through the portal on birth. The pregnancy will be represented by an item in your inventory.\n\nTo consent and setup, type:\n@cpattr Pregnancy/SETUP-PREGNANCY=me\n@trigger me/SETUP-PREGNANCY\n\nBe aware that while pregnant or under pregnancy risk, the game takes a 10 credit + 1 quota deposit - but it is returned when the scenario ends."

descMotherReady :: T.Text
descMotherReady = "This object represents you being at risk of getting pregnant if you receive a creampie from [name(get(me/FATHER))].\n\nIf you want to withdraw consent for this, type:\n@destroy/instant [num(me)]\n\n[name(get(me/FATHER))] can type @pemit [num(me)]=creampie to cum in your pussy."

descCreampie :: T.Text
descCreampie = "A sticky creampie left in you by [name(get(me/FATHER))]. This might get you pregnant. To find out if you got pregnant, at least one minute after being creampied, you can type:\npregnancy test\nTo take a morning after pill and avoid getting pregnant, type:\n@destroy/instant [num(me)]"

descPregnant :: T.Text
descPregnant = "A growing pregnancy inside you, fathered by [name(get(me/FATHER))]. Gestation is fast here - so you will soon give birth. To get an abortion instead, type \"@destroy/instant [num(me)]\""

aNextCmd = Attr "NEXTCMD"
aFather = Attr "FATHER"
aMother = Attr "MOTHER"
aSetup = Attr "SETUP-PREGNANCY"
aForwardlist = Attr "FORWARDLIST"
aGender = Attr "GENDER"

motherSetupState :: [Softcode]
motherSetupState = [
  SoftcodeSetName me $ "Pregnancy Setup with [name(get(me/FATHER))]",
  SoftcodeLock me LockDefault (LockEOr (LockEIs . ObjExpr $ SoftEGet me aFather)
                                       (LockEIs . ObjExpr $ SoftEGet me aMother)), 
  SoftcodeLock me LockUse (LockEOr (LockEIs . ObjExpr $ SoftEGet me aFather)
                            (LockEIs . ObjExpr $ SoftEGet me aMother)),
  SoftcodeSet me [(True, FlagChownOk)],
  SoftcodeSetDesc me descMotherSetup,
  SoftcodeSetAttr aSetup me $ AttrValueProg motherSetup,
  SoftcodeSetAttr aNextCmd me $ AttrValueCommand "consent to pregnancy risk" $
    motherReadyState
  ]

motherReadyState :: [Softcode]
motherReadyState = [
    SoftcodeSetDesc me descMotherReady
  , SoftcodePEmit (ObjExpr $ SoftEGet me aFather) "Type @pemit [num(me)]=creampie to cum in [name(get(me/MOTHER))]"
  , SoftcodeSetAttr aNextCmd me $ AttrValueListen "creampie" $
      [SoftcodeSwitch (SoftEVar "#") [(SoftEGet me aFather, creampieSetup)] []]
                   ]

creampieSetup :: [Softcode]
creampieSetup = [
    SoftcodeSetName me "Creampie from [name(get(me/FATHER))]"
  , SoftcodeSetDesc me descCreampie
  , SoftcodeSetAttr aNextCmd me $ AttrValueCommand "pregnancy test" [
      SoftcodePEmit (ObjExpr $ SoftEGet me aMother) "It's too soon to take a pregnancy test"
      ]
  , SoftcodeSet me [(False, FlagMonitor), (True, FlagCommands)]
  , SoftcodePEmit (ObjExpr $ SoftEGet me aFather) "Your cum squirts deep inside [name(get(me/MOTHER))]"
  , SoftcodePEmit (ObjExpr $ SoftEGet me aMother) "[name(get(me/FATHER))]'s cum squirts from his unprotected member deep into your pussy"
  , SoftcodeSetAttr aGender me $ AttrValueExpr $ SoftERand (SoftEInt 2)
  , SoftcodeWait pregSpeed readyToTest
                ]

readyToTest :: [Softcode]
readyToTest = [
    SoftcodePEmit (ObjExpr $ SoftEGet me aMother) "You sense it is long enough after the creampie to type \"pregnancy test\" and get a result."
  , SoftcodeSetAttr aNextCmd me $ AttrValueCommand "pregnancy test" validTakeTest
              ]

validTakeTest :: [Softcode]
validTakeTest = [
  SoftcodeSwitch (SoftELte (SoftERand $ SoftEInt 100) (SoftEInt successChance))
    [(SoftEInt 1, gotPregnant)] notPregnant
                ]

notPregnant :: [Softcode]
notPregnant = [
    SoftcodePEmit (ObjExpr $ SoftEGet me aMother) "The test is negative. It looks like [name(get(me/FATHER))] didn't impregnate you this time - better luck next time!"
  , SoftcodeDestroyInstant me
              ]

gotPregnant :: [Softcode]
gotPregnant = [
    SoftcodePEmit (ObjExpr $ SoftEGet me aMother) "Congratulations, you are pregnant to [name(get(me/FATHER))]!"
  , SoftcodePEmit (ObjExpr $ SoftEGet me aFather) "You feel something in your gut that tells you that you got [name(get(me/MOTHER))] pregnant!"
  , SoftcodeSetName me "Growing embryo (1st trimester)"
  , SoftcodeSetDesc me descPregnant
  , SoftcodeWait (3 * pregSpeed) [
        SoftcodeSetName me "Growing fetus (2nd trimester)"
      , SoftcodePEmit (ObjExpr $ SoftEGet me aMother) "Your pregnancy entered the second trimester"
      ]
  , SoftcodeWait (6 * pregSpeed) [
        SoftcodeSetName me "Growing fetus (3rd trimester)"
      , SoftcodePEmit (ObjExpr $ SoftEGet me aMother) "Your pregnancy entered the third trimester"
      ]
  , SoftcodeWait (9 * pregSpeed) [
      SoftcodePEmit (ObjExpr $ SoftEGet me aMother) "You go into labour"
      ]
  , SoftcodeWait (9 * pregSpeed + 5) [
        SoftcodePEmit (ObjExpr $ SoftEGet me aMother) "Your child with [name(get(me/FATHER))] slides down the birth canal and out into the world as a healthy baby [switch(get(me/GENDER), 0, boy, girl)]. Since this is no place for children, a passing trader takes the child through the portal to a loving family, and [switch(get(me/GENDER), 0, he, she)] lives happily ever after."
      , SoftcodePEmit (ObjExpr $ SoftEGet me aFather) "Your child with [name(get(me/MOTHER))] slides down the birth canal and out into the world as a healthy baby [switch(get(me/GENDER), 0, boy, girl)]. Since this is no place for children, a passing trader takes the child through the portal to a loving family, and [switch(get(me/GENDER), 0, he, she)] lives happily ever after."
      , SoftcodeDestroyInstant me
      ]
  ]

initialState :: [Softcode]
initialState = [
      SoftcodeSetDesc oBlank descInitial
    , SoftcodeLock oBlank LockUse (LockEIs ObjMe)
    , SoftcodeSetAttr aNextCmd oBlank $ AttrValueCommand "setup pregnancy risk with *" [
          SoftcodeSetAttr aFather oBlank (AttrValueExpr . SoftEVar $ "#")
        , SoftcodeSetAttr aMother oBlank (AttrValueExpr $ SoftELocate (SoftEObj me) (SoftEVar "0") [LocateAll])
        , SoftcodeSwitch (SoftEGet me aMother) [(SoftEString "#-1",
                                                 [SoftcodePEmit (ObjExpr $ SoftEOwner (SoftEObj me)) "Couldn't find that user"])] motherSetupState
        ]
  ]

wiz :: ObjName
wiz = ObjRef 1

motherSetup :: [Softcode]
motherSetup =
  let
    it = ObjExpr . SoftENum . SoftEObj $ me
  in
    [
        SoftcodeGet it
      , SoftcodeChown it me
      , SoftcodeListen me "*"
      , SoftcodeSet it [(False, FlagHalted), (True, FlagMonitor)]
      , SoftcodeRaw "consent to pregnancy risk"
    ]

pregnancyCode :: [Softcode]
pregnancyCode = [
      SoftcodeCreate oBlank
    , SoftcodeSet oBlank [(True, FlagCommands), (True, FlagVisual)]
    , SoftcodeLock oBlank LockDefault (LockEIs (ObjExpr $ SoftEOwner (SoftEObj me)))
    , SoftcodeLock oBlank LockEnter (LockEIs wiz)
    , SoftcodeLock oBlank LockReceive (LockEIs wiz)
  ] ++ initialState
