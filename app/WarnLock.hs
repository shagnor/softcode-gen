{-# LANGUAGE OverloadedStrings #-}
module WarnLock (warnlockCode) where
import Softcode
import qualified Data.Text as T
import Data.Maybe

{-
               Skill required: Intermediate
          Commitment required:          Low

We'd like to modernize the 'consent locks' that projects like Club
Inferno (@tel #1000) and others have on their doors so that they
provide a better experience. The modern analogy is the 'Press OK to
continue' experience of license agreements that typical software has.

Old design pattern:

 Locks are set to check for the existence of a visual attribute on the
enactor, such as INFERO, that is set with a '1'. This requires users to
copy/paste code and store VISUAL data on them that is visible to other
users.

 The @fail is set with a message with instructions to setup this
attribute.

New design pattern:

 The exit is locked against all players that haven't been shown the
business's content warning. When the player hits the lock and, they are
shown the message but automatically added to the project's list of
people that have been advised of the project's nature and contents.

 Players that have consented to the project's terms are stored on
database object that is separate from the code. This database is
cleaned regularly of nuked and recycled players. Any records older than
a year are also purged and the player is made to review the policy
message again.

Bonus objectives:

 Code the entire setup with functions and do not use @afail.

 Only store long-term data for players that continue into the project
within 15 minutes of seeing the message. Purge records of players that
do not continue after seeing this message.

 Adapt the door lock functionality to work against teleportation in a
simple fashion, but do not use a room parent.
-}

oLockParent = ObjName "Warn Lock Parent"

aAttempt = Attr "ATTEMPT"
aShards = Attr "SHARDS"
aCleanup = Attr "CLEANUP"
aLastCleanup = Attr "LAST_CLEANUP"

rShard = SoftReg "0"

shardCount :: Int
shardCount = 255

cleanupFrequencySecs :: Int
cleanupFrequencySecs = 3600 * 24

sender :: ObjName
sender = ObjExpr (SoftEVar "#")


shardAttr :: SoftExpr -> Attr
shardAttr forShard = DynamicAttr "SHARD_" forShard ""

senderShard :: Attr
senderShard = shardAttr (SoftEVar "#")

-- A note on record packing...
-- Records look like this PLAYERDBNUM#PLAYERCREATED#STATECODE#

cleanupCheck :: SoftExpr
cleanupCheck = SoftEIf (SoftELt (SoftEAdd [SoftEGet me aLastCleanup, SoftEInt cleanupFrequencySecs]) SoftESecs) cleanupExpr Nothing

cleanupExpr :: SoftExpr
cleanupExpr = SoftEAnd [SoftESetAttrEval me aLastCleanup SoftESecs,
                        
                        SoftEList (SoftEGet me aShards) Nothing $
                         SoftEAnd [
                           SoftESetReg rShard SoftEItem,
                           SoftEList (SoftEGet me (shardAttr (SoftEGetReg rShard))) Nothing $
                             SoftEString "TO DO"
                                  ]
                       ]

attemptExpr :: SoftExpr
attemptExpr = SoftEAnd [SoftEPemit [sender] $ SoftEString "Consent needed"]

warnlockCode :: [Softcode]
warnlockCode = [
  SoftcodeCreate oLockParent,
  SoftcodeSetDesc oLockParent
    "Objects inheriting from this act as databases of players who have consented to access\n\
    \a room. Create a new object, put it in a safe place, and set the parent to this object.\n\
    \\n\
    \Set &CONSENT-MESSAGE yourobj=Some text users will see when trying to access warning about the room.\n\
    \\n\
    \Next, set the exit locks to @lock exit==[u(yourobj/ATTEMPT)]\n\
    \Players will fail the lock the first time, but the second time will\n\
    \be allowed to enter if they try again within 15 minutes.",
  SoftcodeSetAttr aAttempt oLockParent $ AttrValueFun attemptExpr,
  SoftcodeLock oLockParent LockDefault (LockEMyAttribute aAttempt "1")
  ]
