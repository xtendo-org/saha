module Saha (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import System.Console.CmdArgs.Explicit

import Saha.Compile
import Saha.Server.Run

data Command
  = CmdCompile
  | CmdRun
    OpenAt
    Bool -- debug mode?
    ByteString -- absolute host for redirecting
  | CmdHelp

arguments :: Mode Command
arguments = modes "saha" CmdHelp "Saha web serving tool"
  [compileCmd, runCmd]

compileCmd :: Mode Command
compileCmd = mode "compile" CmdCompile "apply templating and prepare serving"
  (flagArg (\ _ c -> Right c) "") []

runCmd :: Mode Command
runCmd = mode "server" (CmdRun (OpenAtPort 3000) False "")
  "launch HTTP server"
  (flagArg (\ _ c -> Right c) "")
  [ flagReq ["socket", "s"]
    socketUpd "PORT_OR_PATH" "Port number or Unix domain socket path"
  , flagReq ["host", "h"]
    hostUpd "HOST" "Absolute host address (for correct redirection)"
  , flagNone ["debug", "d"]
    debugUpd "Run the server in the debug mode or not"
  ]
 where
  socketUpd s (CmdRun _ d h) = Right $ CmdRun (parseOpenAt s) d h
  socketUpd _ x = Right x
  hostUpd arg (CmdRun s d _) = Right $ CmdRun s d host
   where
    host = if B.last b == '/' then B.init b else b
    b = B.pack arg
  hostUpd _ x = Right x
  debugUpd (CmdRun s _ h) = CmdRun s True h
  debugUpd x = x

main :: IO ()
main = do
  args <- processArgs arguments
  case args of
    CmdHelp -> print $ helpText [] HelpFormatDefault arguments
    CmdCompile -> compile
    CmdRun openAt debug host -> run openAt debug host
