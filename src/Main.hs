{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.CmdArgs.Explicit

import Compile
import Server.Run

data Command
    = CmdCompile
    | CmdRun OpenAt Bool
    | CmdHelp

arguments :: Mode Command
arguments = modes "saha" CmdHelp "Saha web serving tool"
    [compileCmd, runCmd]

compileCmd :: Mode Command
compileCmd = mode "compile" CmdCompile "apply templating and prepare serving"
    (flagArg (\ _ c -> Right c) "") []

runCmd :: Mode Command
runCmd = mode "server" (CmdRun (OpenAtPort 3000) False) "launch HTTP server"
    (flagArg (\ _ c -> Right c) "")
    [ flagReq ["socket", "s"]
        socketUpd "PORT_OR_PATH" "Port number or Unix domain socket path"
    , flagNone ["debug", "d"]
        debugUpd "Run the server in the debug mode or not"
    ]
  where
    socketUpd s (CmdRun _ d) = Right $ CmdRun (parseOpenAt s) d
    socketUpd _ x = Right x
    debugUpd (CmdRun s _) = CmdRun s True
    debugUpd x = x

main :: IO ()
main = do
    args <- processArgs arguments
    case args of
        CmdHelp -> print $ helpText [] HelpFormatDefault arguments
        CmdCompile -> compile
        CmdRun openAt debug -> run openAt debug
