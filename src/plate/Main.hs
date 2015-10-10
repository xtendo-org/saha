{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.CmdArgs.Explicit

import Types
import Run

data Command
    = CmdRun OpenAt Bool
    | CmdHelp

arguments :: Mode Command
arguments = mode "run" (CmdRun (OpenAtPort 3000) False) "launch HTTP server"
    (flagArg (\ _ c -> Right c) "")
    [ flagReq ["socket", "s"]
        socketUpd "PORT_OR_PATH" "Port number or Unix domain socket path"
    , flagNone ["debug", "d"]
        debugUpd "Run the server in the debug mode or not"
    , flagHelpSimple (\ _ -> CmdHelp)
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
        CmdRun openAt debug -> run openAt debug
