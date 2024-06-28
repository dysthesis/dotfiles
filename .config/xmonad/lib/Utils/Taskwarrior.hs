module Utils.Taskwarrior where

import System.Process (callCommand, readProcess)
import XMonad (X)
import XMonad.Core (io)
import XMonad.Prompt (XPConfig)
import XMonad.Prompt.Input (inputPrompt, (?+))

addTaskAndNotify :: String -> X ()
addTaskAndNotify task = do
        let command = "task add " ++ task
        output <- io $ readProcess "sh" ["-c", command] ""
        io $ callCommand $ "notify-send 'Task Added' " ++ show output
taskPrompt :: XPConfig -> X ()
taskPrompt config = inputPrompt config "Task" ?+ addTaskAndNotify
