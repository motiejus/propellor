module Utility.Process.Shim (module X, createProcess) where

import System.Process as X hiding (createProcess)
import Propellor.Message (createProcessConcurrent)
import System.IO

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) 
createProcess = createProcessConcurrent
