import System.Process
import System.Exit
import Distribution.Simple

main = defaultMainWithHooks $ simpleUserHooks { runTests = runTestSuite }

runTestSuite _ _ _ _ =
 runCommand "runhaskell Test.hs" >>= waitForProcess >>= exitWith

