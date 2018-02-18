import Control.Concurrent (threadDelay)
import Test.QuickCheck.Text (handle, newTerminal, putTemp)
import System.IO (stdout)

main = do
  tm <- newTerminal (handle stdout) (handle stdout)
  putTemp tm "0.23"
  threadDelay (1000000)
  putTemp tm "0.25"
  threadDelay (1000000)
  putTemp tm "1.25"
  threadDelay (1000000)
  putTemp tm "4.25"
  threadDelay (1000000)
  putTemp tm "4.65"