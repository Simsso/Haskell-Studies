import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    mapM_ print args
