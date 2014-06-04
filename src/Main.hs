import System.Environment
import Text.Regex (splitRegex, mkRegex)

import OddOneOut

perform extensions input = oddOneOut (lines input) (map (splitRegex $ mkRegex ",") extensions)

main = do
  input <- getContents
  args <- getArgs
  mapM_ putStrLn $ perform args input

