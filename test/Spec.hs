

import            Test.Framework


import            Test.Payload as P

main :: IO ()
main = do
  defaultMain (P.tests)
