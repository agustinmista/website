{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shelly

main :: IO ()
main = shelly $ verbosely $ do

  -- run "stack" ["exec", "site", "rebuild"]
  run "rsync" ["-r", "_site/.", "mista@remote11.chalmers.se:/chalmers/users/mista/www/www.cse.chalmers.se/"]
  -- run "rsync" ["-r", "_site/.", "ubuntu@mista.me:/var/www/html/mista.me/"]
  echo "Finished publishing to Chalmers server."
  exit 0
