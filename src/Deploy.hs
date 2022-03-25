{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shelly

main :: IO ()
main = shelly $ verbosely $ do
  -- run "rsync" ["-r", "_site/.", "mista@remote11.chalmers.se:/chalmers/users/mista/www/www.cse.chalmers.se/"]
  run "rsync" ["-Pavr", "-e", "ssh -i oci_rsa", "_site/.", "ubuntu@mista.me:/var/www/html/"]
  exit 0
