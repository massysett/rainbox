-- Generates a Cabal file using the Cartel package.
--
-- Written for Cartel version 0.10.0.2.

import qualified Cartel as A

-- Package version
version :: A.Version
version = A.Version [0,4,0,4]

-- Dependencies

base :: A.Package
base = A.closedOpen "base" [4,5,0,0] [4,8,0,0]

rainbow :: A.Package
rainbow = A.closedOpen "rainbow" [0,16,2,0] [0,17]

terminfo :: A.Package
terminfo = A.closedOpen "terminfo" [0,3,2] [0,5,0,0]

text :: A.Package
text = A.closedOpen "text" [0,11,3,1] [1,2,0,0]

transformers :: A.Package
transformers = A.closedOpen "transformers" [0,3,0,0] [0,5,0,0]

array :: A.Package
array = A.closedOpen "array" [0,4,0,0] [0,6,0,0]

tasty :: A.Package
tasty = A.closedOpen "tasty" [0,8] [0,9]

tasty_quickcheck :: A.Package
tasty_quickcheck = A.closedOpen "tasty-quickcheck" [0,8,1] [0,9]

quickcheck :: A.Package
quickcheck = A.closedOpen "QuickCheck" [2,7] [2,8]

random :: A.Package
random = A.closedOpen "random" [1,0,0,0] [1,1]

properties :: A.Properties
properties = A.empty
  { A.prName = "rainbox"
  , A.prVersion = version
  , A.prLicense = A.BSD3
  , A.prLicenseFile = "LICENSE"
  , A.prCopyright = "Copyright 2014 Omari Norman"
  , A.prAuthor = "Omari Norman"
  , A.prMaintainer = "omari@smileystation.com"
  , A.prStability = "Experimental"
  , A.prHomepage = "http://www.github.com/massysett/rainbox"
  , A.prBugReports = "http://www.github.com/massyett/rainbox/issues"
  , A.prSynopsis = "Two-dimensional box pretty printing, with colors"
  , A.prDescription =
    [ "Prints boxes in two dimensions, with colors.  Boxes are"
    , "automatically padded with necessary whitespace."
    , ""
    , "For more information, please see the Haddock documentation and"
    , ""
    , "<http://www.github.com/massysett/rainbox"
    ]
  , A.prCategory = "Text"
  , A.prTestedWith =
    map (\ls -> (A.GHC, A.eq ls))
    [ [7,4,1], [7,6,3], [7,8,2] ]

  , A.prExtraSourceFiles =
    [ "README.md"
    , "sunlight-test.hs"
    , "minimum-versions.txt"
    , "current-versions.txt"
    , "changelog"
    ]
  }

repo :: A.Repository
repo = A.empty
  { A.repoVcs = A.Git
  , A.repoKind = A.Head
  , A.repoLocation = "git://github.com/massysett/rainbox.git"
  , A.repoBranch = "master"
  }

ghcOptions :: A.Field a => a
ghcOptions = A.ghcOptions ["-Wall"]

libDeps :: A.Field a => a
libDeps = A.buildDepends
  [ base
  , rainbow
  , text
  , transformers
  , array
  ]

library
  :: [String]
  -- ^ Library modules
  -> A.Library
library ms = A.Library
  [ A.LibExposedModules ms
  , A.defaultLanguage A.Haskell2010
  , ghcOptions
  , A.hsSourceDirs ["lib"]
  , libDeps
  ]

visual
  :: [String]
  -- ^ Visual modules
  -> A.TestSuite
visual ms = A.TestSuite "rainbox-visual" $
  [ A.TestType A.ExitcodeStdio
  , ghcOptions
  , A.otherModules ms
  , A.TestMainIs "rainbox-visual.hs"
  , A.hsSourceDirs ["test", "lib"]
  , A.defaultLanguage A.Haskell2010
  , libDeps
  , A.buildDepends
    [ tasty
    , tasty_quickcheck
    , quickcheck
    , random
    ]
  ]

mosaic
  :: [String]
  -- ^ Mosaic modules
  -> A.Executable
mosaic ms = A.Executable "rainbox-mosaic"
  [ A.ExeMainIs "rainbox-mosaic.hs"
  , A.cif (A.flag "mosaic")
    [ ghcOptions
    , A.otherModules ms
    , A.hsSourceDirs ["test", "lib"]
    , A.defaultLanguage A.Haskell2010
    , libDeps
    , A.buildDepends
      [ tasty
      , tasty_quickcheck
      , quickcheck
      , random
      ]
    ]
    [ A.buildable False]
  ]

mainTest
  :: [String]
  -- ^ Test modules
  -> A.TestSuite
mainTest ms = A.TestSuite "rainbox-test"
  [ ghcOptions
  , A.TestType A.ExitcodeStdio
  , A.hsSourceDirs ["test", "lib"]
  , A.defaultLanguage A.Haskell2010
  , libDeps
  , A.TestMainIs "rainbox-test.hs"
  , A.buildDepends
    [ tasty
    , tasty_quickcheck
    , quickcheck
    ]
  ]

flagVisual :: A.Flag
flagVisual = A.empty
  { A.flName = "mosaic"
  , A.flDescription = "Build the rainbox-mosaic executable"
  , A.flDefault = False
  , A.flManual = True
  }

cabal
  :: [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> A.Cabal
cabal ms ts = A.empty
  { A.cProperties = properties
  , A.cRepositories = [repo]
  , A.cLibrary = Just $ library ms
  , A.cExecutables = [mosaic $ ms ++ ts]
  , A.cTestSuites = [ mainTest $ ms ++ ts, visual $ ms ++ ts ]
  , A.cFlags = [ flagVisual ]
  }

main :: IO ()
main = do
  ms <- A.modules "lib"
  ts <- A.modules "test"
  A.render "genCabal.hs" $ cabal ms ts
