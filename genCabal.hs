-- Generates a Cabal file using the Cartel package.
--
-- Written for Cartel version 0.14.2.0

import Cartel

-- Package version
pkgVersion :: [Word]
pkgVersion = [0,10,0,0]

-- Dependencies

base :: Package
base = closedOpen "base" [4,5,0,0] [4,8,0,0]

rainbow :: Package
rainbow = nextBreaking "rainbow" [0,20]

terminfo :: Package
terminfo = closedOpen "terminfo" [0,3,2] [0,5,0,0]

text :: Package
text = closedOpen "text" [0,11,3,1] [1,3,0,0]

transformers :: Package
transformers = closedOpen "transformers" [0,3,0,0] [0,5,0,0]

array :: Package
array = closedOpen "array" [0,4,0,0] [0,6,0,0]

tasty :: Package
tasty = closedOpen "tasty" [0,10,1] [0,11]

tasty_quickcheck :: Package
tasty_quickcheck = closedOpen "tasty-quickcheck" [0,8,1] [0,9]

quickcheck :: Package
quickcheck = closedOpen "QuickCheck" [2,7,5] [2,8]

random :: Package
random = closedOpen "random" [1,0,0,0] [1,2]

properties :: Properties
properties = blank
  { name = "rainbox"
  , version = pkgVersion
  , cabalVersion = Just (1, 14)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "Copyright 2014-2015 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "http://www.github.com/massysett/rainbox"
  , bugReports = "http://www.github.com/massyett/rainbox/issues"
  , synopsis = "Two-dimensional box pretty printing, with colors"
  , description =
    [ "Prints boxes in two dimensions, with colors.  Boxes are"
    , "automatically padded with necessary whitespace."
    , ""
    , "For more information, please see the Haddock documentation and"
    , ""
    , "<http://www.github.com/massysett/rainbox>"
    ]
  , category = "Text"
  , extraSourceFiles =
    [ "README.md"
    , "changelog"
    ]
  }

ghcOpts :: HasBuildInfo a => a
ghcOpts = ghcOptions ["-Wall"]

libDeps :: HasBuildInfo a => a
libDeps = buildDepends
  [ base
  , rainbow
  , text
  , transformers
  , array
  ]

library
  :: [String]
  -- ^ Library modules
  -> [LibraryField]
library ms =
  [ exposedModules ms
  , haskell2010
  , ghcOpts
  , hsSourceDirs ["lib"]
  , libDeps
  ]

visual
  :: [String]
  -- ^ Visual modules
  -> Section
visual ms = testSuite "rainbox-visual" $
  [ exitcodeStdio
  , ghcOpts
  , otherModules ms
  , mainIs "rainbox-visual.hs"
  , hsSourceDirs ["test", "lib"]
  , haskell2010
  , libDeps
  , buildDepends
    [ tasty
    , tasty_quickcheck
    , quickcheck
    , random
    ]
  ]

mosaic
  :: FlagName
  -- ^ mosaic flag
  -> [String]
  -- ^ Mosaic modules
  -> Section
mosaic mosaic ms = executable "rainbox-mosaic"
  [ mainIs "rainbox-mosaic.hs"
  , condBlock (flag mosaic)
    ( ghcOpts
    , [ otherModules ms
      , hsSourceDirs ["test", "lib"]
      , haskell2010
      , libDeps
      , buildDepends
        [ tasty
        , tasty_quickcheck
        , quickcheck
        , random
        ]
      ]
    )
    [ buildable False]
  ]

mainTest
  :: [String]
  -- ^ Test modules
  -> Section
mainTest ms = testSuite "rainbox-test"
  [ ghcOpts
  , exitcodeStdio
  , hsSourceDirs ["test", "lib"]
  , haskell2010
  , libDeps
  , mainIs "rainbox-test.hs"
  , buildDepends
    [ tasty
    , tasty_quickcheck
    , quickcheck
    ]
  ]

main :: IO ()
main = defaultMain $ do
  ms <- modules "lib"
  ts <- modules "test"
  fl <- makeFlag "mosaic" $ FlagOpts
    { flagDescription = "Build the rainbox-mosaic executable"
    , flagDefault = False
    , flagManual = True
    }
  return
    ( properties
    , library ms
    , [ visual (ms ++ ts)
      , mosaic fl (ms ++ ts)
      , mainTest (ms ++ ts)
      , githubHead "massysett" "rainbox"
      ]
    )
