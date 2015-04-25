-- Generates a Cabal file using the Cartel package.
--
-- Written for Cartel version 0.14.2.0

import Cartel

-- Package version
pkgVersion :: [Word]
pkgVersion = [0,14,0,0]

-- Dependencies

base :: Package
base = closedOpen "base" [4,5,0,0] [4,9,0,0]

bytestring :: Package
bytestring = closedOpen "bytestring" [0,10] [0,11]

containers :: Package
containers = closedOpen "containers" [0,5,5] [0,6]

rainbow :: Package
rainbow = nextBreaking "rainbow" [0,24]

text :: Package
text = closedOpen "text" [0,11,3,1] [1,3,0,0]

tasty :: Package
tasty = closedOpen "tasty" [0,10,1] [0,11]

tasty_quickcheck :: Package
tasty_quickcheck = closedOpen "tasty-quickcheck" [0,8,1] [0,9]

quickcheck :: Package
quickcheck = closedOpen "QuickCheck" [2,7] [2,9]

properties :: Properties
properties = blank
  { name = "rainbox"
  , version = pkgVersion
  , cabalVersion = Just (1, 18)
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

libPackages :: [Package]
libPackages =
  [ base
  , rainbow
  , bytestring
  , containers
  , text
  ]

libDeps :: HasBuildInfo a => a
libDeps = buildDepends libPackages

testDeps :: HasBuildInfo a => a
testDeps = buildDepends $ libPackages ++
  [ tasty
  , tasty_quickcheck
  , quickcheck
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

testSection
  :: [String]
  -- ^ Library modules
  -> [String]
  -- ^ Test modules
  -> String
  -- ^ Name of test
  -> Section
testSection ms ts nm = testSuite nm $
  [ mainIs (nm ++ ".hs")
  , testDeps
  , ghcOpts
  , haskell2010
  , hsSourceDirs ["lib", "test"]
  , otherModules (ms ++ ts)
  , exitcodeStdio
  ]

main :: IO ()
main = defaultMain $ do
  ms <- modules "lib"
  ts <- modules "test"
  return
    ( properties
    , library ms
    , [ githubHead "massysett" "rainbox"
      , testSection ms ts "rainbox-properties"
      , testSection ms ts "rainbox-visual"
      ]
    )
