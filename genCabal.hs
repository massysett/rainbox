#!/usr/bin/env stack
-- stack --resolver lts-5.2 --install-ghc runghc --package cartel

-- Generates a Cabal file using the Cartel package.
--
-- Written for Cartel version 0.14.2.0

import Cartel

-- Package version
pkgVersion :: [Word]
pkgVersion = [0,18,0,6]

-- Dependencies

base :: Package
base = closedOpen "base" [4,8,0,0] [5]

bytestring :: Package
bytestring = package "bytestring" (gtEq [0,10])

containers :: Package
containers = package "containers" (gtEq [0,5,5])

rainbow :: Package
rainbow = package "rainbow" (gtEq [0,26])

lensSimple :: Package
lensSimple = package "lens-simple" (gtEq [0,1,0,8])

text :: Package
text = package "text" (gtEq [0,11,3,1])

tasty :: Package
tasty = package "tasty" (gtEq [0,10,1])

tasty_quickcheck :: Package
tasty_quickcheck = package "tasty-quickcheck" (gtEq [0,8,1])

quickcheck :: Package
quickcheck = package "QuickCheck" (gtEq [2,8,2])

properties :: Properties
properties = blank
  { name = "rainbox"
  , version = pkgVersion
  , cabalVersion = Just (1, 18)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "Copyright 2014-2016 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "http://www.github.com/massysett/rainbox"
  , bugReports = "http://www.github.com/massysett/rainbox/issues"
  , synopsis = "Two-dimensional box pretty printing, with colors"
  , testedWith = [(ghc, eq [7,10,2])]
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
  , lensSimple
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
  , otherExtensions ["TemplateHaskell"]
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
  , otherExtensions ["TemplateHaskell"]
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
