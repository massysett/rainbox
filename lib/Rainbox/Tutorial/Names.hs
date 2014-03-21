-- | Provides some data to use for the Rainbox tutorial.
--
-- Some data comes from www.fakenamegenerator.com, quite handy.
module Rainbox.Tutorial.Names where

data Record a = Record
  { firstName :: a
  , lastName :: a
  , address :: [a]
  , phone :: [(a, a)]
  -- ^ Each pair is (description, number)
  , email :: a
  , balance :: a
  } deriving Show

instance Functor Record where
  fmap f r = Record
    { firstName = f (firstName r)
    , lastName = f (lastName r)
    , address = map f . address $ r
    , phone = map (\(a1, a2) -> (f a1, f a2)) . phone $ r
    , email = f (email r)
    , balance = f (balance r)
    }

records :: [Record String]
records =
  [ Record
      { firstName = "Nell"
      , lastName = "Langston"
      , address = [ "Owings Mills, MD 21117" ]
      , phone = [ ]
      , email = "NellJLangston@dayrep.com"
      , balance = "0"
      }

  , Record
      { firstName = "Sharon"
      , lastName = "Sutton"
      , address = [ "37 Church Street", "Flushing, NY 11354" ]
      , phone = [("cell", "312-555-8100"), ("home", "720-555-9011")]
      , email = "SharonJSutton@teleworm.us"
      , balance = "1033.54"
      }

  , Record
      { firstName = "Barack"
      , lastName = "Obama"
      , address = [ "1600 Pennsylvania Ave NW", "Washington, DC" ]
      , phone = []
      , email = "president@whitehouse.gov"
      , balance = "23562.00"
      }

  , Record
      { firstName = "Bert and Ernie"
      , lastName = "Sesame"
      , address = [ "123 Sesame Street", "Lower Level",
                    "Sesame, WN V6B432" ]
      , phone = [("home", "+45-123-4567")]
      , email = "lower@rhyta.com"
      , balance = "100,451.05"
      }

  , Record
      { firstName = "Vip"
      , lastName = "Vipperman"
      , address = [ "10000 Smiley Lane", "Denver, CO 80266" ]
      , phone = [("home", "(303) 380-2000")]
      , email = "vip@rhyta.com"
      , balance = "903.80"
      }

  ]
