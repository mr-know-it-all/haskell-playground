data MaybePerson a = JustPerson a | NoPerson

instance Functor MaybePerson where
  fmap f NoPerson = NoPerson
  fmap f (JustPerson s) = JustPerson (f s)

instance Applicative MaybePerson where
  pure = JustPerson
  NoPerson <*> _ = NoPerson
  (JustPerson f) <*> something = fmap f something

instance Monad MaybePerson where
  NoPerson >>= f = NoPerson
  (JustPerson x) >>= f = f x
  return = JustPerson

instance (Show a) => Show (MaybePerson a) where
  show (JustPerson a) = show a
  show NoPerson = show "Could not find person."

data Person = Person {name::String, mother::MaybePerson Person, father::MaybePerson Person}

instance Show Person where show s = show (name s)

maternalGrandfather :: Person -> MaybePerson Person
maternalGrandfather s = do
  m <- mother s
  father m

paternalGrandfather :: Person -> MaybePerson Person
paternalGrandfather s = do
  m <- father s
  father m

fathersMaternalGrandmother :: Person -> MaybePerson Person
fathersMaternalGrandmother s = do
  f <- father s
  gm <- mother f
  mother gm

mothersPaternalGrandfather :: Person -> MaybePerson Person
mothersPaternalGrandfather s = do
  m <- mother s
  gf <- father m
  father gf

createPerson :: Person
createPerson =
  let adam  = Person "Adam" NoPerson NoPerson
      eve  = Person "Eve" NoPerson NoPerson
      kronos = Person "Kronos" NoPerson NoPerson
      holly = Person "Holly" (JustPerson eve) (JustPerson adam)
      roger = Person "Roger" (JustPerson eve) (JustPerson kronos)
      molly = Person "Molly" (JustPerson holly) (JustPerson roger)
  in Person "Dolly" (JustPerson molly) NoPerson

chuckNorris :: Person
chuckNorris = Person "Chuck Norris" (JustPerson chuckNorris) (JustPerson chuckNorris)

main :: IO ()
main = let dolly = createPerson in do
  print "---"
  print dolly
  print (maternalGrandfather dolly)
  print (paternalGrandfather dolly)
  print (fathersMaternalGrandmother dolly)
  print (mothersPaternalGrandfather dolly)
  print "---"

  print "..."
  print chuckNorris
  print (maternalGrandfather chuckNorris)
  print (fathersMaternalGrandmother chuckNorris)
  print "..."
