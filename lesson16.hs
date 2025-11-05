type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName
          | TwoInitialsWithLast Char Char LastName

data Author = Author Name

data Artist = Person Name | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist


data Book = Book {
  bookAuthor :: Creator,
  isbn :: String,
  bookTitle :: String,
  bookYear :: Int,
  bookPrice :: Double
}

data Vinyl = Vinyl {
  vinylArtist :: Creator,
  vinylTitle :: String,
  vinylYear :: Int,
  vinylPrice :: Double
}

data CollectibleToy = CollectibleToy {
  name :: String,
  description::String,
  toyPrice::Double
}

data Pamphlet = Pamphlet {
  title:: String,
  pamphletDescription:: String,
  tell :: String}

data StoreItem = BookItem Book | VinylItem Vinyl | CollectibleToyItem CollectibleToy | PamphletItem Pamphlet

price::StoreItem->Double
price (BookItem book) = bookPrice book
price (VinylItem vinyl) = vinylPrice vinyl
price (CollectibleToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0


madeBy::StoreItem->String
madeBy (BookItem book) = show (bookAuthor book)
madeBy (VinylItem vinyl) = show (vinylArtist vinyl)
madeBy (CollectibleToyItem toy) = name toy

type R = Double
type H = Double
type W = Double

data Shape = CircleShape R | SquareShape H | RectangleShape W H deriving (Show)

shape::Shape->Double
shape (CircleShape r) = pi*r^2
shape (SquareShape h) = h^2
shape (RectangleShape w h) = w*h/2