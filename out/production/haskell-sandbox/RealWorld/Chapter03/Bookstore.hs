module RealWorld.Chapter03.Bookstore where

data BookInfo =
    Book Int
         String
         [String]
    deriving (Show)

data MagazineInfo =
    Magazine Int
             String
             [String]
    deriving (Show)

data BookReview =
    BookReview BookInfo
               CustomerID
               ReviewBody

type CustomerID = Int

type ReviewBody = String

type BookRecord = (BookInfo, BookReview)

type CardHolder = String

type CardNumber = String

type Address = [String]

data BillingInfo
    = CreditCard CardNumber
                 CardHolder
                 Address
    | CashOnDelivery
    | Invoice CustomerID
    deriving (Show)


bookID :: BookInfo -> Int
bookID (Book ident _ _) = ident

bookTitle :: BookInfo -> String
bookTitle (Book _ title _) = title

bookAuthors :: BookInfo -> [String]
bookAuthors (Book _ _ authors) = authors

-- Using record syntax to give field accessors without the above boilerplate.
data Customer = Customer
    { customerID :: CustomerID
    , customerName :: String
    , customerAddress :: Address
    } deriving (Show)
