
import qualified Data.Map as M

type PrimaryId = Maybe String
type AlternateId = Maybe String

data Prog = Prog ProgType PrimaryId AlternateId
    deriving (Eq, Ord, Show)

data ProgType = Brand | Series | Episode
    deriving (Eq, Ord, Show)

data Database = Database {
    list  :: [Prog],
    graph :: M.Map Prog Prog
}

search :: Database -> Prog -> [Prog]
search db p =
    filter (match p) (list db)
    where
        match (Prog t1 _ _) (Prog t2 _ _) | t1 /= t2 = False

        match (Prog _ Nothing Nothing) _ = False
        match _ (Prog _ Nothing Nothing) = False

        match (Prog _ Nothing _) (Prog _ _ Nothing) = False
        match (Prog _ _ Nothing) (Prog _ Nothing _) = False

        match (Prog _ (Just p1) _) (Prog _ (Just p2) _) | p1 /= p2 = False
        match (Prog _ _ (Just a1)) (Prog _ _ (Just a2)) | a1 /= a2 = False

        match _ _ = True

getHierarchy :: Database -> Prog -> [Prog]
getHierarchy db p =
    let l = M.lookup p (graph db)
    in case l of
        Nothing -> [p]
        Just (parent) -> p : (getHierarchy db parent)

-- sample data and call

b1 = Prog Brand (Just "bbc1") Nothing
s1 = Prog Series (Just "hignfy") Nothing
e1 = Prog Episode (Just "hignfy1") (Just "hignfy1a")

db :: Database
db = Database [b1, s1, e1] 
    (M.fromList [ 
        (e1, s1),
        (s1, b1)
    ])

main = do
    print $ search db (Prog Episode (Just "hignfy1") Nothing) >>= getHierarchy db
    print $ search db (Prog Episode Nothing (Just "hignfy1a")) >>= getHierarchy db
    print $ search db (Prog Series (Just "hignfy") (Just "new ID")) >>= getHierarchy db
