import System.Environment
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List

main = do
    filename <- head <$> getArgs
    route <- readFile filename
    print $ verify route

data Region = A | C | D | E | F | G | H | K | L | N | P | R | T | W | X | Z deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Korok = Korok Region Int deriving (Eq, Ord)

allKoroks :: [Korok]
allKoroks = do
    region <- [A .. Z]
    number <- [1 .. numInRegion region]
    return $ Korok region number

instance Show Korok where
    show (Korok r i) = show r ++ if i >= 10 then show i else "0" ++ show i

readMaybeKorok :: String -> Maybe Korok
readMaybeKorok [] = Nothing
readMaybeKorok (c : num) = do
    r <- readMaybe [c]
    n <- readMaybe num
    return $ Korok r n

numInRegion :: Region -> Int
numInRegion r = case r of
    A -> 57
    C -> 89
    D -> 59
    E -> 45
    F -> 58
    G -> 36
    H -> 73
    K -> 35
    L -> 92
    N -> 66
    P -> 18
    R -> 80
    T -> 37
    W -> 68
    X -> 25
    Z -> 62

data Error = Missing Korok | Duplicate Korok Int | BadNum Korok
data Response = Bad [Error] | Good

instance Show Error where
    show (Missing k)     = "Missing Korok   : " ++ show k
    show (Duplicate k n) = "Duplicate Korok : " ++ show k ++ " (" ++ show n ++ " occurences)"
    show (BadNum k)      = "Badly numbered  : " ++ show k

instance Show Response where show = showResponse

showResponse :: Response -> String
showResponse (Bad errors) = intercalate "\n" $ map show errors
showResponse Good = "No problems!"

verify :: String -> Response
verify route = case errors of
    [] -> Good
    _  -> Bad errors
  where errors = badNums ++ duplicates ++ missings
        duplicates = map (\l@(x:_) -> Duplicate x (length l)) . filter (\l -> length l > 1) . group $ koroks
        badNums = map BadNum . filter (\(Korok r n) -> n > numInRegion r) $ koroks
        missings = map Missing $ allKoroks \\ koroks
        koroks = (sort . mapMaybe readMaybeKorok . words) route