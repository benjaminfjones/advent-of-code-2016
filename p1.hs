inputs :: [String]
inputs = words "R1 L3 R5 R5 R5 L4 R5 R1 R2 L1 L1 R5 R1 L3 L5 L2 R4 L1 R4 R5 L3 R5 L1 R3 L5 R1 L2 R1 L5 L1 R1 R4 R1 L1 L3 R3 R5 L3 R4 L4 R5 L5 L1 L2 R4 R3 R3 L185 R3 R4 L5 L4 R48 R1 R2 L1 R1 L4 L4 R77 R5 L2 R192 R2 R5 L4 L5 L3 R2 L4 R1 L5 R5 R4 R1 R2 L3 R4 R4 L2 L4 L3 R5 R4 L2 L1 L3 R1 R5 R5 R2 L5 L2 L3 L4 R2 R1 L4 L1 R1 R5 R3 R3 R4 L1 L4 R1 L2 R3 L3 L2 L1 L2 L2 L1 L2 R3 R1 L4 R1 L1 L4 R1 L2 L5 R3 L5 L2 L2 L3 R1 L4 R1 R1 R2 L1 L4 L4 R2 R2 R2 R2 R5 R1 L1 L4 L5 R2 R4 L3 L5 R2 R3 L4 L1 R2 R3 R5 L2 L3 R3 R1 R3"

testInput1 = words "R1 L1"     -- dist 2
testInput2 = words "R1 L2 R1"  -- dist 4
testInput3 = words "R1 L2 L1"  -- dist 2

data Command = TLeft Int
             | TRight Int
             | Fwd Int
  deriving (Eq, Show)

parse :: String -> Command
parse ('R':rest) = TRight (read rest :: Int)
parse ('L':rest) = TLeft  (read rest :: Int)
parse s          = error ("Failed to parse: " ++ s)

expand :: Command -> [Command]
expand (TRight n) = TRight 1 : replicate (n-1) (Fwd 1)
expand (TLeft  n) = TLeft 1  : replicate (n-1) (Fwd 1)
expand c = [c]

type Dir = (Int, Int)
type Pos = (Int, Int)

origin :: Dir
origin = (0,0)

north, south, east, west :: Dir
north = (0,1)
south = (0,-1)
east = (1,0)
west = (-1,0)

rotL :: Dir -> Dir
rotL (x,y) = (-y, x)

rotR :: Dir -> Dir
rotR (x,y) = (y, -x)

adv :: Dir -> Int -> Pos -> Pos
adv (vx, vy) n (x,y) = (x+n*vx, y+n*vy)

initPos :: (Dir, Pos)
initPos = (north, origin)

action :: (Dir, Pos) -> Command -> (Dir, Pos)
action (d, p) (TLeft n)  = let d' = rotL d
                           in (d', adv d' n p)
action (d, p) (TRight n) = let d' = rotR d
                           in (d', adv d' n p)
action (d, p) (Fwd n)    = (d, adv d n p)

follow :: [Command] -> Pos
follow = snd . foldl action initPos

trail :: [Command] -> [Pos]
trail = map snd . scanl action initPos

repeatList :: [Pos] -> [(Pos, Bool)]
repeatList ps = [ (ps !! i, (ps !! i) `elem` (take (i-1) ps)) | i <- [1..(length ps - 1) ] ]

answer = fst $ filter snd $ repeatList (trail (concatMap (expand . parse) inputs))
