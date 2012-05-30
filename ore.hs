import Text.Printf
import Data.List

class Material o where
    value :: o -> Double -- in isk per unit
    value_per_m3 :: o -> Double -- in isk per m3
    size :: o -> Double -- in m3 per unit
    print_out :: o -> IO () -- prints out name and value per m3

data Ore =
      Veldspar
    | Scordite
    | Pyroxeres
    | Plagioclase
    | Omber
    | Kernite
    | Jaspet
    | Hemorphite
    | Hedbergite
    | Gneiss
    | Dark_Ochre
    | Crokite
    | Spodumain
    | Bistot
    | Arkonor
    | Mercoxit
    deriving (Show, Enum, Bounded)

instance Material Ore where
    value Veldspar = batch 333 $ tritanium 1000
    value Scordite = batch 333 $ tritanium 833 + pyerite 416
    value Pyroxeres = batch 333 $ tritanium 844 + pyerite 59 + mexallon 120 + nocxium 11
    value Plagioclase = batch 333 $ tritanium 256 + pyerite 512 + mexallon 256
    value Omber = batch 500 $ tritanium 307 + pyerite 123 + isogen 307
    value Kernite = batch 400 $ tritanium 386 + mexallon 773 + isogen 386
    value Jaspet = batch 500 $ tritanium 259 + pyerite 259 + mexallon 518 + nocxium 259 + zydrine 8
    value Hemorphite = batch 500 $ tritanium 212 + isogen 212 + nocxium 424 + zydrine 28
    value Hedbergite = batch 500 $ isogen 708 + nocxium 354 + zydrine 32
    value Gneiss = batch 400 $ tritanium 171 + mexallon 171 + isogen 343 + zydrine 171
    value Dark_Ochre = batch 400 $ tritanium 250 + nocxium 500 + zydrine 250
    value Crokite = batch 250 $ tritanium 331 + nocxium 331 + zydrine 663
    value Spodumain = batch 250 $ tritanium 700 + pyerite 140 + megacyte 140
    value Bistot = batch 200 $ pyerite 170 + zydrine 341 + megacyte 170
    value Arkonor = batch 200 $ tritanium 300 + zydrine 166 + megacyte 333
    value Mercoxit = batch 250 $ morphite 530
    size Veldspar = 0.1
    size Scordite = 0.15
    size Pyroxeres = 0.3
    size Plagioclase = 0.35
    size Omber = 0.6
    size Kernite = 1.2
    size Jaspet = 2
    size Hemorphite = 3
    size Hedbergite = 3
    size Gneiss = 5
    size Dark_Ochre = 8
    size Crokite = 16
    size Spodumain = 16
    size Bistot = 16
    size Arkonor = 16
    size Mercoxit = 40
    value_per_m3 a = value a * (1 / size a)
    print_out a = printf "%s %.2f\n" (show a) (value_per_m3 a)

tritanium x = x * 5.8
pyerite x = x * 7.50
mexallon x = x * 48
nocxium x = x * 673
isogen x = x * 78.05
zydrine x = x * 1000
megacyte x = x * 2108.24
morphite x = x * 7199.99

batch size value = value / size

most_valuable a b = compare (value_per_m3 b) (value_per_m3 a)

main = do
        mapM_ print_out $ sortBy most_valuable ([minBound .. maxBound] :: [Ore])
