import Text.Printf
import Data.List
import Text.PrettyPrint.Boxes

class Material o where
    value :: o -> Double -- in isk per unit
    size :: o -> Double -- in m3 per unit

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

batch ore size minerals amounts = (sum [mineral (amount * efficiency ore * (1 - station_tax)) | (mineral, amount) <- zip minerals amounts]) / size

instance Material Ore where
    value Veldspar = batch Veldspar 333 [tritanium] [1000]
    value Scordite = batch Scordite 333 [tritanium, pyerite] [833, 416]
    value Pyroxeres = batch Pyroxeres 333 [tritanium, pyerite, mexallon, nocxium] [844, 59, 120, 11]
    value Plagioclase = batch Plagioclase 333 [tritanium, pyerite, mexallon] [256, 512, 256]
    value Omber = batch Omber 500 [tritanium, pyerite, isogen] [307, 123, 307]
    value Kernite = batch Kernite 400 [tritanium, mexallon, isogen] [386, 773, 386]
    value Jaspet = batch Jaspet 500 [tritanium, pyerite, mexallon, nocxium, zydrine] [259, 259, 518, 259, 8]
    value Hemorphite = batch Hemorphite 500 [tritanium, isogen, nocxium, zydrine] [212, 212, 424, 28]
    value Hedbergite = batch Hedbergite 500 [isogen, nocxium, zydrine] [708, 354, 32]
    value Gneiss = batch Gneiss 400 [tritanium, mexallon, isogen, zydrine] [171, 171, 343, 171]
    value Dark_Ochre = batch Dark_Ochre 400 [tritanium, nocxium, zydrine] [250, 500, 250]
    value Crokite = batch Crokite 250 [tritanium, nocxium, zydrine] [331, 331, 663]
    value Spodumain = batch Spodumain 250 [tritanium, pyerite, megacyte] [700, 140, 140]
    value Bistot = batch Bistot 200 [pyerite, zydrine, megacyte] [170, 341, 170]
    value Arkonor = batch Arkonor 200 [tritanium, zydrine, megacyte] [300, 166, 333]
    value Mercoxit = batch Mercoxit 250 [morphite] [530]
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

tritanium x = x * 5.8
pyerite x = x * 8
mexallon x = x * 49.99
nocxium x = x * 690
isogen x = x * 75.06
zydrine x = x * 999.99
megacyte x = x * 2400
morphite x = x * 6930

station_tax = 0.05
station_efficiency = 0.5
refining_skill = 5
refinery_efficiency_skill = 3
ore_refining_skill Pyroxeres = 3
ore_refining_skill Jaspet = 3
ore_refining_skill Hemorphite = 3
ore_refining_skill ore = 0

efficiency ore = if raw_efficiency > 1.0 then 1.0 else raw_efficiency
                 where raw_efficiency = station_efficiency + 0.375 * (1 + 0.02 * refining_skill) * (1 + 0.04 * refinery_efficiency_skill) * (1 + 0.05 * ore_refining_skill ore)

value_per_m3 a = value a * (1 / size a)
most_valuable a b = compare (value_per_m3 b) (value_per_m3 a)

prep_row a = [(show a), (printf "%.2f" $ value_per_m3 a)]

print_table :: [[String]] -> IO ()
print_table rows = printBox $ hsep 2 left (map (vcat left . map text) (transpose rows))

main = do
        print_table $ map prep_row $ sortBy most_valuable ([minBound .. maxBound] :: [Ore])
