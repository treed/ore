class Material o where
    name :: o -> String -- Just for convenience sake
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

instance Material Ore where
    name Veldspar = "Veldspar"
    name Scordite = "Scordite"
    name Pyroxeres = "Pyroxeres"
    name Plagioclase = "Plagioclase"
    name Omber = "Omber"
    name Kernite = "Kernite"
    name Jaspet = "Jaspet"
    name Hemorphite = "Hemorphite"
    name Hedbergite = "Hedbergite"
    name Gneiss = "Gneiss"
    name Dark_Ochre = "Dark Ochre"
    name Crokite = "Crokite"
    name Spodumain = "Spodumain"
    name Bistot = "Bistot"
    name Arkonor = "Arkonor"
    name Mercoxit = "Mercoxit"
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
    value Spodumain = batch 250 $ tritanium 700 + pyerite 140 + megacite 140
    value Bistot = batch 200 $ pyerite 170 + zydrine 341 + megacite 170
    value Arkonor = batch 200 $ tritanium 300 + zydrine 166 + megacite 333
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
    print_out a = putStrLn $ name a ++ " " ++ (show $ value_per_m3 a)

tritanium x = x * 6.04
pyerite x = x * 8.08
mexallon x = x * 52.50
nocxium x = x * 700
isogen x = x * 80.20
zydrine x = x * 900
megacite x = x * 2500
morphite x = x * 6815

batch size value = value / size

main = do
        print_out Scordite
        print_out Pyroxeres
        print_out Plagioclase
        print_out Omber
        print_out Kernite
        print_out Jaspet
        print_out Hemorphite
        print_out Hedbergite
        print_out Gneiss
        print_out Dark_Ochre
        print_out Crokite
        print_out Spodumain
        print_out Bistot
        print_out Arkonor
        print_out Mercoxit
