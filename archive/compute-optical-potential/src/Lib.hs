--{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
-- This whole thing might benefit from modifying one of those "read csv" 
-- tutorials.

module Lib
    ( computeOpticalPotential
    ) where

import System.IO    -- Used for hGetLine, hGetContents, etc.
import System.Environment (getArgs)

-- What does this program actually need?
-- For the whole simulation, it requires the wavelength, total thickness of the 
-- material, and its density.
-- For each element, it needs the atomic mass (in amus), number of atoms in the material, 
-- bXsecSc, bXsecAbs, xsecSctt, and xsecAbs
--
-- So, something like this for the layout of the input
-- # (neutron wavelength in angstrom) (material density in g/cm3) (material thickness in cm)
-- <material name>
-- wavelength density thickness
-- <element 1> mass num bCohReal bCohImag bIncReal bIncImag xsCohSctt xsIncSctt xsAbs
-- <element 2> mass num bCohReal bCohImag bIncReal bIncImag xsCohSctt xsIncSctt xsAbs
-- <element 3> mass num bCohReal bCohImag bIncReal bIncImag xsCohSctt xsIncSctt xsAbs
-- etc.
-- `num` is allowed to be a Double since some materials are mixtures, like air, 
-- and it makes more sense to allow percentages *there* than like writing something
-- else

-- Constants
avo     = 6.022e23                      -- Avogadro's number
u       = 1.66e-27                      -- Mass of single amu in kg
barn    = 1.0e-24                       -- 1 barn = 1e-24 cm^2
h       = 4.136e-15                     -- Planck's constant, eV*s
hbar    = h / (2.0 * pi)
hJs     = 6.626e-34                     -- Planck's constant, J*s
hbarJs  = hJs / (2.0 * pi)
cLight  = 2.99e8                        -- m/s
mNeut   = 939.57e6 / cLight^2           -- Mass of neutron in eV/c^2
prefac  = 2.0 * pi * hbar^2 / mNeut     -- Common prefactor for some calculations
vTherm  = 2200.0                        -- m/s; some things need to be in cm/s

-- Make a datatype to contain information about the material
data Material = Material 
    {
        matName     :: String,
        waveLngth   :: Double, 
        density     :: Double, 
        thickness   :: Double
    }
    deriving (Show, Eq)

printMaterial :: Material -> String
--printMaterial = (. matName) ++ "\t" ++ show (. waveLngth) ++ "\t" ++ show (. density) ++"\t" ++  show (. thickness)
printMaterial mtrl = (matName mtrl) ++ "\t" ++ show (waveLngth mtrl) ++ "\t" ++ show (density mtrl) ++"\t" ++  show (thickness mtrl)

readInMaterial :: String -> String -> Material
readInMaterial title props = Material title neutLambda density thickness
    where
        splitProps = fmap read (words props) :: [Double]
        neutLambda = splitProps !! 0
        density = splitProps !! 1
        thickness = splitProps !! 2
        
-- Declare a "class"/new data (type) for each element
-- bCoh and bInc should be allowed complex since the imaginary part just plays
-- into the absorption. Some other time.
data Element = Element
    {
        elemName    :: String,
        mass        :: Double,
        numAtoms    :: Double,
        bCohReal    :: Double,
        bCohImag    :: Double,
        bIncReal    :: Double,
        bIncImag    :: Double,
        xsCohSctt   :: Double,
        xsIncSctt   :: Double,
        xsTotSctt   :: Double,
        xsAbs       :: Double
    }
    deriving (Show, Eq)

readInElement :: String -> Element
readInElement s = Element name mss nAtms bCohR bCohI bIncR bIncI xsCoh xsInc xsTot xsAbs
    where
        props   = words s
        name    = props !! 0
        mss     = read (props !! 1) :: Double
        nAtms   = read (props !! 2) :: Double
        bCohR   = read (props !! 3) :: Double
        bCohI   = read (props !! 4) :: Double
        bIncR   = read (props !! 5) :: Double
        bIncI   = read (props !! 6) :: Double
        xsCoh   = read (props !! 7) :: Double
        xsInc   = read (props !! 8) :: Double
        xsTot   = xsCoh + xsInc
        xsAbs   = read (props !! 9) :: Double

-- Returns the g/cm^2 of an element
densityTimesThickness :: Material -> Double
densityTimesThickness m = (density m) * (thickness m)

-- Returns the amus/mol of a material
uPerMol :: Element -> Double
uPerMol e = mass e * numAtoms e

-- Computes the number of atoms of each element in the area (atoms/cm^2)
atomsPerArea :: Material -> Double -> Double
atomsPerArea mtrl molecAMU = avo * (densityTimesThickness mtrl) / molecAMU

-- Computes the average velocity of a neutron from the wavelength (in angstroms)
aveVel :: Double -> Double
aveVel lam = h / (lam * u)

-- Calculate the n*Sig for each element (J3:J5 in the spreadsheet)
nSigmai :: Double -> Double -> Element -> Double
nSigmai lam numPerArea e = 
    barn * numPerArea * (xsCohSctt e + (aveVel lam) * (xsAbs e))

printTuple :: Show a => (String, a) -> IO ()
printTuple tup = putStrLn $ (fst tup) ++ "\t" ++ (show $ snd tup)

-- Multiple input files should mean mapping over the files - do them all
computeOpticalPotential :: IO ()
computeOpticalPotential = do
    -- Get the file from the command line
    args <- getArgs

    -- Read the first line of the file and get its name
    inh <- openFile (head args) ReadMode
    title <- hGetLine inh
    props <- hGetLine inh
    restOfTheFile <- hGetContents inh
    let material = readInMaterial title props
        elements = [readInElement e | e <- lines restOfTheFile] -- Gather the elements
        uPerMols = [uPerMol e | e <- elements]                  -- List of masses (in amus) of the elements
        molarMass = sum uPerMols                                -- Molar mass of molecule
        matFactor= density material * avo / molarMass
        opticalV = prefac * matFactor * (sum $ [numAtoms e * bCohReal e | e <- elements])
        wFactors = 100.0 * 1e9 * barn * hbar / 2.0 * matFactor
        opticalWabs = wFactors * vTherm * (sum $ [numAtoms e * xsAbs e | e <- elements])
        opticalWsct = wFactors * (sum $ [numAtoms e * xsTotSctt e | e <- elements])
        opticalWtot = wFactors * (sum $ [numAtoms e * (xsTotSctt e + vTherm * xsAbs e) | e <- elements])
        phitsDensities = map (* (matFactor / 1e24)) [numAtoms e | e <- elements]
        phitsDensities' = [(elemName e, p)  | e <- elements | p <- phitsDensities]

    putStrLn $ title ++ " Molar Mass    = " ++ show molarMass ++ " u"
    putStrLn $ title ++ " V             = " ++ show opticalV ++ " neV"
    putStrLn $ title ++ " Wsc           = " ++ show opticalWsct ++ " neV"
    putStrLn $ title ++ " Wabs          = " ++ show opticalWabs ++ " neV"
    putStrLn $ title ++ " Wtot          = " ++ show opticalWtot ++ " neV"
    --putStrLn $ "\nmatFactor / 1e24 = " ++ show (matFactor / 1.0e24)
    putStrLn $ "\nPHITS densities for " ++ title ++ ":"
    mapM_ printTuple phitsDensities'
    --[(elemName e, p)  | e <- elements
    --                  | p <- phitsDensities]
