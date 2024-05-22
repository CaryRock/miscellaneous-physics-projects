import System.Environment (getArgs)

kb      = 1.381e-23         -- J/K
mn      = 1.674927485e-27   -- neutron mass, kg
hbar    = 1.055e-34         -- J*s
h       = 6.626e-34         -- J*s

--mwbmDist :: Double -> Int -> [Double]
--mwbmDist temp numVals = 

mwbmVelFromT :: Double -> Double
mwbmVelFromT temp = sqrt ((2.0 * kb * temp) / mn)

lamFromVel :: Double -> Double
lamFromVel vel = (1.0e10 * (h / mn) / vel)

showVelLam :: Double -> Double -> String
showVelLam vel lam = 
    "Assuming input as temp, the most probable velocity and its lambda are \n\t" 
    ++ (show vel) ++ "\t" ++ (show lam)

showVelTemp :: Double -> Double -> String
showVelTemp vel temp =
    "Assuming input as lambda, the most probable velocity and its temperature are \n\t" 
    ++ (show vel) ++ "\t" ++ (show temp)

showLamTemp :: Double -> Double -> String
showLamTemp lam temp =
    "Assuming input as most probable velocity, the lambda and its temperature are \n\t" 
    ++ (show lam) ++ "\t" ++ (show temp)

main = do
    -- Ideas: use something like argparse (but for Hasekll) that can take optional
    -- flags/switches to customize output. Default should be "take temperature, 
    -- spit out velocity and lambda"
    cmdTemp <- getArgs
    let temp    = read (cmdTemp !! 0) :: Double
        vel     = mwbmVelFromT temp
        lam     = lamFromVel vel
    putStrLn $ showVelLam vel lam
