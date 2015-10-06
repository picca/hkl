{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-
    Copyright  : Copyright (C) 2014-2015 Synchrotron Soleil
    License    : GPL3+

    Maintainer : picca@synchrotron-soleil.fr
    Stability  : Experimental
    Portability: GHC only?
-}

import Numeric.LinearAlgebra (Vector, Matrix,
                              vecdisp, disps,
                              dispf)

import Numeric.Units.Dimensional.Prelude (nano, meter, degree,
                                          (*~),
                                          (*~~), (/~~))

import Options.Applicative hiding ((<>))

import Hkl.Lattice
import Hkl.Diffractometer

dispv :: Vector Double -> IO ()
dispv = putStr . vecdisp (disps 2)

disp :: Matrix Double -> IO ()
disp = putStr . dispf 3

-- command parsing
data Command
    = Ca Double Double Double -- ca command

data Options
    = Options Command

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCa :: Parser Command
parseCa = Ca
    <$> argument auto (metavar "H")
    <*> argument auto (metavar "K")
    <*> argument auto (metavar "L")

parseCommand :: Parser Command
parseCommand = subparser $
    command "ca"   (parseCa `withInfo` "compute angles for the given hkl")

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

-- Actual program logic
run :: Options -> IO ()
run (Options cmd) =
    case cmd of
        Ca h k l-> do
           print (solution /~~ degree)
           dispv (computeHkl e4c solution lattice)
           disp path
               where
                 (sol, path) = computeAngles e4c angles lattice mode [h, k, l]
                 s = [30.0, 0.0, 0.0, 0.0, 10.0, 0.0]
                 d = [60.0]
                 angles = (s ++ d) *~~ degree
                 solution = fromMode mode sol angles
                 lattice = Cubic (1.54 *~ nano meter)
                 mode = ModeHklE4CConstantPhi

main :: IO ()
main = run =<< execParser
    (parseOptions `withInfo` "Interact with hkl API")
