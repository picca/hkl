{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}

module Hkl.C
       ( compute
       , geometryDetectorRotationGet
       , solve
       , solveTraj
       , solveTrajPipe
       ) where

import Prelude hiding (min, max)

import Control.Monad (void, forever)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.Trans.State.Strict
import Numeric.LinearAlgebra
import Foreign ( ForeignPtr
               , FunPtr
               , Ptr
               , nullPtr
               , newForeignPtr
               , withForeignPtr
               , peekArray
               , withArray)
import Foreign.C (CInt(..), CDouble(..), CSize(..), CString,
                 peekCString, withCString)
import Foreign.Storable
import Hkl.Types
import Hkl.Detector
import Numeric.Units.Dimensional.Prelude ( meter, degree, radian, nano
                                         , (*~), (/~))
import Pipes (Pipe, await, lift, yield)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV

#include "hkl.h"

-- private types

data HklDetector
data HklEngine
data HklEngineList
data HklFactory
data HklGeometry
data HklGeometryList
data HklGeometryListItem
data HklLattice
data HklMatrix
data HklQuaternion
data HklSample

-- helpers

peekDArrayString :: Ptr () -> IO [CString]
peekDArrayString p = do
  n <- (#{peek darray_string, size} p) :: IO CSize
  items <- #{peek darray_string ,item} p :: IO (Ptr CString)
  peekArray (fromEnum n) items

darrayStringLen :: Ptr () -> IO (CSize)
darrayStringLen p = do
    n <- (#{peek darray_string, size} p) :: IO CSize
    return n

-- Engine

solve' :: Ptr HklEngine -> CSize -> Engine -> IO (ForeignPtr HklGeometryList)
solve' engine n (Engine _ ps _) = do
  let positions = [v | (Parameter _ v _) <- ps]
  withArray positions $ \values ->
      c_hkl_engine_pseudo_axis_values_set engine values n unit nullPtr
      >>= newForeignPtr c_hkl_geometry_list_free

solve :: Geometry -> Detector a -> Sample -> Engine -> IO [Geometry]
solve g@(Geometry f _ _ _) d s e@(Engine name _ _) = do
  withSample s $ \sample ->
      withDetector d $ \detector ->
          withGeometry g $ \geometry ->
              withEngineList f $ \engines ->
                  withCString name $ \cname -> do
                  c_hkl_engine_list_init engines geometry detector sample
                  engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
                  n <- c_hkl_engine_pseudo_axis_names_get engine >>= darrayStringLen
                  solve' engine n e >>= peekHklGeometryList

getSolution0 :: ForeignPtr HklGeometryList -> IO Geometry
getSolution0 gl = withForeignPtr gl $ \solutions ->
                  c_hkl_geometry_list_items_first_get solutions
                  >>= c_hkl_geometry_list_item_geometry_get
                  >>= peekGeometry

engineName :: Engine -> String
engineName (Engine name _ _) = name

solveTraj :: Geometry -> Detector a -> Sample -> [Engine] -> IO [Geometry]
solveTraj g@(Geometry f _ _ _) d s es = do
  let name = engineName (head es)
  withSample s $ \sample ->
      withDetector d $ \detector ->
          withGeometry g $ \geometry ->
              withEngineList f $ \engines ->
                withCString name $ \cname -> do
                  c_hkl_engine_list_init engines geometry detector sample
                  engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
                  n <- c_hkl_engine_pseudo_axis_names_get engine >>= darrayStringLen
                  mapM (\e -> solve' engine n e >>= getSolution0) es

-- Pipe

data Diffractometer = Diffractometer { difEngineList :: (ForeignPtr HklEngineList)
                                     , difGeometry :: (ForeignPtr HklGeometry)
                                     , difDetector :: (ForeignPtr HklDetector)
                                     , difSample :: (ForeignPtr HklSample)
                                     }
                    deriving (Show)

withDiffractometer :: Diffractometer -> (Ptr HklEngineList -> IO b) -> IO b
withDiffractometer d fun = do
  let f_engines = difEngineList d
  withForeignPtr f_engines fun

newDiffractometer :: Geometry -> Detector a -> Sample -> IO Diffractometer
newDiffractometer g@(Geometry f _ _ _) d s = do
  f_engines <- newEngineList f
  f_geometry <- newGeometry g
  f_detector <- newDetector d
  f_sample <- newSample s
  withForeignPtr f_sample $ \sample ->
    withForeignPtr f_detector $ \detector ->
    withForeignPtr f_geometry $ \geometry ->
    withForeignPtr f_engines $ \engines -> do
      c_hkl_engine_list_init engines geometry detector sample
      return $ Diffractometer { difEngineList = f_engines
                              , difGeometry = f_geometry
                              , difDetector = f_detector
                              , difSample = f_sample
                              }

solveTrajPipe :: Geometry -> Detector a -> Sample -> Pipe Engine Geometry IO ()
solveTrajPipe g d s = do
  dif <- lift $ newDiffractometer g d s
  solveTrajPipe' dif

solveTrajPipe' :: Diffractometer -> Pipe Engine Geometry IO ()
solveTrajPipe' dif = flip evalStateT dif $ forever $ do
    -- Inside here we are using `StateT Diffractometer (Pipe Engine Geometry IO ()) r`
    e <- lift await
    dif_ <- get
    let name = engineName e
    solutions <- lift . lift $ withDiffractometer dif_ $ \engines ->
     withCString name $ \cname -> do
       engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
       n <- c_hkl_engine_pseudo_axis_names_get engine >>= darrayStringLen
       solve' engine n e >>= getSolution0
    put dif_
    lift $ yield solutions

foreign import ccall unsafe "hkl.h hkl_engine_list_engine_get_by_name"
  c_hkl_engine_list_engine_get_by_name :: Ptr HklEngineList --engine list
                                       -> CString -- engine name
                                       -> Ptr () --  gerror need to deal about this
                                       -> IO (Ptr HklEngine) -- the returned HklEngine

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_values_set"
  c_hkl_engine_pseudo_axis_values_set :: Ptr HklEngine
                                      -> Ptr Double --values
                                      -> CSize -- n_values
                                      -> CInt -- unit_type
                                      -> Ptr () --  GError **error
                                      -> IO (Ptr HklGeometryList)

foreign import ccall unsafe "hkl.h &hkl_geometry_list_free"
  c_hkl_geometry_list_free :: FunPtr (Ptr HklGeometryList -> IO ())

-- Factory

newFactory :: Factory -> IO (Ptr HklFactory)
newFactory f = withCString (show f) $ \cname -> c_hkl_factory_get_by_name cname nullPtr

foreign import ccall unsafe "hkl.h hkl_factory_get_by_name"
  c_hkl_factory_get_by_name :: CString  -- ^ name
                            -> Ptr () -- ^ GError (null for now)
                            -> IO (Ptr HklFactory)
-- Geometry

peekGeometry :: Ptr HklGeometry -> IO (Geometry)
peekGeometry gp = do
  f_name <- c_hkl_geometry_name_get gp >>= peekCString
  let factory = factoryFromString f_name
  (CDouble w) <- c_hkl_geometry_wavelength_get gp unit
  darray <- c_hkl_geometry_axis_names_get gp
  n <- darrayStringLen darray
  v <- MV.new (fromEnum n)
  MV.unsafeWith v $ \values ->
      c_hkl_geometry_axis_values_get gp values n unit
  vs <- V.freeze v

  axis_names <- peekDArrayString darray
  ps <- mapM (getAxis gp) axis_names
  return $ Geometry factory (Source (w *~ nano meter)) vs (Just ps)
      where
        getAxis :: Ptr HklGeometry -> CString -> IO Parameter
        getAxis _g n = c_hkl_geometry_axis_get _g n nullPtr >>= peek

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_get"
  c_hkl_geometry_wavelength_get :: Ptr HklGeometry -- geometry
                                -> CInt -- unit
                                -> IO CDouble -- wavelength


foreign import ccall unsafe "hkl.h hkl_geometry_axis_values_get"
  c_hkl_geometry_axis_values_get :: Ptr HklGeometry -- geometry
                                 -> Ptr Double -- axis values
                                 -> CSize -- size of axis values
                                 -> CInt -- unit
                                 -> IO () -- IO CInt but for now do not deal with the errors

foreign import ccall unsafe "hkl.h hkl_geometry_axis_names_get"
  c_hkl_geometry_axis_names_get :: Ptr HklGeometry -- goemetry
                                -> IO (Ptr ()) -- darray_string

foreign import ccall unsafe "hkl.h hkl_geometry_axis_get"
  c_hkl_geometry_axis_get :: Ptr HklGeometry -- geometry
                          -> CString -- axis name
                          -> Ptr () -- gerror
                          -> IO (Ptr Parameter) -- parameter or nullPtr

foreign import ccall unsafe "hkl.h hkl_geometry_name_get"
  c_hkl_geometry_name_get :: Ptr HklGeometry -> IO CString


withGeometry ::  Geometry -> (Ptr HklGeometry -> IO b) -> IO b
withGeometry g fun = do
  fptr <- newGeometry g
  withForeignPtr fptr fun

newGeometry :: Geometry -> IO (ForeignPtr HklGeometry)
newGeometry (Geometry f (Source lw) vs _ps) = do
  let wavelength = CDouble (lw /~ nano meter)
  factory <- newFactory f
  geometry <- c_hkl_factory_create_new_geometry factory
  c_hkl_geometry_wavelength_set geometry wavelength unit nullPtr
  darray <- c_hkl_geometry_axis_names_get geometry
  n <- darrayStringLen darray
  V.unsafeWith vs $ \values ->
      c_hkl_geometry_axis_values_set geometry values n unit nullPtr

  newForeignPtr c_hkl_geometry_free geometry

foreign import ccall unsafe "hkl.h hkl_factory_create_new_geometry"
  c_hkl_factory_create_new_geometry :: Ptr HklFactory -> IO (Ptr HklGeometry)

foreign import ccall unsafe "hkl.h &hkl_geometry_free"
  c_hkl_geometry_free :: FunPtr (Ptr HklGeometry -> IO ())

foreign import ccall unsafe "hkl.h hkl_geometry_wavelength_set"
  c_hkl_geometry_wavelength_set :: Ptr HklGeometry -- geometry
                                -> CDouble -- wavelength
                                -> CInt -- unit
                                -> Ptr () -- *gerror
                                -> IO () -- IO CInt but for now do not deal with the errors

foreign import ccall unsafe "hkl.h hkl_geometry_axis_values_set"
  c_hkl_geometry_axis_values_set :: Ptr HklGeometry -- geometry
                                 -> Ptr Double -- axis values
                                 -> CSize -- size of axis values
                                 -> CInt -- unit
                                 -> Ptr () -- gerror
                                 -> IO () -- IO CInt but for now do not deal with the errors

-- geometryName :: ForeignPtr HklGeometry -> IO String
-- geometryName g = withForeignPtr g (c_hkl_geometry_name_get >=> peekCString)

-- HklGeometryList

buildMatrix' :: Element a => CInt -> CInt -> ((CInt, CInt) -> IO a) -> IO (Matrix a)
buildMatrix' rc cc f = do
   let coordinates' = map (\ ri -> map (\ ci -> (ri, ci)) [0 .. (cc - 1)]) [0 .. (rc - 1)]
   l <- mapM (mapM f) coordinates'
   return $ fromLists l


    -- fromLists $ map (map f)
    --     $ map (\ ri -> map (\ ci -> (ri, ci)) [0 .. (cc - 1)]) [0 .. (rc - 1)]

geometryDetectorRotationGet :: Geometry -> Detector a -> IO (Matrix Double)
geometryDetectorRotationGet g d  = do
  f_geometry <- newGeometry g
  f_detector <- newDetector d
  withForeignPtr f_detector $ \detector ->
    withForeignPtr f_geometry $ \geometry -> do
      f_q <- newForeignPtr c_hkl_quaternion_free =<< c_hkl_geometry_detector_rotation_get_binding geometry detector
      withForeignPtr f_q $ \quaternion -> do
        f_m <- newForeignPtr c_hkl_matrix_free =<< c_hkl_quaternion_to_matrix_binding quaternion
        withForeignPtr f_m $ \matrix ->
          buildMatrix' 3 3 (getV matrix)
          where
            getV :: Ptr HklMatrix -> (CInt, CInt) -> IO Double
            getV m (i', j') = do
              (CDouble v) <- c_hkl_matrix_get m i' j'
              return v

foreign import ccall unsafe "hkl.h hkl_geometry_detector_rotation_get_binding"
  c_hkl_geometry_detector_rotation_get_binding :: Ptr HklGeometry
                                               -> Ptr HklDetector
                                               -> IO (Ptr HklQuaternion)

foreign import ccall unsafe "hkl.h hkl_quaternion_to_matrix_binding"
  c_hkl_quaternion_to_matrix_binding :: Ptr HklQuaternion
                                     -> IO (Ptr HklMatrix)

foreign import ccall unsafe "hkl.h &hkl_quaternion_free"
  c_hkl_quaternion_free :: FunPtr (Ptr HklQuaternion -> IO ())

foreign import ccall unsafe "hkl.h &hkl_matrix_free"
  c_hkl_matrix_free :: FunPtr (Ptr HklMatrix -> IO ())

foreign import ccall unsafe "hkl.h hkl_matrix_get"
  c_hkl_matrix_get :: Ptr HklMatrix
                   -> CInt
                   -> CInt
                   -> IO CDouble


peekItems :: Ptr HklGeometryList -> IO [Ptr HklGeometryListItem]
peekItems l = c_hkl_geometry_list_items_first_get l >>= unfoldrM go
   where
      go e
         | e == nullPtr = return Nothing
         | otherwise    = do
               next <- c_hkl_geometry_list_items_next_get l e
               return (Just (e, next))

peekHklGeometryList :: ForeignPtr HklGeometryList -> IO [Geometry]
peekHklGeometryList l = withForeignPtr l $ \ls -> do
  items <- peekItems ls
  mapM extract items
    where
      extract it = c_hkl_geometry_list_item_geometry_get it >>= peekGeometry

foreign import ccall unsafe "hkl.h hkl_geometry_list_items_first_get"
  c_hkl_geometry_list_items_first_get :: Ptr HklGeometryList
                                      -> IO (Ptr HklGeometryListItem)

foreign import ccall unsafe "hkl.h hkl_geometry_list_items_next_get"
  c_hkl_geometry_list_items_next_get :: Ptr HklGeometryList
                                     -> Ptr HklGeometryListItem
                                     -> IO (Ptr HklGeometryListItem)

foreign import ccall unsafe "hkl.h hkl_geometry_list_item_geometry_get"
  c_hkl_geometry_list_item_geometry_get :: Ptr HklGeometryListItem
                                        -> IO (Ptr HklGeometry)

-- Detector
withDetector :: Detector a -> (Ptr HklDetector -> IO b) -> IO b
withDetector d func = do
  fptr <- newDetector d
  withForeignPtr fptr func

newDetector :: Detector a -> IO (ForeignPtr HklDetector)
newDetector DetectorXpad32 = error "Can not use 2D detector with the hkl library"
newDetector DetectorZeroD = c_hkl_detector_new 0 >>= newForeignPtr c_hkl_detector_free

foreign import ccall unsafe "hkl.h hkl_detector_new"
  c_hkl_detector_new:: CInt -> IO (Ptr HklDetector)

foreign import ccall unsafe "hkl.h &hkl_detector_free"
  c_hkl_detector_free :: FunPtr (Ptr HklDetector -> IO ())

-- Engine

peekMode :: Ptr HklEngine -> IO Mode
peekMode e = do
  name <- c_hkl_engine_current_mode_get e >>= peekCString
  parameters <- c_hkl_engine_parameters_names_get e
                >>= peekDArrayString
                >>= mapM f
  return (Mode name parameters)
  where
    f n = (c_hkl_engine_parameter_get e n nullPtr >>= peek)


foreign import ccall unsafe "hkl.h hkl_engine_current_mode_get"
  c_hkl_engine_current_mode_get :: Ptr HklEngine -> IO CString

foreign import ccall unsafe "hkl.h hkl_engine_parameters_names_get"
  c_hkl_engine_parameters_names_get:: Ptr HklEngine -> IO (Ptr ()) -- darray_string

foreign import ccall unsafe "hkl.h hkl_engine_parameter_get"
  c_hkl_engine_parameter_get:: Ptr HklEngine -> CString -> Ptr () -> IO (Ptr Parameter) -- darray_string


peekEngine :: Ptr HklEngine -> IO Engine
peekEngine e = do
  name <- peekCString =<< c_hkl_engine_name_get e
  ps <- enginePseudoAxesGet e
  mode <- peekMode e
  return (Engine name ps mode)

-- engineNameGet :: Ptr HklEngine -> IO String
-- engineNameGet engine = c_hkl_engine_name_get engine >>= peekCString

foreign import ccall unsafe "hkl.h hkl_engine_name_get"
  c_hkl_engine_name_get :: Ptr HklEngine -> IO CString

enginePseudoAxisNamesGet' :: Ptr HklEngine -> IO [CString]
enginePseudoAxisNamesGet' e = c_hkl_engine_pseudo_axis_names_get e >>= peekDArrayString

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_names_get"
  c_hkl_engine_pseudo_axis_names_get:: Ptr HklEngine -> IO (Ptr ()) -- darray_string

-- enginePseudoAxisNamesGet :: Ptr HklEngine -> IO [String]
-- enginePseudoAxisNamesGet e = enginePseudoAxisNamesGet' e >>= mapM peekCString

enginePseudoAxisGet :: Ptr HklEngine -> CString -> IO Parameter
enginePseudoAxisGet e n = c_hkl_engine_pseudo_axis_get e n nullPtr >>= peek

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_get"
  c_hkl_engine_pseudo_axis_get:: Ptr HklEngine -> CString -> Ptr () -> IO (Ptr Parameter)

enginePseudoAxesGet :: Ptr HklEngine -> IO [Parameter]
enginePseudoAxesGet e = enginePseudoAxisNamesGet' e >>= mapM (enginePseudoAxisGet e)

-- EngineList

withEngineList :: Factory -> (Ptr HklEngineList -> IO b) -> IO b
withEngineList f func = do
  fptr <- newEngineList f
  withForeignPtr fptr func

newEngineList :: Factory -> IO (ForeignPtr HklEngineList)
newEngineList f = newFactory f
                  >>= c_hkl_factory_create_new_engine_list
                  >>= newForeignPtr c_hkl_engine_list_free

foreign import ccall unsafe "hkl.h hkl_factory_create_new_engine_list"
  c_hkl_factory_create_new_engine_list:: Ptr HklFactory -> IO (Ptr HklEngineList)

foreign import ccall unsafe "hkl.h &hkl_engine_list_free"
  c_hkl_engine_list_free :: FunPtr (Ptr HklEngineList -> IO ())

engineListEnginesGet :: Ptr HklEngineList -> IO [Engine]
engineListEnginesGet e = do
  pdarray <- c_hkl_engine_list_engines_get e
  n <- (#{peek darray_engine, size} pdarray) :: IO CSize
  engines <- #{peek darray_engine ,item} pdarray :: IO (Ptr (Ptr HklEngine))
  enginess <- peekArray (fromEnum n) engines
  mapM peekEngine enginess

foreign import ccall unsafe "hkl.h hkl_engine_list_engines_get"
  c_hkl_engine_list_engines_get:: Ptr HklEngineList -> IO (Ptr ())

compute :: Geometry -> Detector a -> Sample -> IO [Engine]
compute g@(Geometry f _ _ _) d s = do
  withSample s $ \sample ->
      withDetector d $ \detector ->
          withGeometry g $ \geometry ->
              withEngineList f $ \engines -> do
                    c_hkl_engine_list_init engines geometry detector sample
                    c_hkl_engine_list_get engines
                    engineListEnginesGet engines

foreign import ccall unsafe "hkl.h hkl_engine_list_init"
  c_hkl_engine_list_init:: Ptr HklEngineList -> Ptr HklGeometry -> Ptr HklDetector -> Ptr HklSample -> IO ()

foreign import ccall unsafe "hkl.h hkl_engine_list_get"
  c_hkl_engine_list_get:: Ptr HklEngineList -> IO ()

-- Lattice

withLattice :: Lattice -> (Ptr HklLattice -> IO b) -> IO b
withLattice l func = do
  fptr <- newLattice l
  withForeignPtr fptr func

newLattice' :: CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> CDouble
            -> IO (ForeignPtr HklLattice)
newLattice' a b c alpha beta gamma = do
  lattice <- c_hkl_lattice_new a b c alpha beta gamma nullPtr
  newForeignPtr c_hkl_lattice_free lattice

newLattice :: Lattice -> IO (ForeignPtr HklLattice)
newLattice  (Cubic la) = do
  let a = CDouble (la /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a a a alpha alpha alpha
newLattice (Tetragonal la lc) = do
  let a = CDouble (la /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a a c alpha alpha alpha
newLattice  (Orthorhombic la lb lc) = do
  let a = CDouble (la /~ nano meter)
  let b = CDouble (lb /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  newLattice' a b c alpha alpha alpha
newLattice (Rhombohedral la aalpha) = do
  let a = CDouble (la /~ nano meter)
  let alpha = CDouble (aalpha /~ radian)
  newLattice' a a a alpha alpha alpha
newLattice (Hexagonal la lc) = do
  let a = CDouble (la /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  let gamma = CDouble ((120 *~ degree) /~ radian)
  newLattice' a a c alpha alpha gamma
newLattice (Monoclinic la lb lc abeta) = do
  let a = CDouble (la /~ nano meter)
  let b = CDouble (lb /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble ((90 *~ degree) /~ radian)
  let beta = CDouble (abeta /~ radian)
  newLattice' a b c alpha beta alpha
newLattice (Triclinic la lb lc aalpha abeta agamma) = do
  let a = CDouble (la /~ nano meter)
  let b = CDouble (lb /~ nano meter)
  let c = CDouble (lc /~ nano meter)
  let alpha = CDouble (aalpha /~ radian)
  let beta = CDouble (abeta /~ radian)
  let gamma = CDouble (agamma /~ radian)
  newLattice' a b c alpha beta gamma

foreign import ccall unsafe "hkl.h hkl_lattice_new"
  c_hkl_lattice_new :: CDouble -- a
                    -> CDouble -- b
                    -> CDouble -- c
                    -> CDouble -- alpha
                    -> CDouble -- beta
                    -> CDouble -- gamma
                    -> Ptr () -- *gerror
                    -> IO (Ptr HklLattice)

foreign import ccall unsafe "hkl.h &hkl_lattice_free"
  c_hkl_lattice_free :: FunPtr (Ptr HklLattice -> IO ())

-- Sample

withSample :: Sample -> (Ptr HklSample -> IO b) -> IO b
withSample s fun = do
  fptr <- newSample s
  withForeignPtr fptr fun

newSample :: Sample -> IO (ForeignPtr HklSample)
newSample (Sample name l ux uy uz) =
    withCString name $ \cname -> do
      sample <- c_hkl_sample_new cname
      withLattice l $ \lattice -> do
          c_hkl_sample_lattice_set sample lattice
          go sample ux c_hkl_sample_ux_get c_hkl_sample_ux_set
          go sample uy c_hkl_sample_uy_get c_hkl_sample_uy_set
          go sample uz c_hkl_sample_uz_get c_hkl_sample_uz_set
          newForeignPtr c_hkl_sample_free sample
            where
              go s p getter setter = do
                fptr <- copyParameter =<< (getter s)
                withForeignPtr fptr $ \ptr -> do
                  poke ptr p
                  void $ setter s ptr nullPtr

foreign import ccall unsafe "hkl.h hkl_sample_new"
  c_hkl_sample_new:: CString -> IO (Ptr HklSample)

foreign import ccall unsafe "hkl.h hkl_sample_lattice_set"
  c_hkl_sample_lattice_set :: Ptr HklSample -> Ptr HklLattice -> IO ()

foreign import ccall unsafe "hkl.h &hkl_sample_free"
  c_hkl_sample_free :: FunPtr (Ptr HklSample -> IO ())

foreign import ccall unsafe "hkl.h hkl_sample_ux_get"
  c_hkl_sample_ux_get :: Ptr HklSample
                      -> IO (Ptr Parameter)

foreign import ccall unsafe "hkl.h hkl_sample_uy_get"
  c_hkl_sample_uy_get :: Ptr HklSample
                      -> IO (Ptr Parameter)

foreign import ccall unsafe "hkl.h hkl_sample_uz_get"
  c_hkl_sample_uz_get :: Ptr HklSample
                      -> IO (Ptr Parameter)

foreign import ccall unsafe "hkl.h hkl_sample_ux_set"
  c_hkl_sample_ux_set :: Ptr HklSample
                      -> Ptr Parameter
                      -> Ptr ()
                      -> IO CInt

foreign import ccall unsafe "hkl.h hkl_sample_uy_set"
  c_hkl_sample_uy_set :: Ptr HklSample
                      -> Ptr Parameter
                      -> Ptr ()
                      -> IO CInt

foreign import ccall unsafe "hkl.h hkl_sample_uz_set"
  c_hkl_sample_uz_set :: Ptr HklSample
                      -> Ptr Parameter
                      -> Ptr ()
                      -> IO CInt
