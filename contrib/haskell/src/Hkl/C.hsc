{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Hkl.C
       ( compute
       , factories
       , newSample
       , solve
       , solveTraj
       ) where

import Control.Monad
import Control.Monad.Loops (unfoldrM)
import Data.Map.Strict as Map
import Numeric.Units.Dimensional.Prelude ( meter, degree, radian, nano
                                         , (*~), (/~))
import Foreign
import Foreign.C

import Hkl.Types

#include "hkl.h"

-- private types

data HklDetector
data HklEngine
data HklEngineList
data HklGeometry
data HklGeometryList
data HklGeometryListItem
data HklLattice
data HklParameter
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

solve :: Factory -> Geometry -> Detector -> Sample -> Engine -> IO [Geometry]
solve f g d s e@(Engine name _ _) = do
  f_sample <- newSample s
  f_detector <- newDetector d
  f_engines <- newEngineList f
  withForeignPtr f_sample $ \sample ->
      withForeignPtr f_detector $ \detector ->
          withGeometry f g $ \geometry ->
              withForeignPtr f_engines $ \engines ->
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

solveTraj :: Factory -> Geometry -> Detector -> Sample -> [Engine] -> IO [Geometry]
solveTraj f g d s es = do
  let name = engineName (head es)
  f_sample <- newSample s
  f_detector <- newDetector d
  f_engines <- newEngineList f
  withForeignPtr f_sample $ \sample ->
      withForeignPtr f_detector $ \detector ->
          withGeometry f g $ \geometry ->
              withForeignPtr f_engines $ \engines ->
                withCString name $ \cname -> do
                  c_hkl_engine_list_init engines geometry detector sample
                  engine <- c_hkl_engine_list_engine_get_by_name engines cname nullPtr
                  n <- c_hkl_engine_pseudo_axis_names_get engine >>= darrayStringLen
                  mapM (\e -> solve' engine n e >>= getSolution0) es

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

factories :: IO (Map String Factory)
factories = do
  fs <- factoryGetAll
  ns <- mapM factoryNameGet fs
  return $ fromList $ zip ns fs

{-# NOINLINE factoryGetAll #-}
factoryGetAll :: IO [Factory]
factoryGetAll =  alloca $ \ptr -> do
                   fact <- c_hkl_factory_get_all ptr
                   n <- peek ptr
                   hklfactories <- peekArray n fact
                   return $ fmap Factory hklfactories

foreign import ccall unsafe "hkl.h hkl_factory_get_all"
  c_hkl_factory_get_all :: Ptr Int -> IO (Ptr (Ptr HklFactory))

factoryNameGet :: Factory -> IO String
factoryNameGet (Factory factory) = c_hkl_factory_name_get factory >>= peekCString

foreign import ccall unsafe "hkl.h hkl_factory_name_get"
  c_hkl_factory_name_get :: Ptr HklFactory -> IO CString


-- Geometry

peekGeometry :: Ptr HklGeometry -> IO (Geometry)
peekGeometry gp = do
  (CDouble w) <- c_hkl_geometry_wavelength_get gp unit
  darray <- c_hkl_geometry_axis_names_get gp
  n <- darrayStringLen darray
  let nn = fromEnum n
  allocaArray nn $ \values -> do
    c_hkl_geometry_axis_values_get gp values n unit
    vs <- peekArray nn values
    return $ Geometry (Source (w *~ nano meter)) vs

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

withGeometry ::  Factory -> Geometry -> (Ptr HklGeometry -> IO b) -> IO b
withGeometry f g fun = do
  fptr <- newGeometry f g
  withForeignPtr fptr fun

newGeometry :: Factory -> Geometry -> IO (ForeignPtr HklGeometry)
newGeometry (Factory f) (Geometry (Source lw) vs) = do
  let wavelength = CDouble (lw /~ nano meter)
  geometry <- c_hkl_factory_create_new_geometry f
  c_hkl_geometry_wavelength_set geometry wavelength unit nullPtr
  darray <- c_hkl_geometry_axis_names_get geometry
  n <- darrayStringLen darray
  withArray vs $ \values ->
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

geometryName :: ForeignPtr HklGeometry -> IO String
geometryName g = withForeignPtr g (c_hkl_geometry_name_get >=> peekCString)

foreign import ccall unsafe "hkl.h hkl_geometry_name_get"
  c_hkl_geometry_name_get :: Ptr HklGeometry -> IO CString


geometryAxisNamesGet' :: ForeignPtr HklGeometry -> IO [CString]
geometryAxisNamesGet' g =
  withForeignPtr g (c_hkl_geometry_axis_names_get >=> peekDArrayString)

foreign import ccall unsafe "hkl.h hkl_geometry_axis_names_get"
  c_hkl_geometry_axis_names_get :: Ptr HklGeometry -- goemetry
                                -> IO (Ptr ()) -- darray_string

geometryAxisGet :: ForeignPtr HklGeometry -> CString -> IO Parameter
geometryAxisGet g n =
    withForeignPtr g $ \gp ->
        c_hkl_geometry_axis_get gp n nullPtr >>= peekParameter

foreign import ccall unsafe "hkl.h hkl_geometry_axis_get"
  c_hkl_geometry_axis_get :: Ptr HklGeometry -- geometry
                          -> CString -- axis name
                          -> Ptr () -- gerror
                          -> IO (Ptr HklParameter) -- parameter or nullPtr

geometryAxesGet :: ForeignPtr HklGeometry -> IO [Parameter]
geometryAxesGet g = geometryAxisNamesGet' g >>= mapM (geometryAxisGet g)

-- HklGeometryList

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

newDetector :: Detector -> IO (ForeignPtr HklDetector)
newDetector (Detector t) =
  c_hkl_detector_new it >>= newForeignPtr c_hkl_detector_free
      where
        it = case t of
             DetectorType0D -> 0

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
    f n = (c_hkl_engine_parameter_get e n nullPtr >>= peekParameter)


foreign import ccall unsafe "hkl.h hkl_engine_current_mode_get"
  c_hkl_engine_current_mode_get :: Ptr HklEngine -> IO CString

foreign import ccall unsafe "hkl.h hkl_engine_parameters_names_get"
  c_hkl_engine_parameters_names_get:: Ptr HklEngine -> IO (Ptr ()) -- darray_string

foreign import ccall unsafe "hkl.h hkl_engine_parameter_get"
  c_hkl_engine_parameter_get:: Ptr HklEngine -> CString -> Ptr () -> IO (Ptr HklParameter) -- darray_string


peekEngine :: Ptr HklEngine -> IO Engine
peekEngine e = do
  name <- peekCString =<< c_hkl_engine_name_get e
  ps <- enginePseudoAxesGet e
  mode <- peekMode e
  return (Engine name ps mode)

engineNameGet :: Ptr HklEngine -> IO String
engineNameGet engine = c_hkl_engine_name_get engine >>= peekCString

foreign import ccall unsafe "hkl.h hkl_engine_name_get"
  c_hkl_engine_name_get :: Ptr HklEngine -> IO CString

enginePseudoAxisNamesGet' :: Ptr HklEngine -> IO [CString]
enginePseudoAxisNamesGet' e = c_hkl_engine_pseudo_axis_names_get e >>= peekDArrayString

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_names_get"
  c_hkl_engine_pseudo_axis_names_get:: Ptr HklEngine -> IO (Ptr ()) -- darray_string

enginePseudoAxisNamesGet :: Ptr HklEngine -> IO [String]
enginePseudoAxisNamesGet e = enginePseudoAxisNamesGet' e >>= mapM peekCString

enginePseudoAxisGet :: Ptr HklEngine -> CString -> IO Parameter
enginePseudoAxisGet e n = c_hkl_engine_pseudo_axis_get e n nullPtr >>= peekParameter

foreign import ccall unsafe "hkl.h hkl_engine_pseudo_axis_get"
  c_hkl_engine_pseudo_axis_get:: Ptr HklEngine -> CString -> Ptr () -> IO (Ptr HklParameter)

enginePseudoAxesGet :: Ptr HklEngine -> IO [Parameter]
enginePseudoAxesGet e = enginePseudoAxisNamesGet' e >>= mapM (enginePseudoAxisGet e)

-- EngineList

newEngineList :: Factory -> IO (ForeignPtr HklEngineList)
newEngineList (Factory f) =
  c_hkl_factory_create_new_engine_list f >>= newForeignPtr c_hkl_engine_list_free

foreign import ccall unsafe "hkl.h hkl_factory_create_new_engine_list"
  c_hkl_factory_create_new_engine_list:: Ptr HklFactory -> IO (Ptr HklEngineList)

foreign import ccall unsafe "hkl.h &hkl_engine_list_free"
  c_hkl_engine_list_free :: FunPtr (Ptr HklEngineList -> IO ())

engineListEnginesGet :: ForeignPtr HklEngineList -> IO [Engine]
engineListEnginesGet e = withForeignPtr e $ \ep -> do
  pdarray <- c_hkl_engine_list_engines_get ep
  n <- (#{peek darray_engine, size} pdarray) :: IO CSize
  engines <- #{peek darray_engine ,item} pdarray :: IO (Ptr (Ptr HklEngine))
  enginess <- peekArray (fromEnum n) engines
  mapM peekEngine enginess

foreign import ccall unsafe "hkl.h hkl_engine_list_engines_get"
  c_hkl_engine_list_engines_get:: Ptr HklEngineList -> IO (Ptr ())

compute :: Factory -> Geometry -> Detector -> Sample -> IO [Engine]
compute f g d s = do
  fptr_e <- newEngineList f
  fptr_d <- newDetector d
  fptr_s <- newSample s
  withForeignPtr fptr_s $ \sample ->
      withForeignPtr fptr_d $ \detector ->
          withGeometry f g $ \geometry ->
              withForeignPtr fptr_e $ \engines -> do
                    c_hkl_engine_list_init engines geometry detector sample
                    c_hkl_engine_list_get engines
                    engineListEnginesGet fptr_e

foreign import ccall unsafe "hkl.h hkl_engine_list_init"
  c_hkl_engine_list_init:: Ptr HklEngineList -> Ptr HklGeometry -> Ptr HklDetector -> Ptr HklSample -> IO ()

foreign import ccall unsafe "hkl.h hkl_engine_list_get"
  c_hkl_engine_list_get:: Ptr HklEngineList -> IO ()

-- Lattice

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

-- Parameter

peekParameter :: Ptr HklParameter -> IO Parameter
peekParameter p =
    alloca $ \pmin ->
        alloca $ \pmax -> do
          cname <- c_hkl_parameter_name_get p
          name <- peekCString cname
          value <- c_hkl_parameter_value_get p unit
          c_hkl_parameter_min_max_get p pmin pmax unit
          min <- peek pmin
          max <- peek pmax
          return (Parameter name value (Range min max))

foreign import ccall unsafe "hkl.h hkl_parameter_name_get"
  c_hkl_parameter_name_get:: Ptr HklParameter -> IO CString

foreign import ccall unsafe "hkl.h hkl_parameter_value_get"
  c_hkl_parameter_value_get:: Ptr HklParameter -> CInt -> IO Double

foreign import ccall unsafe "hkl.h hkl_parameter_min_max_get"
  c_hkl_parameter_min_max_get :: Ptr HklParameter -> Ptr Double -> Ptr Double -> CInt -> IO ()

copyParameter :: Ptr HklParameter -> IO (ForeignPtr HklParameter)
copyParameter p = newForeignPtr c_hkl_parameter_free =<< c_hkl_parameter_new_copy p

foreign import ccall unsafe "hkl.h &hkl_parameter_free"
  c_hkl_parameter_free :: FunPtr (Ptr HklParameter -> IO ())

foreign import ccall unsafe "hkl.h hkl_parameter_new_copy"
  c_hkl_parameter_new_copy:: Ptr HklParameter -> IO (Ptr HklParameter)

pokeParameter :: Parameter -> Ptr HklParameter -> IO ()
pokeParameter (Parameter name value (Range min max)) parameter = do
  c_hkl_parameter_value_set parameter (CDouble value) unit nullPtr
  c_hkl_parameter_min_max_set parameter (CDouble min) (CDouble max) unit nullPtr
  return ()

foreign import ccall unsafe "hkl.h hkl_parameter_value_set"
  c_hkl_parameter_value_set:: Ptr HklParameter -> CDouble -> CInt -> Ptr () -> IO (CInt)

foreign import ccall unsafe "hkl.h hkl_parameter_min_max_set"
  c_hkl_parameter_min_max_set :: Ptr HklParameter -> CDouble -> CDouble -> CInt -> Ptr () -> IO (CInt)

-- Sample

newSample :: Sample -> IO (ForeignPtr HklSample)
newSample (Sample name l ux uy uz) =
    withCString name $ \cname -> do
      sample <- c_hkl_sample_new cname
      fptr_l <- newLattice l
      withForeignPtr fptr_l $ \lattice -> do
          c_hkl_sample_lattice_set sample lattice
          go sample ux c_hkl_sample_ux_get c_hkl_sample_ux_set
          go sample uy c_hkl_sample_uy_get c_hkl_sample_uy_set
          go sample uz c_hkl_sample_uz_get c_hkl_sample_uz_set
          newForeignPtr c_hkl_sample_free sample
            where
              go s p get set = do
                fptr <- copyParameter =<< (get s)
                withForeignPtr fptr $ \ptr -> do
                  pokeParameter p ptr
                  set s ptr nullPtr
                  return ()

foreign import ccall unsafe "hkl.h hkl_sample_new"
  c_hkl_sample_new:: CString -> IO (Ptr HklSample)

foreign import ccall unsafe "hkl.h hkl_sample_lattice_set"
  c_hkl_sample_lattice_set :: Ptr HklSample -> Ptr HklLattice -> IO ()

foreign import ccall unsafe "hkl.h &hkl_sample_free"
  c_hkl_sample_free :: FunPtr (Ptr HklSample -> IO ())

foreign import ccall unsafe "hkl.h hkl_sample_ux_get"
  c_hkl_sample_ux_get :: Ptr HklSample
                      -> IO (Ptr HklParameter)

foreign import ccall unsafe "hkl.h hkl_sample_uy_get"
  c_hkl_sample_uy_get :: Ptr HklSample
                      -> IO (Ptr HklParameter)

foreign import ccall unsafe "hkl.h hkl_sample_uz_get"
  c_hkl_sample_uz_get :: Ptr HklSample
                      -> IO (Ptr HklParameter)

foreign import ccall unsafe "hkl.h hkl_sample_ux_set"
  c_hkl_sample_ux_set :: Ptr HklSample
                      -> Ptr HklParameter
                      -> Ptr ()
                      -> IO CInt

foreign import ccall unsafe "hkl.h hkl_sample_uy_set"
  c_hkl_sample_uy_set :: Ptr HklSample
                      -> Ptr HklParameter
                      -> Ptr ()
                      -> IO CInt

foreign import ccall unsafe "hkl.h hkl_sample_uz_set"
  c_hkl_sample_uz_set :: Ptr HklSample
                      -> Ptr HklParameter
                      -> Ptr ()
                      -> IO CInt
