module Main where

-- import Graphics.UI.Gtk
import Control.Monad
import Prelude hiding (lookup)
-- import System.FilePath ((</>))
import Data.Map.Strict (lookup)
import Data.Maybe (isNothing, fromJust)

-- import Paths_hkl
import qualified Hkl

ghklUi :: FilePath
ghklUi = "ghkl.ui"

tmpGhklUi :: FilePath
tmpGhklUi = "../../gui/ghkl.ui"

-- diffractometer :: Hkl.Diffractometer
-- diffractometer = Hkl.Diffractometer "ZAXIS"


main :: IO ()
main = do
    -- initGUI
  factories <- Hkl.factories
  let mfactory = lookup "E6C" factories
  if isNothing mfactory
     then
         return $ error $ "wrong diffractometer:" ++ show factories
  else do
    let factory = fromJust mfactory
    engines <- Hkl.newEngineList factory
    geometry <- Hkl.newGeometry factory
    axes <- Hkl.geometryAxesGet geometry
    detector <- Hkl.newDetector Hkl.DetectorType0D
    msample <- Hkl.newSample "test"
    if isNothing msample
    then
        return $ error "Please provide a valid sample name"
    else do
      let sample = fromJust msample
      Hkl.engineListInit engines geometry detector sample
      Hkl.engineListGet engines
      pseudoAxes <- Hkl.engineListPseudoAxesGet engines
      values <- Hkl.geometryAxisValuesGet geometry
      print pseudoAxes
      print axes
      print values
      let toto = [1, 2, 3, 4, 5, 6]
      Hkl.geometryAxisValuesSet geometry toto
      values <- Hkl.geometryAxisValuesGet geometry
      print values
      pseudoAxes <- Hkl.engineListPseudoAxesGet engines
      print pseudoAxes
      Hkl.engineListGet engines
      pseudoAxes <- Hkl.engineListPseudoAxesGet engines
      print pseudoAxes
    -- builder <- builderNew
    -- ui <- getDataFileName ghklUi
    -- builderAddFromFile builder tmpGhklUi

    -- mainWindow <- builderGetObject builder castToWindow "window1"
    -- onDestroy mainWindow mainQuit


    -- -- liststore
    -- liststoreDiffractometer <- builderGetObject builder castToTreeModel "liststore_diffractometer"
    -- liststoreAxis <- builderGetObject builder castToTreeModel "liststore_axis"
    -- liststorePseudoAxes <- builderGetObject builder castToTreeModel "liststore_pseudo_axes"
    -- liststoreReflections <- builderGetObject builder castToTreeModel "liststore_reflections"
    -- liststoreCrystals <- builderGetObject builder castToTreeModel "liststore_crystals"

    -- labelUB11 <- builderGetObject builder castToLabel "label_UB11"
    -- labelUB12 <- builderGetObject builder castToLabel "label_UB12"
    -- labelUB13 <- builderGetObject builder castToLabel "label_UB13"
    -- labelUB21 <- builderGetObject builder castToLabel "label_UB21"
    -- labelUB22 <- builderGetObject builder castToLabel "label_UB22"
    -- labelUB23 <- builderGetObject builder castToLabel "label_UB23"
    -- labelUB31 <- builderGetObject builder castToLabel "label_UB31"
    -- labelUB32 <- builderGetObject builder castToLabel "label_UB32"
    -- labelUB33 <- builderGetObject builder castToLabel "label_UB33"

    -- get_object(builder, GTK_BUTTON, priv, button2);
    -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_a);
    -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_a_min);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_a_max);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_a_star);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_b);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_b_min);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_b_max);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_b_star);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_c);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_c_min);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_c_max);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_c_star);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_alpha);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_alpha_min);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_alpha_max);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_alpha_star);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_beta);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_beta_min);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_beta_max);
  -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_beta_star);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_gamma);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_gamma_min);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_gamma_max);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_gamma_star);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_lambda);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_ux);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_uy);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_uz);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U11);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U12);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U13);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U21);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U22);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U23);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U31);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U32);
   -- get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U33);
   -- get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_a);
   -- get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_b);
   -- get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_c);
   -- get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_alpha);
   -- get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_beta);
   -- get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_gamma);
   -- get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_ux);
   -- get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_uy);
   -- get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_uz);
   -- get_object(builder, GTK_TREE_VIEW, priv, treeview_reflections);
   -- get_object(builder, GTK_TREE_VIEW, priv, treeview_crystals);
   -- get_object(builder, GTK_TREE_VIEW, priv, treeview_axes);
   -- get_object(builder, GTK_TREE_VIEW, priv, treeview_pseudo_axes);
   -- get_object(builder, GTK_TREE_VIEW, priv, treeview_solutions);
   -- get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_add_reflection);
   -- get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_goto_reflection);
   -- get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_del_reflection);
   -- get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_setUB);
   -- get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_computeUB);
   -- get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_add_crystal);
   -- get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_copy_crystal);
   -- get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_del_crystal);
   -- get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_affiner);
   -- get_object(builder, GTK_STATUSBAR, priv, statusbar);
   -- get_object(builder, GTK_IMAGE_MENU_ITEM, priv, menuitem5);
   -- get_object(builder, GTK_VBOX, priv, vbox7);
   -- get_object(builder, GTK_VBOX, priv, vbox2);
   -- get_object(builder, GTK_VBOX, priv, box_info_bar);

   -- get_object(builder, GTK_DIALOG, priv, dialog1);

   -- get_object(builder, GTK_COMBO_BOX, priv, combobox1);

   -- gtk_builder_connect_signals (builder, self);

    -- widgetShowAll mainWindow
    -- mainGUI
