
/* Generated data (by glib-mkenums) */

#undef HKL_DISABLE_DEPRECATED
#define HKL_ENABLE_BROKEN
#include <glib-object.h>
#include "hkl.h"
#include "hkl-type-builtins.h"
/* enumerations from "hkl-detector-factory.h" */
GType
hkl_detector_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { HKL_DETECTOR_TYPE_0D, "HKL_DETECTOR_TYPE_0D", "0d" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("HklDetectorType"), values);
    }
    return etype;
}

/* enumerations from "hkl-geometry.h" */
GType
hkl_geometry_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { HKL_GEOMETRY_TYPE_TWOC_VERTICAL, "HKL_GEOMETRY_TYPE_TWOC_VERTICAL", "twoc-vertical" },
            { HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL, "HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL", "eulerian4c-vertical" },
            { HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL, "HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL", "kappa4c-vertical" },
            { HKL_GEOMETRY_TYPE_EULERIAN6C, "HKL_GEOMETRY_TYPE_EULERIAN6C", "eulerian6c" },
            { HKL_GEOMETRY_TYPE_KAPPA6C, "HKL_GEOMETRY_TYPE_KAPPA6C", "kappa6c" },
            { HKL_GEOMETRY_TYPE_ZAXIS, "HKL_GEOMETRY_TYPE_ZAXIS", "zaxis" },
            { HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2, "HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2", "soleil-sixs-med-2-2" },
            { HKL_GEOMETRY_TYPE_SOLEIL_MARS, "HKL_GEOMETRY_TYPE_SOLEIL_MARS", "soleil-mars" },
            { HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2, "HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2", "soleil-sixs-med-1-2" },
            { HKL_GEOMETRY_TYPE_PETRA3_P09_EH2, "HKL_GEOMETRY_TYPE_PETRA3_P09_EH2", "petra3-p09-eh2" },
            { HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3, "HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3", "soleil-sixs-med-2-3" },
            { HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL, "HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL", "eulerian4c-horizontal" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("HklGeometryType"), values);
    }
    return etype;
}

/* enumerations from "hkl-sample.h" */
GType
hkl_sample_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { HKL_SAMPLE_TYPE_MONOCRYSTAL, "HKL_SAMPLE_TYPE_MONOCRYSTAL", "monocrystal" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("HklSampleType"), values);
    }
    return etype;
}

/* enumerations from "hkl-unit.h" */
GType
hkl_unit_type_get_type (void)
{
    static GType etype = 0;
    if (G_UNLIKELY(etype == 0)) {
        static const GEnumValue values[] = {
            { HKL_UNIT_ANGLE_DEG, "HKL_UNIT_ANGLE_DEG", "angle-deg" },
            { HKL_UNIT_ANGLE_RAD, "HKL_UNIT_ANGLE_RAD", "angle-rad" },
            { HKL_UNIT_LENGTH_NM, "HKL_UNIT_LENGTH_NM", "length-nm" },
            { 0, NULL, NULL }
        };
        etype = g_enum_register_static (g_intern_static_string ("HklUnitType"), values);
    }
    return etype;
}

#define __HKL_TYPE_BUILTINS_C__

/* Generated data ends here */

