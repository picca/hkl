
/* Generated data (by glib-mkenums) */

#undef HKL_DISABLE_DEPRECATED
#define HKL_ENABLE_BROKEN
#include <glib-object.h>
#include "hkl.h"
#include "hkl-type-builtins.h"
/* enumerations from "../hkl.h" */
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

#define __HKL_TYPE_BUILTINS_C__

/* Generated data ends here */

