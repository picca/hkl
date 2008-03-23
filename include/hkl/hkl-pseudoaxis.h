#ifndef __HKL_PSEUDOAXIS_H__
#define __HKL_PSEUDOAXIS_H__

#include <stdarg.h>

#include <hkl/hkl-detector.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef struct _HklPseudoAxisEngine HklPseudoAxisEngine;

struct _HklPseudoAxis
{
	char const * name;
	HklAxisConfig config;
	HklPseudoAxisEngine *engine;
};

struct _HklPseudoAxisEngine
{
	char const *name;
	int is_initialized;
	int is_readable;
	int is_writable;
	HklGeometry *g_init;
	HklList *related_axes;
	HklList *pseudoAxes;
	int (*init) (HklPseudoAxisEngine *engine,
			HklGeometry const *geom,
			HklDetector const *det);
	int (*update) (HklPseudoAxisEngine *engine,
			HklGeometry const *geom,
			HklDetector const *det);
	int (*set) (HklPseudoAxisEngine *engine,
			HklGeometry *geom,
			HklDetector const *det);
};

extern HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(char const *name,
		size_t n, ...);

extern void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *engine);

extern HklPseudoAxis *hkl_pseudoAxisEngine_get_pseudoAxis(
		HklPseudoAxisEngine const *engine, size_t idx);

extern void hkl_pseudoAxis_get_config(HklPseudoAxis const *pseudoAxis,
		HklAxisConfig *config);

extern void hkl_pseudoAxis_set_config(HklPseudoAxis *pseudoAxis,
		HklAxisConfig const *config);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
