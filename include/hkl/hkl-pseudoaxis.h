#ifndef __HKL_PSEUDOAXIS_H__
#define __HKL_PSEUDOAXIS_H__

#include <stdarg.h>
#include <gsl/gsl_vector.h>

#include <hkl/hkl-detector.h>
#include <hkl/hkl-sample.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef struct _HklPseudoAxisEngine HklPseudoAxisEngine;
typedef struct _HklPseudoAxisEngineWorkSpace HklPseudoAxisEngineWorkSpace;

struct _HklPseudoAxis
{
	char const * name;
	HklAxisConfig config;
	HklPseudoAxisEngine *engine;
};

struct _HklPseudoAxisEngineWorkSpace
{
	HklGeometry *geom;
	HklDetector *det;
	HklSample *sample;
};

struct _HklPseudoAxisEngine
{
	char const *name;
	int is_initialized;
	int is_readable;
	int is_writable;
	HklGeometry *g_init;
	HklList *related_axes_idx;
	HklList *pseudoAxes;
	HklPseudoAxisEngineWorkSpace w;
	int (*init) (HklPseudoAxisEngine *engine);
	int (*update) (HklPseudoAxisEngine *engine);
	double (*set) (gsl_vector const *x, void *params);
};

extern HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(char const *name,
		size_t n, ...);

extern void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *engine);

extern HklPseudoAxis *hkl_pseudoAxisEngine_get_pseudoAxis(
		HklPseudoAxisEngine const *engine, size_t idx);

extern void hkl_pseudoAxisEngine_set_related_axes(HklPseudoAxisEngine *engine,
		size_t n, ...);

extern int hkl_pseudoAxisEngine_from_pseudoAxes(HklPseudoAxisEngine *engine,
		HklGeometry *geom, HklDetector *det, HklSample *sample);

extern int hkl_pseudoAxisEngine_to_pseudoAxes(HklPseudoAxisEngine *engine,
		HklGeometry *geom, HklDetector *det, HklSample *sample);

extern void hkl_pseudoAxis_get_config(HklPseudoAxis const *pseudoAxis,
		HklAxisConfig *config);

extern void hkl_pseudoAxis_set_config(HklPseudoAxis *pseudoAxis,
		HklAxisConfig const *config);

extern int hkl_pseudoAxisEngine_hkl_update(HklPseudoAxisEngine *engine);

extern double hkl_pseudoAxisEngine_hkl_set(gsl_vector const *x, void *params);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
