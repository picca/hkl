#ifndef __HKL_PSEUDOAXIS_H__
#define __HKL_PSEUDOAXIS_H__

#include <stdarg.h>
//#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>

#include <hkl/hkl-detector.h>
#include <hkl/hkl-sample.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef struct _HklPseudoAxisEngine HklPseudoAxisEngine;
typedef struct _HklPseudoAxisEngineFunc HklPseudoAxisEngineFunc;
typedef struct _HklPseudoAxisEngineType HklPseudoAxisEngineType;

struct _HklPseudoAxis
{
	char const * name;
	HklAxisConfig config;
	HklPseudoAxisEngine *engine;
};

struct _HklPseudoAxisEngine
{
	HklPseudoAxisEngineType const *type;
	void *state;
	char const *name;
	int is_initialized;
	int is_readable;
	int is_writable;
	HklGeometry *g_init;
	HklGeometry *geom;
	HklDetector *det;
	HklSample *sample;
	HklPseudoAxisEngineFunc *function;
	gsl_vector_uint *related_axes_idx;
	HklList *pseudoAxes;
};

struct _HklPseudoAxisEngineFunc
{
	gsl_multiroot_function f;
};

struct _HklPseudoAxisEngineType
{
	const char *name;
	size_t size;
	int (*alloc) (void *vstate, size_t n);
	int (*set) (void *vstate, HklPseudoAxisEngineFunc *function,
			HklPseudoAxisEngine *engine);
	int (*to_geometry) (void *vstate, HklPseudoAxisEngineFunc *function,
			HklPseudoAxisEngine *engine);
	int (*to_pseudoAxes) (void *vstate,
			HklPseudoAxisEngineFunc *function,
			HklPseudoAxisEngine *engine);
	int (*equiv_geometries) (void *vstate,
			HklPseudoAxisEngineFunc *function,
			HklPseudoAxisEngine *engine);
	void (*free) (void *vstate);
};

extern HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(
		HklPseudoAxisEngineType const *T, char const *name,
		size_t n, ...);

extern void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *engine);

extern void hkl_pseudoAxisEngine_set(HklPseudoAxisEngine *engine,
		HklPseudoAxisEngineFunc *function, HklGeometry *geom,
		HklDetector *det, HklSample *sample,
		size_t n, ...);

extern int hkl_pseudoAxisEngine_to_geometry(HklPseudoAxisEngine *engine);

extern int hkl_pseudoAxisEngine_to_pseudoAxes(HklPseudoAxisEngine *engine);

extern HklPseudoAxis *hkl_pseudoAxisEngine_get_pseudoAxis(
		HklPseudoAxisEngine const *engine, size_t idx);

extern void hkl_pseudoAxis_get_config(HklPseudoAxis const *pseudoAxis,
		HklAxisConfig *config);

extern void hkl_pseudoAxis_set_config(HklPseudoAxis *pseudoAxis,
		HklAxisConfig const *config);

extern int hkl_pseudoAxis_get_equiv_geometries(HklPseudoAxisEngine *engine);

extern HklPseudoAxisEngineType const *hkl_pseudoAxisEngine_type_auto;

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
