#ifndef __HKL_PSEUDOAXIS_H__
#define __HKL_PSEUDOAXIS_H__

#include <stdarg.h>
//#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>

#include <hkl/hkl-detector.h>
#include <hkl/hkl-sample.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef int (* HklPseudoAxisEngineFunction) (const gsl_vector *x, void *params, gsl_vector *f);
typedef struct _HklPseudoAxisEngineFunc HklPseudoAxisEngineFunc;
typedef struct _HklPseudoAxisEngineGetter HklPseudoAxisEngineGetter;
typedef struct _HklPseudoAxisEngineSetter HklPseudoAxisEngineSetter;
typedef struct _HklPseudoAxisEngine HklPseudoAxisEngine;

typedef int (* HklPseudoAxisEngineGetterFunc) (HklPseudoAxisEngine *self,
		HklGeometry const *geometry, HklDetector const *detector,
		HklSample const *sample);

typedef int (* HklPseudoAxisEngineSetterFunc) (HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample);

struct _HklPseudoAxis
{
	char const * name;
	HklAxisConfig config;
	HklPseudoAxisEngine *engine;
};

struct _HklPseudoAxisEngineGetter
{
	char const *name;
	HklPseudoAxisEngineGetterFunc f;
	HklParameter *parameters;
	size_t parameters_len;
	char const **axes_names;
	size_t axis_names_len;
};

struct _HklPseudoAxisEngineSetter
{
	char const *name;
	HklPseudoAxisEngineSetterFunc f;
	HklParameter *parameters;
	size_t parameters_len;
	char const **axes_names;
	size_t axis_names_len;
};

struct _HklPseudoAxisEngineFunc
{
	char const *name;
	HklPseudoAxisEngineFunction *f;
	size_t f_len;
	HklParameter *parameters;
	size_t parameters_len;
	char const **axes_names;
	size_t axes_names_len;
};

struct _HklPseudoAxisEngine
{
	char const *name;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	HklPseudoAxisEngineGetter **getters;
	size_t getters_len;
	HklPseudoAxisEngineSetter **setters;
	size_t setters_len;
	HklPseudoAxisEngineFunc **functions;
	size_t functions_len;
	HklPseudoAxisEngineGetter *getter;
	HklPseudoAxisEngineSetter *setter;
	HklPseudoAxisEngineFunc *function;
	HklAxis **axes;
	size_t axes_len;
	HklPseudoAxis *pseudoAxes;
	size_t pseudoAxes_len;
	HklGeometry **geometries;
	size_t geometries_len;
	size_t geometries_alloc;
};

/* HklPseudoAxisEngineFunc part */

extern HklPseudoAxisEngineFunc *hkl_pseudo_axis_engine_func_new(
		char const *name, size_t n, ...);

extern void hkl_pseudo_axis_engine_func_free(HklPseudoAxisEngineFunc *self);

/* HklPseudoAxisEngine part */

extern HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(char const *name,
		size_t n, ...);

extern void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *self);

extern int RUBh_minus_Q(double const *x, void *params, double *f);

extern void hkl_pseudoAxisEngine_set(HklPseudoAxisEngine *self,
		size_t idx_f, HklGeometry *geom, HklDetector *det,
		HklSample *sample);

extern void hkl_pseudoAxisEngine_add_getter(HklPseudoAxisEngine *self,
		HklPseudoAxisEngineGetter *getter);

extern void hkl_pseudoAxisEngine_add_setter(HklPseudoAxisEngine *self,
		HklPseudoAxisEngineSetter *setter);

extern void hkl_pseudoAxisEngine_add_function(HklPseudoAxisEngine *self,
		HklPseudoAxisEngineFunc *function);

extern void hkl_pseudoAxisEngine_select_getter(HklPseudoAxisEngine *self,
		size_t idx);

extern void hkl_pseudoAxisEngine_select_setter(HklPseudoAxisEngine *self,
		size_t idx);

extern void hkl_pseudoAxisEngine_select_function(HklPseudoAxisEngine *self,
		size_t idx);

extern void hkl_pseudoAxisEngine_setter(HklPseudoAxisEngine *self,
		HklGeometry *geom, HklDetector *det, HklSample *sample);

extern void hkl_pseudoAxisEngine_getter(HklPseudoAxisEngine *self,
		HklGeometry *geom, HklDetector *det, HklSample *sample);

extern int hkl_pseudoAxisEngine_to_geometry(HklPseudoAxisEngine *self);

extern int hkl_pseudoAxisEngine_to_pseudoAxes(HklPseudoAxisEngine *self);

extern void hkl_pseudoAxisEngine_fprintf(HklPseudoAxisEngine *self, FILE *f);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
