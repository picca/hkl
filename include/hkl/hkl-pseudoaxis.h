#ifndef __HKL_PSEUDOAXIS_H__
#define __HKL_PSEUDOAXIS_H__

#include <stdarg.h>
#include <gsl/gsl_multiroots.h>

#include <hkl/hkl-detector.h>
#include <hkl/hkl-sample.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef struct _HklPseudoAxisEngineMode HklPseudoAxisEngineMode;
typedef struct _HklPseudoAxisEngine HklPseudoAxisEngine;
typedef struct _HklPseudoAxisEngineList HklPseudoAxisEngineList;

typedef int (* HklPseudoAxisEngineInitFunc) (HklPseudoAxisEngine *self,
					     HklGeometry *geometry,
					     HklDetector const *detector,
					     HklSample const *sample);

typedef int (* HklPseudoAxisEngineGetterFunc) (HklPseudoAxisEngine *self,
					       HklGeometry *geometry,
					       HklDetector const *detector,
					       HklSample const *sample);

typedef int (* HklPseudoAxisEngineSetterFunc) (HklPseudoAxisEngine *self,
					       HklGeometry *geometry,
					       HklDetector *detector,
					       HklSample *sample);

struct _HklPseudoAxis
{
	HklParameter parent;
	HklPseudoAxisEngine *engine;
};

struct _HklPseudoAxisEngineMode
{
	char const *name;
	HklPseudoAxisEngineInitFunc init;
	HklPseudoAxisEngineGetterFunc get;
	HklPseudoAxisEngineSetterFunc set;
	HKL_LIST(HklParameter, parameters);
	HKL_LIST(const char*, axes_names);
	HklGeometry *geometry_init;
	HklDetector detector_init;
	HklSample *sample_init;
};

struct _HklPseudoAxisEngine
{
	char const *name;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	HKL_LIST(HklPseudoAxisEngineMode *, modes);
	HKL_LIST(HklAxis *, axes);
	HKL_LIST(HklPseudoAxis *, pseudoAxes);
	HklPseudoAxisEngineMode *mode;
	HklGeometry **geometries;
	size_t geometries_len;
	size_t geometries_alloc;
};

struct _HklPseudoAxisEngineList
{
	HKL_LIST(HklPseudoAxisEngine *, engines);
};

/*****************/
/* HklPseudoAxis */
/*****************/

extern HklPseudoAxis *hkl_pseudo_axis_new(HklParameter const *parameter,
					  HklPseudoAxisEngine *engine);

extern void hkl_pseudo_axis_init(HklPseudoAxis *self,
				 HklParameter const *parameter,
				 HklPseudoAxisEngine *engine);

extern void hkl_pseudo_axis_free(HklPseudoAxis *self);

extern void hkl_pseudo_axis_fprintf(FILE *f, HklPseudoAxis *self);

/*****************************/
/* HklPseudoAxisEngineMode */
/*****************************/

extern HklPseudoAxisEngineMode *hkl_pseudo_axis_engine_mode_new(
	char const *name,
	HklPseudoAxisEngineInitFunc init,
	HklPseudoAxisEngineGetterFunc get,
	HklPseudoAxisEngineSetterFunc set,
	size_t n, ...);

extern int hkl_pseudo_axis_engine_mode_init(
	HklPseudoAxisEngineMode *self,
	char const *name,
	HklPseudoAxisEngineInitFunc init,
	HklPseudoAxisEngineGetterFunc get,
	HklPseudoAxisEngineSetterFunc set,
	size_t n_p, char const *parameters_names[],
	size_t n_axes, char const *axes_names[]);

extern void hkl_pseudo_axis_engine_mode_free(HklPseudoAxisEngineMode *self);

/***********************/
/* HklPseudoAxisEngine */
/***********************/

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_new(char const *name,
						       size_t n, ...);

extern void hkl_pseudo_axis_engine_free(HklPseudoAxisEngine *self);

extern void hkl_pseudo_axis_engine_add_get_set(HklPseudoAxisEngine *self,
					       HklPseudoAxisEngineMode *mode);

extern void hkl_pseudo_axis_engine_add_geometry(HklPseudoAxisEngine *self,
						double const x[]);

extern void hkl_pseudo_axis_engine_select_get_set(HklPseudoAxisEngine *self,
						  size_t idx);

extern void hkl_pseudo_axis_engine_prepare_internal(HklPseudoAxisEngine *engine,
						    HklGeometry *geometry,
						    HklDetector *detector,
						    HklSample *sample);


extern int hkl_pseudo_axis_engine_init(HklPseudoAxisEngine *self,
				       HklGeometry *geometry,
				       HklDetector *detector,
				       HklSample *sample);

extern int hkl_pseudo_axis_engine_setter(HklPseudoAxisEngine *self,
					 HklGeometry *geometry,
					 HklDetector *detector,
					 HklSample *sample);

extern int hkl_pseudo_axis_engine_getter(HklPseudoAxisEngine *self,
					 HklGeometry *geometry,
					 HklDetector *detector,
					 HklSample *sample);

extern void hkl_pseudo_axis_engine_fprintf(FILE *f, HklPseudoAxisEngine const *self);

/***************************/
/* HklPseudoAxisEngineList */
/***************************/

extern HklPseudoAxisEngineList *hkl_pseudo_axis_engine_list_new(void);

extern void hkl_pseudo_axis_engine_list_free(HklPseudoAxisEngineList *self);

extern int hkl_pseudo_axis_engine_list_add(HklPseudoAxisEngineList *self,
					   HklPseudoAxisEngine *engine);

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_list_get_by_name(HklPseudoAxisEngineList *self,
								    char const *name);

extern HklPseudoAxis *hkl_pseudo_axis_engine_list_get_pseudo_axis_by_name(HklPseudoAxisEngineList *self,
									  char const *name);

extern void hkl_pseudo_axis_engine_list_clear(HklPseudoAxisEngineList *self);

extern int hkl_pseudo_axis_engine_list_getter(HklPseudoAxisEngineList *self,
					      HklGeometry *geometry,
					      HklDetector *detector,
					      HklSample *sample);

extern void hkl_pseudo_axis_engine_list_fprintf(FILE *f,
						HklPseudoAxisEngineList const *self);
HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
