#ifndef __HKL_SAMPLE_H__
#define __HKL_SAMPLE_H__

#include <hkl/hkl-lattice.h>
#include <hkl/hkl-geometry.h>
#include <hkl/hkl-detector.h>
#include <hkl/hkl-list.h>

HKL_BEGIN_DECLS

typedef struct _HklSample HklSample;
typedef struct _HklSampleReflection HklSampleReflection;
typedef struct _HklSampleList HklSampleList;

enum _HklSampleType {
	HKL_SAMPLE_MONOCRYSTAL
};

typedef enum _HklSampleType HklSampleType;

struct _HklSample {
	const char *name;
	HklSampleType type;
	HklLattice *lattice;
	HklMatrix U;
	HklMatrix UB;
	HklList *reflections;
};

struct _HklSampleReflection {
	HklGeometry *geometry;
	HklDetector detector;
	HklVector hkl;
	HklVector _hkl;
	int flag;
};

struct _HklSampleList {
	HklList *samples;
	HklSample *current;
};

/*************/
/* HklSample */
/*************/

extern HklSample *hkl_sample_new(char const *name, HklSampleType type);
extern HklSample *hkl_sample_new_copy(HklSample const *self);

extern void hkl_sample_free(HklSample *self);

extern int hkl_sample_set_lattice(HklSample *self,
				  double a, double b, double c,
				  double alpha, double beta, double gamma);

extern int hkl_sample_set_U_from_euler(HklSample *self,
				       double x, double y, double z);

extern void hkl_sample_get_UB(HklSample *self, HklMatrix *matrix);

extern HklSampleReflection *hkl_sample_add_reflection(HklSample *self,
						      HklGeometry *geometry,
						      HklDetector const *detector,
						      double h, double k, double l);

extern HklSampleReflection *hkl_sample_get_ith_reflection(HklSample *self,
							  size_t idx);

extern int hkl_sample_del_reflection(HklSample *self, size_t idx);

extern int hkl_sample_compute_UB_busing_levy(HklSample *self,
					     size_t idx1, size_t idx2);

extern void hkl_sample_affine(HklSample *sample);

extern double hkl_sample_get_reflection_mesured_angle(HklSample const *self,
						      size_t idx1, size_t idx2);

extern double hkl_sample_get_reflection_theoretical_angle(HklSample const *self,
							  size_t idx1, size_t idx2);

extern void hkl_sample_fprintf(FILE *f, HklSample const *self);


/*****************/
/* HklSampleList */
/*****************/

extern HklSampleList *hkl_sample_list_new(void);

extern void hkl_sample_list_free(HklSampleList *self);

extern HklSample *hkl_sample_list_append(HklSampleList *self,
					 char const *name,
					 HklSampleType type);

extern size_t hkl_sample_list_len(HklSampleList const *self);

extern HklSample *hkl_sample_list_get_ith(HklSampleList *self, size_t idx);

extern size_t hkl_sample_list_get_idx_from_name(HklSampleList *self,
						char const *name);

extern int hkl_sample_list_select_current(HklSampleList *self, char const *name);

HKL_END_DECLS

#endif
