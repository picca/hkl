#ifndef __HKL_SAMPLE_H__
#define __HKL_SAMPLE_H__

#include <hkl/hkl-lattice.h>
#include <hkl/hkl-geometry.h>
#include <hkl/hkl-detector.h>
#include <hkl/hkl-list.h>

HKL_BEGIN_DECLS

typedef enum _HklSampleType HklSampleType;
typedef struct _HklSample HklSample;
typedef struct _HklSampleReflection HklSampleReflection;

enum _HklSampleType {
	HKL_SAMPLE_MONOCRYSTAL
};

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
	HklDetector *detector;
	HklVector hkl;
	HklVector _hkl;
};

extern HklSample *hkl_sample_new(char const *name, HklSampleType type);
extern HklSample *hkl_sample_new_copy(HklSample const *sample);

extern void hkl_sample_free(HklSample *sample);

extern void hkl_sample_get_UB(HklSample const *sample, HklMatrix *matrix);

extern HklSampleReflection *hkl_sample_add_reflection(HklSample *sample,
		HklGeometry *g, HklDetector const *det,
		double h, double k, double l);

extern HklSampleReflection *hkl_sample_get_reflection(HklSample *sample,
		size_t idx);

extern int hkl_sample_del_reflection(HklSample *sample, size_t idx);

extern int hkl_sample_compute_UB_busing_levy(HklSample *sample,
		size_t idx1, size_t idx2);

extern void hkl_sample_affine(HklSample *sample);

HKL_END_DECLS

#endif
