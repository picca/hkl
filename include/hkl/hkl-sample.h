#ifndef __HKL_SAMPLE_H__
#define __HKL_SAMPLE_H__

HKL_BEGIN_DECLS

/* begin forward declaration */
#ifndef __TYPEDEF_HKL_GEOMETRY__
#define __TYPEDEF_HKL_GEOMETRY__
typedef struct _HklGeometry HklGeometry;
#endif

/* end forward declaration */

typedef enum _HklSampleType HklSampleType;
typedef struct _HklSample HklSample;

enum _HklSampleType {
	HKL_SAMPLE_MONOCRYSTAL
};

struct _HklSample {
	const char* name;
	HklSampleType type;
	HklGeometry *geometry;
	HklLattice *lattice;
	HklReflectionList *reflectionList;
};

extern void hkl_sample_new(HklSample *sample, HklGeometry *geometry, char const *name, HklSampleType type);

extern void hkl_sample_init(HklSample *sample, HklGeometry *geometry, char const *name, HklSampleType type);

extern void hkl_sample_release(HklSample *sample);

extern void hkl_sample_free(HklSample *sample);

extern void hkl_sample_copy(HklSample *src, HklSample *dst);

extern void hkl_sample_get_UB(HklSample *sample, HklMatrix *matrix);

extern void hkl_sample_affine(HklSample *sample);

HKL_END_DECLS

#endif
