#ifndef __HKL_VECTOR_H__
#define __HKL_VECTOR_H__

#include <stdio.h>
#include <hkl/hklmacros.h>

HKL_BEGIN_DECLS

/* forward declaration begin */
typedef struct _HklMatrix HklMatrix;
typedef struct _HklQuaternion HklQuaternion;
/* forward declaration end */

typedef struct _HklVector HklVector;

struct _HklVector
{
	double data[3];
};

extern void hkl_svector_fprintf(FILE *file, HklVector const *v);

extern void hkl_svector_set(HklVector *v, double const x, double const y, double const z);

extern int hkl_svector_cmp(HklVector const *v, HklVector const *v1);

extern int hkl_svector_is_opposite(HklVector const *v, HklVector const *v1);

extern void hkl_svector_minus_svector(HklVector *v, HklVector const *v1);

extern void hkl_svector_div_double(HklVector *v, double const d);

extern void hkl_svector_times_double(HklVector *v, double const d);

extern void hkl_svector_times_svector(HklVector *v, HklVector const *v1);

extern void hkl_svector_times_smatrix(HklVector *v, HklMatrix const *m);

extern double hkl_svector_sum(HklVector const *v);

extern double hkl_svector_scalar_product(HklVector const *v, HklVector const *v1);

extern void hkl_svector_vectorial_product(HklVector *v, HklVector const *v1);

extern double hkl_svector_angle(HklVector const *v, HklVector const *v1);

extern double hkl_svector_norm2(HklVector const *v);

extern int hkl_svector_normalize(HklVector *v);

extern int hkl_svector_is_colinear(HklVector const *v, HklVector const *v1);

extern void hkl_svector_randomize(HklVector *v);
extern void hkl_svector_randomize_svector(HklVector *v, HklVector const *v1);
extern void hkl_svector_randomize_svector_svector(HklVector *v, HklVector const *v1, HklVector const *v2);

extern void hkl_svector_rotated_around_vector(HklVector *v, HklVector const *axe, double angle);
extern void hkl_svector_rotated_quaternion(HklVector *v, HklQuaternion const *q);

extern int hkl_svector_is_null(HklVector const *v);

HKL_END_DECLS

#endif
