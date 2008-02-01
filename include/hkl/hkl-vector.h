#ifndef __HKL_VECTOR_H__
#define __HKL_VECTOR_H__

#include <stdio.h>
#include <hkl/hkl-macros.h>

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

extern HklVector *hkl_vector_new(double x, double y, double z);
extern HklVector *hkl_vector_new_copy(HklVector const *v);

void hkl_vector_free(HklVector *v);

extern void hkl_vector_fprintf(FILE *file, HklVector const *v);

extern void hkl_vector_set(HklVector *v, double const x, double const y, double const z);

extern int hkl_vector_cmp(HklVector const *v, HklVector const *v1);

extern int hkl_vector_is_opposite(HklVector const *v, HklVector const *v1);

extern void hkl_vector_minus_vector(HklVector *v, HklVector const *v1);

extern void hkl_vector_div_double(HklVector *v, double const d);

extern void hkl_vector_times_double(HklVector *v, double const d);

extern void hkl_vector_times_vector(HklVector *v, HklVector const *v1);

extern void hkl_vector_times_smatrix(HklVector *v, HklMatrix const *m);

extern double hkl_vector_sum(HklVector const *v);

extern double hkl_vector_scalar_product(HklVector const *v, HklVector const *v1);

extern void hkl_vector_vectorial_product(HklVector *v, HklVector const *v1);

extern double hkl_vector_angle(HklVector const *v, HklVector const *v1);

extern double hkl_vector_norm2(HklVector const *v);

extern int hkl_vector_normalize(HklVector *v);

extern int hkl_vector_is_colinear(HklVector const *v, HklVector const *v1);

extern void hkl_vector_randomize(HklVector *v);
extern void hkl_vector_randomize_vector(HklVector *v, HklVector const *v1);
extern void hkl_vector_randomize_vector_vector(HklVector *v, HklVector const *v1, HklVector const *v2);

extern void hkl_vector_rotated_around_vector(HklVector *v, HklVector const *axe, double angle);
extern void hkl_vector_rotated_quaternion(HklVector *v, HklQuaternion const *q);

extern int hkl_vector_is_null(HklVector const *v);

HKL_END_DECLS

#endif
