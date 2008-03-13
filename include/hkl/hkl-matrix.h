#ifndef __HKL_MATRIX_H__
#define __HKL_MATRIX_H__

#include <stdio.h>
#include <hkl/hkl-macros.h>
#include <hkl/hkl-vector.h>

HKL_BEGIN_DECLS

struct _HklMatrix
{
	double data[3][3];
};

extern HklMatrix *hkl_smatrix_new(double m11, double m12, double m13,
		double m21, double m22, double m23,
		double m31, double m32, double m33);
extern HklMatrix *hkl_smatrix_new_copy(HklMatrix const *m);

extern void hkl_smatrix_free(HklMatrix *m);

extern void hkl_smatrix_fprintf(FILE *file, HklMatrix const *m);

extern void hkl_smatrix_set(HklMatrix *m,
		double m11, double m12, double m13,
		double m21, double m22, double m23,
		double m31, double m32, double m33);

extern void hkl_smatrix_from_two_vector(HklMatrix *m,
		HklVector const *v1, HklVector const *v2);

extern void hkl_smatrix_from_euler(HklMatrix *m,
		double euler_x, double euler_y, double euler_z);

extern void hkl_smatrix_to_euler(HklMatrix const *m,
		double *euler_x, double *euler_y, double *euler_z);

extern int hkl_smatrix_cmp(HklMatrix const *m, HklMatrix const *m1);

extern void hkl_smatrix_times_smatrix(HklMatrix *m, HklMatrix const *m1);

extern void hkl_smatrix_times_vector(HklMatrix const *m, HklVector *v);

extern void hkl_smatrix_transpose(HklMatrix *m);

extern double hkl_smatrix_det(HklMatrix const *m);

extern int hkl_smatrix_solve(HklMatrix const *m,
		HklVector *x, HklVector const *b);

extern int hkl_smatrix_is_null(HklMatrix const *m);

HKL_END_DECLS

#endif
