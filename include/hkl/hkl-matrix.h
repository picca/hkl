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

extern void hkl_matrix_init(HklMatrix *self,
			    double m11, double m12, double m13,
			    double m21, double m22, double m23,
			    double m31, double m32, double m33);

extern void hkl_matrix_fprintf(FILE *file, HklMatrix const *self);

extern void hkl_matrix_from_two_vector(HklMatrix *self,
				       HklVector const *v1, HklVector const *v2);

extern void hkl_matrix_from_euler(HklMatrix *self,
				  double euler_x, double euler_y, double euler_z);

extern void hkl_matrix_to_euler(HklMatrix const *self,
				double *euler_x, double *euler_y, double *euler_z);

extern int hkl_matrix_cmp(HklMatrix const *self, HklMatrix const *self1);

extern void hkl_matrix_times_smatrix(HklMatrix *self, HklMatrix const *self1);

extern void hkl_matrix_times_vector(HklMatrix const *self, HklVector *v);

extern void hkl_matrix_transpose(HklMatrix *self);

extern double hkl_matrix_det(HklMatrix const *self);

extern int hkl_matrix_solve(HklMatrix const *self,
			    HklVector *x, HklVector const *b);

extern int hkl_matrix_is_null(HklMatrix const *self);

HKL_END_DECLS

#endif
