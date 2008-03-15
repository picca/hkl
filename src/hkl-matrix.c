#include <stdlib.h>
#include <math.h>

#include <hkl/hkl-macros.h>
#include <hkl/hkl-matrix.h>
#include <hkl/hkl-vector.h>

HklMatrix *hkl_matrix_new( double m11, double m12, double m13,
		double m21, double m22, double m23,
		double m31, double m32, double m33)
{
	HklMatrix *m = NULL;
	m = malloc(sizeof(*m));
	if (!m)
		die("Can not allocate memory for an HklMatrix");
	hkl_matrix_set(m, m11, m12, m13, m21, m22, m23, m31, m32, m33);

	return m;
}

HklMatrix *hkl_matrix_new_copy(HklMatrix const *m)
{
	HklMatrix *copy = NULL;
	copy = malloc(sizeof(*copy));
	if (!copy)
		die("Can not allocate memory for an HklMatrix");
	*copy = *m;

	return copy;
}

void hkl_matrix_free(HklMatrix *m)
{
	free(m);
}

void hkl_matrix_fprintf(FILE *file, HklMatrix const *m)
{
	double const (*M)[3] = m->data;

	fprintf(file, "|%f, %f, %f|\n", M[0][0], M[0][1], M[0][2]);
	fprintf(file, "|%f, %f, %f|\n", M[1][0], M[1][1], M[1][2]);
	fprintf(file, "|%f, %f, %f|\n", M[2][0], M[2][1], M[2][2]);
}

void hkl_matrix_set(HklMatrix *m,
		double m11, double m12, double m13,
		double m21, double m22, double m23,
		double m31, double m32, double m33)
{
	double (*M)[3] = m->data;

	M[0][0] = m11;
	M[0][1] = m12;
	M[0][2] = m13;
	M[1][0] = m21;
	M[1][1] = m22;
	M[1][2] = m23;
	M[2][0] = m31;
	M[2][1] = m32;
	M[2][2] = m33;
}


void hkl_matrix_from_two_vector(HklMatrix *m,
		HklVector const *v1, HklVector const *v2)
{
	HklVector x, y, z;
	double (*M)[3] = m->data;

	x = *v1;
	hkl_vector_normalize(&x);

	z = *v1;
	hkl_vector_vectorial_product(&z, v2);
	hkl_vector_normalize(&z);

	y = z;
	hkl_vector_vectorial_product(&y, &x);

	M[0][0] = x.data[0], M[0][1] = y.data[0], M[0][2] = z.data[0];
	M[1][0] = x.data[1], M[1][1] = y.data[1], M[1][2] = z.data[1];
	M[2][0] = x.data[2], M[2][1] = y.data[2], M[2][2] = z.data[2];
}

void hkl_matrix_from_euler(HklMatrix *m,
		double euler_x, double euler_y, double euler_z)
{
	double (*M)[3] = m->data;

	double A = cos(euler_x);
	double B = sin(euler_x);
	double C = cos(euler_y);
	double D = sin(euler_y);
	double E = cos(euler_z);
	double F = sin(euler_z);
	double AD = A *D;
	double BD = B *D;

	M[0][0] = C*E;
	M[0][1] =-C*F;
	M[0][2] = D;
	M[1][0] = BD *E + A *F;
	M[1][1] =-BD *F + A *E;
	M[1][2] =-B *C;
	M[2][0] =-AD *E + B *F;
	M[2][1] = AD *F + B *E;
	M[2][2] = A *C;
}


void hkl_matrix_to_euler(HklMatrix const *m,
		double *euler_x, double *euler_y, double *euler_z)
{
	double tx, ty;
	double C;
	double const (*M)[3] = m->data;

	*euler_y = asin( m->data[0][2] );      /*Calculate Y-axis angle */
	C = cos( *euler_y );
	if (fabs(C) > HKL_EPSILON) {
		/*Gimball lock? */
		tx       =  M[2][2] / C; /*No, so get X-axis angle */
		ty       = -M[1][2] / C;
		*euler_x = atan2( ty, tx );
		tx       =  M[0][0] / C; /*Get Z-axis angle */
		ty       = -M[0][1] / C;
		*euler_z = atan2( ty, tx );
	} else {
		/*Gimball lock has occurred */
		*euler_x = 0.;              /*Set X-axis angle to zero */
		tx       =  M[1][1];    /*And calculate Z-axis angle */
		ty       =  M[1][0];
		*euler_z = atan2( ty, tx );
	}
}

int hkl_matrix_cmp(HklMatrix const *m, HklMatrix const *m1)
{
	unsigned int i;
	unsigned int j;
	for(i=0;i<3;i++)
		for(j=0;j<3;j++)
			if( fabs(m->data[i][j] - m1->data[i][j]) > HKL_EPSILON )
				return HKL_FALSE;
	return HKL_TRUE;
}


void hkl_matrix_times_smatrix(HklMatrix *m, HklMatrix const *m1)
{
	HklMatrix const tmp = *m;
	double (*M)[3] = m->data;
	double const (*Tmp)[3] = tmp.data;
	double const (*M1)[3];
	if (m == m1)
		M1 = tmp.data;
	else
		M1 = m1->data;

	M[0][0] = Tmp[0][0]*M1[0][0] + Tmp[0][1]*M1[1][0] + Tmp[0][2]*M1[2][0];
	M[0][1] = Tmp[0][0]*M1[0][1] + Tmp[0][1]*M1[1][1] + Tmp[0][2]*M1[2][1];
	M[0][2] = Tmp[0][0]*M1[0][2] + Tmp[0][1]*M1[1][2] + Tmp[0][2]*M1[2][2];

	M[1][0] = Tmp[1][0]*M1[0][0] + Tmp[1][1]*M1[1][0] + Tmp[1][2]*M1[2][0];
	M[1][1] = Tmp[1][0]*M1[0][1] + Tmp[1][1]*M1[1][1] + Tmp[1][2]*M1[2][1];
	M[1][2] = Tmp[1][0]*M1[0][2] + Tmp[1][1]*M1[1][2] + Tmp[1][2]*M1[2][2];

	M[2][0] = Tmp[2][0]*M1[0][0] + Tmp[2][1]*M1[1][0] + Tmp[2][2]*M1[2][0];
	M[2][1] = Tmp[2][0]*M1[0][1] + Tmp[2][1]*M1[1][1] + Tmp[2][2]*M1[2][1];
	M[2][2] = Tmp[2][0]*M1[0][2] + Tmp[2][1]*M1[1][2] + Tmp[2][2]*M1[2][2];
}


void hkl_matrix_times_vector(HklMatrix const *m, HklVector *v)
{
	HklVector tmp;
	double *Tmp = tmp.data;
	double *V = v->data;
	double const (*M)[3] = m->data;

	tmp = *v;

	V[0] = Tmp[0]*M[0][0] + Tmp[1]*M[0][1] + Tmp[2]*M[0][2];
	V[1] = Tmp[0]*M[1][0] + Tmp[1]*M[1][1] + Tmp[2]*M[1][2];
	V[2] = Tmp[0]*M[2][0] + Tmp[1]*M[2][1] + Tmp[2]*M[2][2];
}


void hkl_matrix_transpose(HklMatrix *m)
{
#define SWAP(a, b) {double tmp=a; a=b; b=tmp;}
	SWAP(m->data[1][0], m->data[0][1]);
	SWAP(m->data[2][0], m->data[0][2]);
	SWAP(m->data[2][1], m->data[1][2]);
}

/**@todo test */
double hkl_matrix_det(HklMatrix const *m)
{
	double det;
	double const (*M)[3] = m->data;

	det  =  M[0][0] * (M[1][1] * M[2][2] - M[2][1] * M[1][2]);
	det += -M[0][1] * (M[1][0] * M[2][2] - M[2][0] * M[1][2]);
	det +=  M[0][2] * (M[1][0] * M[2][1] - M[2][0] * M[1][1]);

	return det;
}

/** @todo test */
int hkl_matrix_solve(HklMatrix const *m, HklVector *x, HklVector const *b)
{
	double det;
	double const (*M)[3] = m->data;
	double *X = x->data;
	double const *B = b->data;

	det = hkl_matrix_det(m);
	if (fabs(det) < HKL_EPSILON)
		return -1;
	else {
		X[0] =   B[0] * (M[1][1]*M[2][2] - M[1][2]*M[2][1]);
		X[0] += -B[1] * (M[0][1]*M[2][2] - M[0][2]*M[2][1]);
		X[0] +=  B[2] * (M[0][1]*M[1][2] - M[0][2]*M[1][1]);

		X[1] =  -B[0] * (M[1][0]*M[2][2] - M[1][2]*M[2][0]);
		X[1] +=  B[1] * (M[0][0]*M[2][2] - M[0][2]*M[2][0]);
		X[1] += -B[2] * (M[0][0]*M[1][2] - M[0][2]*M[1][0]);

		X[2] =   B[0] * (M[1][0]*M[2][1] - M[1][1]*M[2][0]);
		X[2] += -B[1] * (M[0][0]*M[2][1] - M[0][1]*M[2][0]);
		X[2] +=  B[2] * (M[0][0]*M[1][1] - M[0][1]*M[1][0]);

		hkl_vector_div_double(x, det);
	}
	return 0;
}

/** @todo test */
int hkl_matrix_is_null(HklMatrix const *m)
{
	unsigned int i;
	unsigned int j;
	for (i=0;i<3;i++)
		for (j=0;j<3;j++)
			if ( fabs(m->data[i][j]) > HKL_EPSILON )
				return HKL_FALSE;
	return HKL_TRUE;
}
