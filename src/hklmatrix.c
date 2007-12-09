#include <math.h>

#include <hkl/hklmacros.h>
#include <hkl/hklmatrix.h>
#include <hkl/hklvector.h>

void hkl_smatrix_fprintf(FILE *file, HklMatrix const *m)
{
	fprintf(file, "|%f, %f, %f|\n", m->data[0][0], m->data[0][1], m->data[0][2]);
	fprintf(file, "|%f, %f, %f|\n", m->data[1][0], m->data[1][1], m->data[1][2]);
	fprintf(file, "|%f, %f, %f|\n", m->data[2][0], m->data[2][1], m->data[2][2]);
}

void hkl_smatrix_from_two_svector(HklMatrix *m, HklVector const *v1, HklVector const *v2)
{
	HklVector x, y, z;

	x = *v1;
	hkl_svector_normalize(&x);

	z = *v1;
	hkl_svector_vectorial_product(&z, v2);
	hkl_svector_normalize(&z);

	y = z;
	hkl_svector_vectorial_product(&y, &x);

	m->data[0][0] = x.data[0], m->data[0][1] = y.data[0], m->data[0][2] = z.data[0];
	m->data[1][0] = x.data[1], m->data[1][1] = y.data[1], m->data[1][2] = z.data[1];
	m->data[2][0] = x.data[2], m->data[2][1] = y.data[2], m->data[2][2] = z.data[2];
}

void hkl_smatrix_from_euler(HklMatrix *m, double euler_x, double euler_y, double euler_z)
{
	double A = cos(euler_x);
	double B = sin(euler_x);
	double C = cos(euler_y);
	double D = sin(euler_y);
	double E = cos(euler_z);
	double F = sin(euler_z);
	double AD = A *D;
	double BD = B *D;

	m->data[0][0] = C*E;
	m->data[0][1] =-C*F;
	m->data[0][2] = D;
	m->data[1][0] = BD *E + A *F;
	m->data[1][1] =-BD *F + A *E;
	m->data[1][2] =-B *C;
	m->data[2][0] =-AD *E + B *F;
	m->data[2][1] = AD *F + B *E;
	m->data[2][2] = A *C;
}


void hkl_smatrix_to_euler(HklMatrix const *m, double *euler_x, double *euler_y, double *euler_z)
{
	double tx, ty;
	double C;

	*euler_y = asin( m->data[0][2] );        /*Calculate Y-axis angle */
	C = cos( *euler_y );
	if (fabs(C) > HKL_EPSILON) {
		/*Gimball lock? */
		tx =  m->data[2][2] / C;           /*No, so get X-axis angle */
		ty = -m->data[1][2] / C;
		*euler_x = atan2( ty, tx );
		tx =  m->data[0][0] / C;            /*Get Z-axis angle */
		ty = -m->data[0][1] / C;
		*euler_z = atan2( ty, tx );
	} else {
		/*Gimball lock has occurred */
		*euler_x  = 0.;              /*Set X-axis angle to zero */
		tx      =  m->data[1][1];         /*And calculate Z-axis angle */
		ty      =  m->data[1][0];
		*euler_z  = atan2( ty, tx );
	}
}

int hkl_smatrix_cmp(HklMatrix const *m, HklMatrix const *m1)
{
	unsigned int i;
	unsigned int j;
	for (i=0;i<3;i++)
		for (j=0;j<3;j++)
			if ( fabs(m->data[i][j] - m1->data[i][j]) > HKL_EPSILON )
				return HKL_FALSE;
	return HKL_TRUE;
}


void hkl_smatrix_times_smatrix(HklMatrix *m, HklMatrix const *m1)
{
	HklMatrix M;
	HklMatrix const *M1;
	M = *m;
	if (m == m1)
		M1 = &M;
	else
		M1 = m1;
	m->data[0][0] = M.data[0][0] *M1->data[0][0] + M.data[0][1] *M1->data[1][0] + M.data[0][2] *M1->data[2][0];
	m->data[0][1] = M.data[0][0] *M1->data[0][1] + M.data[0][1] *M1->data[1][1] + M.data[0][2] *M1->data[2][1];
	m->data[0][2] = M.data[0][0] *M1->data[0][2] + M.data[0][1] *M1->data[1][2] + M.data[0][2] *M1->data[2][2];
	m->data[1][0] = M.data[1][0] *M1->data[0][0] + M.data[1][1] *M1->data[1][0] + M.data[1][2] *M1->data[2][0];
	m->data[1][1] = M.data[1][0] *M1->data[0][1] + M.data[1][1] *M1->data[1][1] + M.data[1][2] *M1->data[2][1];
	m->data[1][2] = M.data[1][0] *M1->data[0][2] + M.data[1][1] *M1->data[1][2] + M.data[1][2] *M1->data[2][2];
	m->data[2][0] = M.data[2][0] *M1->data[0][0] + M.data[2][1] *M1->data[1][0] + M.data[2][2] *M1->data[2][0];
	m->data[2][1] = M.data[2][0] *M1->data[0][1] + M.data[2][1] *M1->data[1][1] + M.data[2][2] *M1->data[2][1];
	m->data[2][2] = M.data[2][0] *M1->data[0][2] + M.data[2][1] *M1->data[1][2] + M.data[2][2] *M1->data[2][2];
}


void hkl_smatrix_times_svector(HklMatrix const *m, HklVector *v)
{
	HklVector tmp;
	tmp = *v;

	v->data[0] = tmp.data[0] *m->data[0][0] + tmp.data[1] *m->data[0][1] + tmp.data[2] *m->data[0][2];
	v->data[1] = tmp.data[0] *m->data[1][0] + tmp.data[1] *m->data[1][1] + tmp.data[2] *m->data[1][2];
	v->data[2] = tmp.data[0] *m->data[2][0] + tmp.data[1] *m->data[2][1] + tmp.data[2] *m->data[2][2];
}


void hkl_smatrix_transpose(HklMatrix *m)
{
#define SWAP(a, b) {double tmp=a; a=b; b=tmp;}
	SWAP(m->data[1][0], m->data[0][1]);
	SWAP(m->data[2][0], m->data[0][2]);
	SWAP(m->data[2][1], m->data[1][2]);
}

/**@todo test */
double hkl_smatrix_det(HklMatrix const *m)
{
	double det;

	det  =  m->data[0][0] *(m->data[1][1] *m->data[2][2] - m->data[2][1] *m->data[1][2]);
	det += -m->data[0][1] *(m->data[1][0] *m->data[2][2] - m->data[2][0] *m->data[1][2]);
	det +=  m->data[0][2] *(m->data[1][0] *m->data[2][1] - m->data[2][0] *m->data[1][1]);

	return det;
}

/** @todo test */
int hkl_smatrix_solve(HklMatrix const *m, HklVector *x, HklVector const *b)
{
	double det;

	det = hkl_smatrix_det(m);
	if (fabs(det) < HKL_EPSILON)
		return -1;
	else {
		x->data[0] =   b->data[0] *(m->data[1][1] *m->data[2][2] - m->data[1][2] *m->data[2][1]);
		x->data[0] += -b->data[1] *(m->data[0][1] *m->data[2][2] - m->data[0][2] *m->data[2][1]);
		x->data[0] +=  b->data[2] *(m->data[0][1] *m->data[1][2] - m->data[0][2] *m->data[1][1]);

		x->data[1] =  -b->data[0] *(m->data[1][0] *m->data[2][2] - m->data[1][2] *m->data[2][0]);
		x->data[1] +=  b->data[1] *(m->data[0][0] *m->data[2][2] - m->data[0][2] *m->data[2][0]);
		x->data[1] += -b->data[2] *(m->data[0][0] *m->data[1][2] - m->data[0][2] *m->data[1][0]);

		x->data[2] =   b->data[0] *(m->data[1][0] *m->data[2][1] - m->data[1][1] *m->data[2][0]);
		x->data[2] += -b->data[1] *(m->data[0][0] *m->data[2][1] - m->data[0][1] *m->data[2][0]);
		x->data[2] +=  b->data[2] *(m->data[0][0] *m->data[1][1] - m->data[0][1] *m->data[1][0]);

		hkl_svector_div_double(x, det);
	}
	return 0;
}

/** @todo test */
int hkl_smatrix_is_null(HklMatrix const *m)
{
	unsigned int i;
	unsigned int j;
	for (i=0;i<3;i++)
		for (j=0;j<3;j++)
			if ( fabs(m->data[i][j]) > HKL_EPSILON )
				return HKL_FALSE;
	return HKL_TRUE;
}
