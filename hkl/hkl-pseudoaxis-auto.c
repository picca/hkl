/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <alloca.h>                     // for alloca
#include <gsl/gsl_errno.h>              // for ::GSL_CONTINUE
#include <gsl/gsl_machine.h>            // for GSL_SQRT_DBL_EPSILON
#include <gsl/gsl_matrix_double.h>      // for gsl_matrix_alloc, etc
#include <gsl/gsl_multiroots.h>         // for gsl_multiroot_function, etc
#include <gsl/gsl_vector_double.h>      // for gsl_vector, etc
#include <math.h>                       // for fabs, M_PI
#include <stddef.h>                     // for size_t
#include <stdlib.h>                     // for rand, RAND_MAX
#include <string.h>                     // for NULL, memset, memcpy
#include <sys/types.h>                  // for uint
#include "hkl-geometry-private.h"       // for hkl_geometry_update
#include "hkl-macros-private.h"         // for HKL_MALLOC, hkl_assert, etc
#include "hkl-parameter-private.h"      // for _HklParameter
#include "hkl-pseudoaxis-auto-private.h"  // for HklModeAutoInfo, etc
#include "hkl-pseudoaxis-private.h"     // for _HklEngine, HklModeInfo, etc
#include "hkl.h"                        // for HklEngine, HklMode, etc
#include "hkl/ccan/container_of/container_of.h"  // for container_of
#include "hkl/ccan/darray/darray.h"     // for darray_foreach

/* #define DEBUG */

#define HKL_MODE_AUTO_ERROR hkl_mode_auto_error_quark ()

static GQuark hkl_mode_auto_error_quark (void)
{
	return g_quark_from_static_string ("hkl-mode-auto-error-quark");
}

typedef enum {
	HKL_MODE_AUTO_ERROR_SET, /* can not set the engine */
} HklModeAutoError;

/*********************************************/
/* methods use to solve numerical pseudoAxes */
/*********************************************/

/**
 * @brief This private method find the degenerated axes.
 *
 * @param func the gsl_multiroopt_function to test
 * @param x the starting point
 * @param f the result of the function evaluation.
 *
 * with this method we can see if an axis is degenerated or not.
 * A degenerated axis is an axis with no effect on the function evaluation.
 * In the Jacobian matrix all elements of a columnn is null.
 * Once we know this the axis is mark as degenerated and we do not need to
 * change is sector.
 */
static void find_degenerated_axes(HklEngine *self,
				  gsl_multiroot_function *func,
				  gsl_vector const *x, gsl_vector const *f,
				  int degenerated[])
{
	gsl_matrix *J;
	size_t i, j;

	memset(degenerated, 0, x->size * sizeof(int));
	J = gsl_matrix_alloc(x->size, f->size);

	gsl_multiroot_fdjacobian(func, x, f, GSL_SQRT_DBL_EPSILON, J);
	for(j=0; j<x->size && !degenerated[j]; ++j) {
		for(i=0; i<f->size; ++i)
			if (fabs(gsl_matrix_get(J, i, j)) > HKL_EPSILON)
				break;
		if (i == f->size)
			degenerated[j] = 1;
	}

#ifdef DEBUG
	fprintf(stdout, "\nLooks for degenerated axes\n");
	for(i=0; i<x->size; ++i)
		fprintf(stdout, " %d", degenerated[i]);
	for(i=0;i<x->size;++i) {
		fprintf(stdout, "\n   ");
		for(j=0;j<f->size;++j)
			fprintf(stdout, " %f", gsl_matrix_get(J, i, j));
	}
	fprintf(stdout, "\n");
#endif
	gsl_matrix_free(J);
}

/**
 * @brief this private method try to find the first solution
 *
 * @param self the current HklPseudoAxeEngine.
 * @param f The function to use for the computation.
 *
 * If a solution was found it also check for degenerated axes.
 * A degenerated axes is an Axes with no effect on the function.
 * @see find_degenerated
 * @return TRUE or FALSE.
 */
static int find_first_geometry(HklEngine *self,
			       gsl_multiroot_function *f,
			       int degenerated[])
{
	gsl_multiroot_fsolver_type const *T;
	gsl_multiroot_fsolver *s;
	gsl_vector *x;
	size_t len = darray_size(self->mode->info->axes_w);
	double *x_data;
	double *x_data0 = alloca(len * sizeof(*x_data0));
	size_t iter = 0;
	int status;
	int res = FALSE;
	size_t i;
	HklParameter **axis;

	/* get the starting point from the geometry */
	/* must be put in the auto_set method */
	x = gsl_vector_alloc(len);
	x_data = (double *)x->data;
	i = 0;
	darray_foreach(axis, self->axes){
		x_data[i++] = (*axis)->_value;
	}

	/* keep a copy of the first axes positions to deal with degenerated axes */
	memcpy(x_data0, x_data, len * sizeof(double));

	/* Initialize method  */
	T = gsl_multiroot_fsolver_hybrid;
	s = gsl_multiroot_fsolver_alloc (T, len);
	gsl_multiroot_fsolver_set (s, f, x);

	/* iterate to find the solution */
	do {
		++iter;
		status = gsl_multiroot_fsolver_iterate(s);
		if (status || iter % 300 == 0) {
			/* Restart from another point. */
			for(i=0; i<len; ++i)
				x_data[i] = (double)rand() / RAND_MAX * 180. / M_PI;
			gsl_multiroot_fsolver_set(s, f, x);
			gsl_multiroot_fsolver_iterate(s);
		}
		status = gsl_multiroot_test_residual (s->f, HKL_EPSILON / 10.);
	} while (status == GSL_CONTINUE && iter < 2000);

#ifdef DEBUG
	fprintf(stdout, "\nstatus : %d iter : %d", status, iter);
	for(i=0; i<len; ++i)
		fprintf(stdout, " %.7f", s->f->data[i]);
	fprintf(stdout, "\n");
#endif

	if (status != GSL_CONTINUE) {
		find_degenerated_axes(self, f, s->x, s->f, degenerated);

#ifdef DEBUG
		/* print the test header */
		fprintf(stdout, "\n");
		for(i=0; i<len; ++i)
			fprintf(stdout, "\t f(%d)", i);
		darray_foreach(axis, self->axes){
			fprintf(stdout, "\t \"%s\"", (*axis)->name);
		}
#endif
		/* set the geometry from the gsl_vector */
		/* in a futur version the geometry must contain a gsl_vector */
		/* to avoid this. */
		x_data = (double *)s->x->data;
		i = 0;
		darray_foreach(axis, self->axes){
			hkl_parameter_value_set(*axis,
						degenerated[i] ? x_data0[i] : x_data[i],
						HKL_UNIT_DEFAULT,
						NULL);
			++i;
		}

		hkl_geometry_update(self->geometry);
		res = TRUE;
	}

	/* release memory */
	gsl_vector_free(x);
	gsl_multiroot_fsolver_free(s);

	return res;
}

/**
 * @brief This private method change the sector of angles.
 *
 * @param x The vector of changed angles.
 * @param x0 The vector of angles to change.
 * @param sector the sector vector operation.
 * @param n the size of all vectors.
 *
 * 0 -> no change
 * 1 -> pi - angle
 * 2 -> pi + angle
 * 3 -> -angle
 */
static void change_sector(double x[], double const x0[],
			  int const sector[], size_t n)
{
	size_t i;

	for(i=0; i<n; ++i) {
		switch (sector[i]) {
		case 0:
			x[i] = x0[i];
			break;
		case 1:
			x[i] = M_PI - x0[i];
			break;
		case 2:
			x[i] = M_PI + x0[i];
			break;
		case 3:
			x[i] = -x0[i];
			break;
		}
	}
}

/**
 * @brief Test if an angle combination is compatible with q function.
 *
 * @param x The vector of angles to test.
 * @param function The gsl_multiroot_function used for the test.
 * @param f a gsl_vector use to compute the result (optimization)
 */
static int test_sector(gsl_vector const *x,
		       gsl_multiroot_function *function,
		       gsl_vector *f)
{
	int res = TRUE;
	size_t i;
	double *f_data = f->data;

	function->f(x, function->params, f);

	for(i=0; i<f->size; ++i)
		if (fabs(f_data[i]) > HKL_EPSILON){
			res = FALSE;
			break;
		}

#ifdef DEBUG
	fprintf(stdout, "\n");
	for(i=0; i<f->size; ++i)
		if(fabs(f_data[i]) < HKL_EPSILON)
			fprintf(stdout, "\t%f *", f_data[i]);
		else
			fprintf(stdout, "\t%f", f_data[i]);
	for(i=0; i<f->size; ++i)
		fprintf(stdout, "\t%f", gsl_sf_angle_restrict_symm(x->data[i]) * HKL_RADTODEG);

	if(res == FALSE)
		fprintf(stdout, "\t FAIL");
	else
		fprintf(stdout, "\t SUCCESS");
#endif

	return res;
}

/**
 * @brief compute the permutation and test its validity.
 *
 * @param axes_len number of axes
 * @param op_len number of operation per axes. (4 for now)
 * @param p The vector containing the current permutation.
 * @param axes_idx The index of the axes we are permution.
 * @param op the current operation to set.
 * @param f The function for the validity test.
 * @param x0 The starting point of all geometry permutations.
 * @param _x a gsl_vector use to compute the sectors (optimization)
 * @param _f a gsl_vector use during the sector test (optimization)
 */
static void perm_r(size_t axes_len, size_t op_len[], int p[], size_t axes_idx,
		   int op, gsl_multiroot_function *f, double x0[],
		   gsl_vector *_x, gsl_vector *_f)
{
	size_t i;

	p[axes_idx++] = op;
	if (axes_idx == axes_len) {
		double *x_data = _x->data;
		change_sector(x_data, x0, p, axes_len);
		if (test_sector(_x, f, _f))
			hkl_engine_add_geometry(f->params, x_data);
	} else
		for (i=0; i<op_len[axes_idx]; ++i)
			perm_r(axes_len, op_len, p, axes_idx, i, f, x0, _x, _f);
}

/**
 * @brief Find all numerical solutions of a mode.
 *
 * @param self the current HklEngine
 * @param function The mode function
 *
 * @return TRUE or FALSE
 *
 * This method find a first solution with a numerical method from the
 * GSL library (the multi root solver hybrid). Then it multiplicates the
 * solutions from this starting point using cosinus/sinus properties.
 * It addes all valid solutions to the self->geometries.
 */
static int solve_function(HklEngine *self,
			  const HklFunction *function)
{

	size_t i;
	int p[function->size];
	double x0[function->size];
	int degenerated[function->size];
	size_t op_len[function->size];
	int res;
	gsl_vector *_x; /* use to compute sectors in perm_r (avoid copy) */
	gsl_vector *_f; /* use to test sectors in perm_r (avoid copy) */
	gsl_multiroot_function f;
	HklParameter **axis;

	_x = gsl_vector_alloc(function->size);
	_f = gsl_vector_alloc(function->size);

	f.f = function->function;
	f.n = function->size;
	f.params = self;

	res = find_first_geometry(self, &f, degenerated);
	if (res) {
		memset(p, 0, sizeof(p));
		/* use first solution as starting point for permutations */
		i = 0;
		darray_foreach(axis, self->axes){
			x0[i] = (*axis)->_value;
			op_len[i] = degenerated[i] ? 1 : 4;
			++i;
		}
		for (i=0; i<op_len[0]; ++i)
			perm_r(function->size, op_len, p, 0, i, &f, x0, _x, _f);
	}

	gsl_vector_free(_f);
	gsl_vector_free(_x);
	return res;
}

/* check that the number of axis of the mode is the right number of variables expected by mode functions */
static inline void check_validity(const HklModeAutoInfo *auto_info)
{
	const HklFunction **function;
	darray_foreach(function, auto_info->functions)
		hkl_assert((*function)->size == darray_size(auto_info->info.axes_w));
}

HklMode *hkl_mode_auto_new(const HklModeAutoInfo *auto_info,
			   const HklModeOperations *ops,
			   int initialized)
{
	check_validity(auto_info);

	return hkl_mode_new(&auto_info->info, ops, initialized);

}

void hkl_mode_auto_init(HklMode *self,
			const HklModeAutoInfo *auto_info,
			const HklModeOperations *ops,
			int initialized)
{
	check_validity(auto_info);

	hkl_mode_init(self, &auto_info->info, ops, initialized);

}

int hkl_mode_auto_set_real(HklMode *self,
			   HklEngine *engine,
			   HklGeometry *geometry,
			   HklDetector *detector,
			   HklSample *sample,
			   GError **error)
{
	int ok = FALSE;
	HklModeAutoInfo *auto_info = container_of(self->info, HklModeAutoInfo, info);
	const HklFunction **function;

	g_return_val_if_fail (error == NULL || *error == NULL, FALSE);

	if(!self || !engine || !geometry || !detector || !sample){
		g_set_error(error,
			    HKL_MODE_AUTO_ERROR,
			    HKL_MODE_AUTO_ERROR_SET,
			    "Internal error");
		return FALSE;
	}

	darray_foreach(function, auto_info->functions)
		ok |= solve_function(engine, *function);

	if(!ok){
		g_set_error(error,
			    HKL_MODE_AUTO_ERROR,
			    HKL_MODE_AUTO_ERROR_SET,
			    "none of the functions were solved !!!");
		return FALSE;
	}

#ifdef DEBUG
	hkl_engine_fprintf(stdout, engine);
#endif

	return TRUE;
}

HklMode *hkl_mode_auto_with_init_new(const HklModeAutoInfo *auto_info,
				     const HklModeOperations *ops,
				     int initialized)
{
	HklModeAutoWithInit *self = HKL_MALLOC(HklModeAutoWithInit);

	hkl_mode_auto_init(&self->mode, auto_info, ops, initialized);

	self->geometry = NULL;
	self->detector = NULL;
	self->sample = NULL;

	return &self->mode;
}
