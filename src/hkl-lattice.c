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
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdlib.h>
#include <math.h>

#include "hkl/hkl-lattice.h"
#include "hkl/hkl-unit.h"

/* private */

static int check_lattice_param(double a, double b, double c,
			       double alpha, double beta, double gamma)
{
	double D = 1. - cos(alpha)*cos(alpha) - cos(beta)*cos(beta)
		- cos(gamma)*cos(gamma) + 2. * cos(alpha)*cos(beta)*cos(gamma);

	if (D < 0.)
		return HKL_FAIL;
	else
		return HKL_SUCCESS;
}

/* public */

HklLattice *hkl_lattice_new(double a, double b, double c,
			    double alpha, double beta, double gamma)
{
	HklLattice *self = NULL;
	if(!check_lattice_param(a, b, c, alpha, beta, gamma)) {
		self = malloc(sizeof(*self));
		if (!self)
			die("Can not allocate memory for an HklLattice");

		self->a = hkl_parameter_new("a", 0, a, a+10,
					    HKL_FALSE, HKL_TRUE,
					    &hkl_unit_length_nm,
					    &hkl_unit_length_nm);
		self->b = hkl_parameter_new("b", 0, b, b+10,
					    HKL_FALSE, HKL_TRUE,
					    &hkl_unit_length_nm,
					    &hkl_unit_length_nm);
		self->c = hkl_parameter_new("c", 0, c, c+10,
					    HKL_FALSE, HKL_TRUE,
					    &hkl_unit_length_nm,
					    &hkl_unit_length_nm);
		self->alpha = hkl_parameter_new("alpha", -M_PI, alpha, M_PI,
						HKL_FALSE, HKL_TRUE,
						&hkl_unit_angle_rad,
						&hkl_unit_angle_deg);
		self->beta = hkl_parameter_new("beta", -M_PI, beta, M_PI,
					       HKL_FALSE, HKL_TRUE,
					       &hkl_unit_angle_rad,
					       &hkl_unit_angle_deg);
		self->gamma = hkl_parameter_new("gamma", -M_PI, gamma, M_PI,
						HKL_FALSE, HKL_TRUE,
						&hkl_unit_angle_rad,
						&hkl_unit_angle_deg);
	}

	return self;	
}

HklLattice *hkl_lattice_new_copy(HklLattice const *self)
{
	HklLattice *copy = NULL;

	copy = malloc(sizeof(*copy));
	if (!copy)
		die("Can not allocate memory for an HklLattice");
	
	copy->a = hkl_parameter_new_copy(self->a);
	copy->b = hkl_parameter_new_copy(self->b);
	copy->c = hkl_parameter_new_copy(self->c);
	copy->alpha = hkl_parameter_new_copy(self->alpha);
	copy->beta = hkl_parameter_new_copy(self->beta);
	copy->gamma = hkl_parameter_new_copy(self->gamma);

	return copy;	
}

HklLattice* hkl_lattice_new_default(void)
{
	return hkl_lattice_new(1.54, 1.54, 1.54,
			       90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
}

void hkl_lattice_free(HklLattice *self)
{
	hkl_parameter_free(self->a);
	hkl_parameter_free(self->b);
	hkl_parameter_free(self->c);
	hkl_parameter_free(self->alpha);
	hkl_parameter_free(self->beta);
	hkl_parameter_free(self->gamma);
	free(self);
}

int hkl_lattice_set(HklLattice *self,
		    double a, double b, double c,
		    double alpha, double beta, double gamma)
{
	int res = HKL_FAIL;

	if(!check_lattice_param(a, b, c, alpha, beta, gamma)) {
		self->a->value = a;
		self->b->value = b;
		self->c->value = c;
		self->alpha->value = alpha;
		self->beta->value = beta;
		self->gamma->value = gamma;
		res = HKL_SUCCESS; 
	}
	return res;
}

/* 
 * Get the B matrix from the l parameters 
 */
int hkl_lattice_get_B(HklLattice const *self, HklMatrix *B)
{
	double D;
	double c_alpha, s_alpha;
	double c_beta, s_beta;
	double c_gamma, s_gamma;
	double b11, b22, tmp;

	c_alpha = cos(self->alpha->value);
	c_beta = cos(self->beta->value);
	c_gamma = cos(self->gamma->value);
	D = 1 - c_alpha*c_alpha - c_beta*c_beta - c_gamma*c_gamma
		+ 2*c_alpha*c_beta*c_gamma;

	if (D > 0.)
		D = sqrt(D);
	else
		return HKL_FAIL;

	s_alpha = sin(self->alpha->value);
	s_beta  = sin(self->beta->value);
	s_gamma = sin(self->gamma->value);

	b11 = HKL_TAU / (self->b->value * s_alpha);
	b22 = HKL_TAU / self->c->value;
	tmp = b22 / s_alpha;

	B->data[0][0] = HKL_TAU * s_alpha / (self->a->value * D);
	B->data[0][1] = b11 / D * (c_alpha*c_beta - c_gamma);
	B->data[0][2] = tmp / D * (c_gamma*c_alpha - c_beta);

	B->data[1][0] = 0;
	B->data[1][1] = b11;
	B->data[1][2] = tmp / (s_beta*s_gamma) * (c_beta*c_gamma - c_alpha);

	B->data[2][0] = 0;
	B->data[2][1] = 0;
	B->data[2][2] = b22;

	return HKL_SUCCESS;
}

int hkl_lattice_reciprocal(HklLattice const *self, HklLattice *reciprocal)
{
	double c_alpha, c_beta, c_gamma;
	double s_alpha, s_beta, s_gamma;
	double c_beta1, c_beta2, c_beta3;
	double s_beta1, s_beta2, s_beta3;
	double s_beta_s_gamma, s_gamma_s_alpha, s_alpha_s_beta;
	double D;

	c_alpha = cos(self->alpha->value);
	c_beta  = cos(self->beta->value);
	c_gamma = cos(self->gamma->value);
	D = 1 - c_alpha*c_alpha - c_beta*c_beta - c_gamma*c_gamma
		+ 2*c_alpha*c_beta*c_gamma;

	if (D > 0.)
		D = sqrt(D);
	else
		return HKL_FAIL;

	s_alpha = sin(self->alpha->value);
	s_beta  = sin(self->beta->value);
	s_gamma = sin(self->gamma->value);

	s_beta_s_gamma  = s_beta  * s_gamma;
	s_gamma_s_alpha = s_gamma * s_alpha;
	s_alpha_s_beta  = s_alpha * s_beta;

	c_beta1 = (c_beta  * c_gamma - c_alpha) / s_beta_s_gamma;
	c_beta2 = (c_gamma * c_alpha - c_beta)  / s_gamma_s_alpha;
	c_beta3 = (c_alpha * c_beta  - c_gamma) / s_alpha_s_beta;
	s_beta1 = D / s_beta_s_gamma;
	s_beta2 = D / s_gamma_s_alpha;
	s_beta3 = D / s_alpha_s_beta;

	reciprocal->a->value = HKL_TAU * s_alpha / (self->a->value * D);
	reciprocal->b->value = HKL_TAU * s_beta  / (self->b->value * D);
	reciprocal->c->value = HKL_TAU * s_gamma / (self->c->value * D);
	reciprocal->alpha->value = atan2(s_beta1, c_beta1);
	reciprocal->beta->value  = atan2(s_beta2, c_beta2);
	reciprocal->gamma->value = atan2(s_beta3, c_beta3);

	return HKL_SUCCESS;
}

void hkl_lattice_randomize(HklLattice *self)
{
	static HklVector vector_x = {{1, 0, 0}};
	HklVector a, b, c;
	HklVector axe;
	unsigned int angles_to_randomize;

	// La valeur des angles alpha, beta et gamma ne sont pas indépendant.
	// Il faut donc gérer les différents cas.
	hkl_parameter_randomize(self->a);
	hkl_parameter_randomize(self->b);
	hkl_parameter_randomize(self->c);

	angles_to_randomize = !self->alpha->not_to_fit
		+ !self->beta->not_to_fit
		+ !self->gamma->not_to_fit;
	switch (angles_to_randomize) {
	case 0:
		break;
	case 1:
		if (!self->alpha->not_to_fit) {// alpha
			a = b = c = vector_x;

			// randomize b
			hkl_vector_randomize_vector(&axe, &a);
			hkl_vector_rotated_around_vector(&b, &axe, self->gamma->value);

			// randomize c
			hkl_vector_randomize_vector(&axe, &a);
			hkl_vector_rotated_around_vector(&c, &axe, self->beta->value);

			//compute the alpha angle.
			self->alpha->value = hkl_vector_angle(&b, &c);
		} else if (!self->beta->not_to_fit) {
			// beta
			a = b = vector_x;

			// randomize b
			hkl_vector_randomize_vector(&axe, &a);
			hkl_vector_rotated_around_vector(&b, &axe, self->gamma->value);

			// randomize c
			c = b;
			hkl_vector_randomize_vector(&axe, &b);
			hkl_vector_rotated_around_vector(&c, &axe, self->alpha->value);

			//compute beta
			self->beta->value = hkl_vector_angle(&a, &c);
		} else {
			// gamma
			a = c = vector_x;

			// randomize c
			hkl_vector_randomize_vector(&axe, &a);
			hkl_vector_rotated_around_vector(&c, &axe, self->beta->value);

			// randomize b
			b = c;
			hkl_vector_randomize_vector(&axe, &c);
			hkl_vector_rotated_around_vector(&b, &axe, self->alpha->value);

			//compute gamma
			self->gamma->value = hkl_vector_angle(&a, &b);
		}
		break;
	case 2:
		if (!self->alpha->not_to_fit) {
			if (!self->beta->not_to_fit) {// alpha + beta
				a = b = vector_x;

				// randomize b
				hkl_vector_randomize_vector(&axe, &a);
				hkl_vector_rotated_around_vector(&b, &axe, self->gamma->value);

				//randomize c
				hkl_vector_randomize_vector_vector(&c, &a, &b);

				self->alpha->value = hkl_vector_angle(&b, &c);
				self->beta->value = hkl_vector_angle(&a, &c);
			} else {
				// alpha + gamma
				a = c = vector_x;

				// randomize c
				hkl_vector_randomize_vector(&axe, &a);
				hkl_vector_rotated_around_vector(&c, &axe, self->beta->value);

				//randomize c
				hkl_vector_randomize_vector_vector(&b, &a, &c);

				self->alpha->value = hkl_vector_angle(&b, &c);
				self->gamma->value = hkl_vector_angle(&a, &b);
			}
		} else {
			// beta + gamma
			b = c = vector_x;

			// randomize c
			hkl_vector_randomize_vector(&axe, &b);
			hkl_vector_rotated_around_vector(&c, &axe, self->alpha->value);

			//randomize c
			hkl_vector_randomize_vector_vector(&a, &b, &c);

			self->beta->value = hkl_vector_angle(&a, &c);
			self->gamma->value = hkl_vector_angle(&a, &b);
		}
		break;
	case 3:
		hkl_vector_randomize(&a);
		hkl_vector_randomize_vector(&b, &a);
		hkl_vector_randomize_vector_vector(&c, &b, &a);

		self->alpha->value = hkl_vector_angle(&b, &c);
		self->beta->value = hkl_vector_angle(&a, &c);
		self->gamma->value = hkl_vector_angle(&a, &b);
		break;
	}
}

void hkl_lattice_fprintf(FILE *f, HklLattice const *self)
{
	fprintf(f, "\n");
	hkl_parameter_fprintf(f, self->a);
	fprintf(f, "\n");
	hkl_parameter_fprintf(f, self->b);
	fprintf(f, "\n");
	hkl_parameter_fprintf(f, self->c);
	fprintf(f, "\n");
	hkl_parameter_fprintf(f, self->alpha);
	fprintf(f, "\n");
	hkl_parameter_fprintf(f, self->beta);
	fprintf(f, "\n");
	hkl_parameter_fprintf(f, self->gamma);
}
