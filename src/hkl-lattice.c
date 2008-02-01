#include <stdlib.h>
#include <math.h>

#include "hkl/hkl-lattice.h"

/* private */

static double check_lattice_param(double a, double b, double c,
		double alpha, double beta, double gamma)
{
	return 1. - cos(alpha)*cos(alpha) - cos(beta)*cos(beta)
		- cos(gamma)*cos(gamma) + 2. * cos(alpha)*cos(beta)*cos(gamma);
}

/* public */

HklLattice *hkl_lattice_new(double a, double b, double c, double alpha, double beta, double gamma)
{
	HklLattice *l = NULL;
	if(check_lattice_param(a, b, c, alpha, beta, gamma) > 0.) {
		l = malloc(sizeof(*l));
		if (!l)
			die("Can not allocate memory for an HklLattice");

		l->a = hkl_parameter_new("a", 0, a, a+10, 0);
		l->b = hkl_parameter_new("b", 0, b, b+10, 0);
		l->c = hkl_parameter_new("c", 0, c, c+10, 0);
		l->alpha = hkl_parameter_new("alpha", -M_PI, alpha, M_PI, 0);
		l->beta = hkl_parameter_new("beta", -M_PI, beta, M_PI, 0);
		l->gamma = hkl_parameter_new("gamma", -M_PI, gamma, M_PI, 0);
	}

	return l;	
}

HklLattice *hkl_lattice_new_copy(HklLattice const *l)
{
	HklLattice *copy = NULL;

	copy = malloc(sizeof(*copy));
	if (!copy)
		die("Can not allocate memory for an HklLattice");
	
	copy->a = hkl_parameter_new_copy(l->a);
	copy->b = hkl_parameter_new_copy(l->b);
	copy->c = hkl_parameter_new_copy(l->c);
	copy->alpha = hkl_parameter_new_copy(l->alpha);
	copy->beta = hkl_parameter_new_copy(l->beta);
	copy->gamma = hkl_parameter_new_copy(l->gamma);

	return copy;	
}

HklLattice* hkl_lattice_new_default(void)
{
	return hkl_lattice_new(1.54, 1.54, 1.54,
			90*HKL_DEGTORAD, 90*HKL_DEGTORAD, 90*HKL_DEGTORAD);
}

void hkl_lattice_free(HklLattice *l)
{
	hkl_parameter_free(l->a);
	hkl_parameter_free(l->b);
	hkl_parameter_free(l->c);
	hkl_parameter_free(l->alpha);
	hkl_parameter_free(l->beta);
	hkl_parameter_free(l->gamma);
	free(l);
}

int hkl_lattice_set(HklLattice *l, double a, double b, double c,
		double alpha, double beta, double gamma)
{
	int res = HKL_FAIL;

	if(check_lattice_param(a, b, c, alpha, beta, gamma) > 0.) {
		l->a->value = a;
		l->b->value = b;
		l->c->value = c;
		l->alpha->value = alpha;
		l->beta->value = beta;
		l->gamma->value = gamma;
		res = HKL_SUCCESS; 
	}
	return res;
}

/* 
 * Get the B matrix from the l parameters 
 */
int hkl_lattice_get_B(HklLattice const *l, HklMatrix *B)
{
	double D;
	double c_alpha, s_alpha;
	double c_beta, s_beta;
	double c_gamma, s_gamma;
	double b11, b22, tmp;

	c_alpha = cos(l->alpha->value);
	c_beta = cos(l->beta->value);
	c_gamma = cos(l->gamma->value);
	D = 1 - c_alpha*c_alpha - c_beta*c_beta - c_gamma*c_gamma
		+ 2*c_alpha*c_beta*c_gamma;

	if (D > 0.)
		D = sqrt(D);
	else
		return HKL_FAIL;

	s_alpha = sin(l->alpha->value);
	s_beta = sin(l->beta->value);
	s_gamma = sin(l->gamma->value);

	b11 = HKL_TAU / (l->b->value * s_alpha);
	b22 = HKL_TAU / l->c->value;
	tmp = b22 / s_alpha;

	B->data[0][0] = HKL_TAU * s_alpha / (l->a->value * D);
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

int hkl_lattice_reciprocal(HklLattice const *l, HklLattice *reciprocal)
{
	double c_alpha, c_beta, c_gamma;
	double s_alpha, s_beta, s_gamma;
	double c_beta1, c_beta2, c_beta3;
	double s_beta1, s_beta2, s_beta3;
	double s_beta_s_gamma, s_gamma_s_alpha, s_alpha_s_beta;
	double D;

	c_alpha = cos(l->alpha->value);
	c_beta = cos(l->beta->value);
	c_gamma = cos(l->gamma->value);
	D = 1 - c_alpha*c_alpha - c_beta*c_beta - c_gamma*c_gamma + 2*c_alpha*c_beta*c_gamma;

	if (D > 0.)
		D = sqrt(D);
	else
		return HKL_FAIL;

	s_alpha = sin(l->alpha->value);
	s_beta = sin(l->beta->value);
	s_gamma = sin(l->gamma->value);

	s_beta_s_gamma = s_beta*s_gamma;
	s_gamma_s_alpha = s_gamma*s_alpha;
	s_alpha_s_beta = s_alpha*s_beta;

	c_beta1 = (c_beta*c_gamma - c_alpha) / s_beta_s_gamma;
	c_beta2 = (c_gamma*c_alpha - c_beta) / s_gamma_s_alpha;
	c_beta3 = (c_alpha*c_beta - c_gamma) / s_alpha_s_beta;
	s_beta1 = D / s_beta_s_gamma;
	s_beta2 = D / s_gamma_s_alpha;
	s_beta3 = D / s_alpha_s_beta;

	reciprocal->a->value = HKL_TAU * s_alpha / (l->a->value * D);
	reciprocal->b->value = HKL_TAU * s_beta / (l->b->value * D);
	reciprocal->c->value = HKL_TAU * s_gamma / (l->c->value * D);
	reciprocal->alpha->value = atan2(s_beta1, c_beta1);
	reciprocal->beta->value = atan2(s_beta2, c_beta2);
	reciprocal->gamma->value = atan2(s_beta3, c_beta3);

	return HKL_SUCCESS;
}

void hkl_lattice_randomize(HklLattice *l)
{
	static HklVector vector_x = {{1, 0, 0}};
	HklVector a, b, c;
	HklVector axe;
	unsigned int angles_to_randomize;

	// La valeur des angles alpha, beta et gamma ne sont pas indépendant.
	// Il faut donc gérer les différents cas.
	hkl_parameter_randomize(l->a);
	hkl_parameter_randomize(l->b);
	hkl_parameter_randomize(l->c);

	angles_to_randomize = !l->alpha->not_to_fit + !l->beta->not_to_fit + !l->gamma->not_to_fit;
	switch (angles_to_randomize) {
		case 0:
			break;
		case 1:
			if (!l->alpha->not_to_fit) {// alpha
				a = b = c = vector_x;

				// randomize b
				hkl_vector_randomize_vector(&axe, &a);
				hkl_vector_rotated_around_vector(&b, &axe, l->gamma->value);

				// randomize c
				hkl_vector_randomize_vector(&axe, &a);
				hkl_vector_rotated_around_vector(&c, &axe, l->beta->value);

				//compute the alpha angle.
				l->alpha->value = hkl_vector_angle(&b, &c);
			} else if (!l->beta->not_to_fit) {
				// beta
				a = b = vector_x;

				// randomize b
				hkl_vector_randomize_vector(&axe, &a);
				hkl_vector_rotated_around_vector(&b, &axe, l->gamma->value);

				// randomize c
				c = b;
				hkl_vector_randomize_vector(&axe, &b);
				hkl_vector_rotated_around_vector(&c, &axe, l->alpha->value);

				//compute beta
				l->beta->value = hkl_vector_angle(&a, &c);
			} else {
				// gamma
				a = c = vector_x;

				// randomize c
				hkl_vector_randomize_vector(&axe, &a);
				hkl_vector_rotated_around_vector(&c, &axe, l->beta->value);

				// randomize b
				b = c;
				hkl_vector_randomize_vector(&axe, &c);
				hkl_vector_rotated_around_vector(&b, &axe, l->alpha->value);

				//compute gamma
				l->gamma->value = hkl_vector_angle(&a, &b);
			}
			break;
		case 2:
			if (!l->alpha->not_to_fit) {
				if (!l->beta->not_to_fit) {// alpha + beta
					a = b = vector_x;

					// randomize b
					hkl_vector_randomize_vector(&axe, &a);
					hkl_vector_rotated_around_vector(&b, &axe, l->gamma->value);

					//randomize c
					hkl_vector_randomize_vector_vector(&c, &a, &b);

					l->alpha->value = hkl_vector_angle(&b, &c);
					l->beta->value = hkl_vector_angle(&a, &c);
				} else {
					// alpha + gamma
					a = c = vector_x;

					// randomize c
					hkl_vector_randomize_vector(&axe, &a);
					hkl_vector_rotated_around_vector(&c, &axe, l->beta->value);

					//randomize c
					hkl_vector_randomize_vector_vector(&b, &a, &c);

					l->alpha->value = hkl_vector_angle(&b, &c);
					l->gamma->value = hkl_vector_angle(&a, &b);
				}
			} else {
				// beta + gamma
				b = c = vector_x;

				// randomize c
				hkl_vector_randomize_vector(&axe, &b);
				hkl_vector_rotated_around_vector(&c, &axe, l->alpha->value);

				//randomize c
				hkl_vector_randomize_vector_vector(&a, &b, &c);

				l->beta->value = hkl_vector_angle(&a, &c);
				l->gamma->value = hkl_vector_angle(&a, &b);
			}
			break;
		case 3:
			hkl_vector_randomize(&a);
			hkl_vector_randomize_vector(&b, &a);
			hkl_vector_randomize_vector_vector(&c, &b, &a);

			l->alpha->value = hkl_vector_angle(&b, &c);
			l->beta->value = hkl_vector_angle(&a, &c);
			l->gamma->value = hkl_vector_angle(&a, &b);
			break;
	}
}
