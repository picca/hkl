#include <math.h>

#include "config.h"
#include "lattice.h"
#include "smatrix.h"
#include "svector.h"

int hkl_lattice_init(struct hkl_lattice *lattice, double a, double b, double c, double alpha, double beta, double gamma)
{
	double D;

	/* check the validity of the lattice */
	D = 1 - cos(alpha)*cos(alpha) - cos(beta)*cos(beta) - cos(gamma)*cos(gamma) + 2*cos(alpha)*cos(beta)*cos(gamma);
	if (D < 0.)
		return HKL_FAIL;

	hkl_parameter_init(&lattice->a, "a", 0, a, a + 100, 1);
	hkl_parameter_init(&lattice->b, "b", 0, b, b + 100, 1);
	hkl_parameter_init(&lattice->c, "c", 0, c, c + 100, 1);

	if (!hkl_parameter_init(&lattice->alpha, "alpha", 0, alpha , 180. * HKL_DEGTORAD, 1)) return HKL_FAIL;
	if (!hkl_parameter_init(&lattice->beta, "beta", 0, beta, 180. * HKL_DEGTORAD, 1)) return HKL_FAIL;
	if (!hkl_parameter_init(&lattice->gamma, "gamma", 0, gamma, 180. * HKL_DEGTORAD, 1)) return HKL_FAIL;

	return HKL_SUCCESS;
}

int hkl_lattice_get_B(struct hkl_lattice const *lattice, struct hkl_smatrix *B)
{
	double D;
	double cos_alpha, sin_alpha;
	double cos_beta, sin_beta;
	double cos_gamma, sin_gamma;
	double a_star, b_star_cos_gamma_star, b_star_sin_gamma_star;
	double tmp;
	double c_star_cos_beta_star, c_star_sin_beta_star_cos_alpha_star;

	cos_alpha = cos(lattice->alpha.value);
	cos_beta = cos(lattice->beta.value);
	cos_gamma = cos(lattice->gamma.value);
	D = 1 - cos_alpha*cos_alpha - cos_beta*cos_beta - cos_gamma*cos_gamma + 2*cos_alpha*cos_beta*cos_gamma;

	if (D > 0.)
		D = sqrt(D);
	else
		return HKL_FAIL;

	sin_alpha = sin(lattice->alpha.value);
	sin_beta = sin(lattice->beta.value);
	sin_gamma = sin(lattice->gamma.value);

	// optimization (18*, 3+)
	a_star = HKL_TAU * sin_alpha / (lattice->a.value * D);

	b_star_sin_gamma_star = HKL_TAU / (lattice->b.value * sin_alpha);
	b_star_cos_gamma_star = b_star_sin_gamma_star / D * (cos_alpha*cos_beta - cos_gamma);

	tmp = HKL_TAU / (lattice->c.value * sin_alpha);
	c_star_cos_beta_star = tmp / D * (cos_gamma*cos_alpha - cos_beta);
	c_star_sin_beta_star_cos_alpha_star = tmp / (sin_beta * sin_gamma) * (cos_beta*cos_gamma - cos_alpha);
	// end of optimization

	B->data[0][0] = a_star;
	B->data[0][1] = b_star_cos_gamma_star;
	B->data[0][2] = c_star_cos_beta_star;

	B->data[1][0] = 0;
	B->data[1][1] = b_star_sin_gamma_star;
	B->data[1][2] = c_star_sin_beta_star_cos_alpha_star;

	B->data[2][0] = 0;
	B->data[2][1] = 0;
	B->data[2][2] = HKL_TAU / lattice->c.value;

	return HKL_SUCCESS;
}

int hkl_lattice_reciprocal(struct hkl_lattice const *lattice, struct hkl_lattice *reciprocal)
{
	double cos_alpha, cos_beta, cos_gamma;
	double sin_alpha, sin_beta, sin_gamma;
	double cos_beta1, cos_beta2, cos_beta3;
	double sin_beta1, sin_beta2, sin_beta3;
	double sin_beta_sin_gamma, sin_gamma_sin_alpha, sin_alpha_sin_beta;
	double D;

	cos_alpha = cos(lattice->alpha.value);
	cos_beta = cos(lattice->beta.value);
	cos_gamma = cos(lattice->gamma.value);
	D = 1 - cos_alpha*cos_alpha - cos_beta*cos_beta - cos_gamma*cos_gamma + 2*cos_alpha*cos_beta*cos_gamma;

	if (D > 0.)
		D = sqrt(D);
	else
		return HKL_FAIL;

	sin_alpha = sin(lattice->alpha.value);
	sin_beta = sin(lattice->beta.value);
	sin_gamma = sin(lattice->gamma.value);

	sin_beta_sin_gamma = sin_beta*sin_gamma;
	sin_gamma_sin_alpha = sin_gamma*sin_alpha;
	sin_alpha_sin_beta = sin_alpha*sin_beta;

	cos_beta1 = (cos_beta*cos_gamma - cos_alpha) / sin_beta_sin_gamma;
	cos_beta2 = (cos_gamma*cos_alpha - cos_beta) / sin_gamma_sin_alpha;
	cos_beta3 = (cos_alpha*cos_beta - cos_gamma) / sin_alpha_sin_beta;
	sin_beta1 = D / sin_beta_sin_gamma;
	sin_beta2 = D / sin_gamma_sin_alpha;
	sin_beta3 = D / sin_alpha_sin_beta;

	reciprocal->a.value = HKL_TAU * sin_alpha / (lattice->a.value * D);
	reciprocal->b.value = HKL_TAU * sin_beta / (lattice->b.value * D);
	reciprocal->c.value = HKL_TAU * sin_gamma / (lattice->c.value * D);
	reciprocal->alpha.value = atan2(sin_beta1, cos_beta1);
	reciprocal->beta.value = atan2(sin_beta2, cos_beta2);
	reciprocal->gamma.value = atan2(sin_beta3, cos_beta3);

	return HKL_SUCCESS;
}

void hkl_lattice_randomize(struct hkl_lattice *lattice)
{
	static struct hkl_svector svector_x = {{1, 0, 0}};
	struct hkl_svector a, b, c;
	struct hkl_svector axe;
	unsigned int angles_to_randomize;

	// La valeur des angles alpha, beta et gamma ne sont pas indépendant.
	// Il faut donc gérer les différents cas.
	hkl_parameter_randomize(&lattice->a);
	hkl_parameter_randomize(&lattice->b);
	hkl_parameter_randomize(&lattice->c);
	
	angles_to_randomize = !lattice->alpha.not_to_fit + !lattice->beta.not_to_fit + !lattice->gamma.not_to_fit;
	switch (angles_to_randomize) {
		case 0:
			break;
		case 1:
			if (!lattice->alpha.not_to_fit) {// alpha
				a = b = c = svector_x;

				// randomize b
				hkl_svector_randomize_svector(&axe, &a);
				hkl_svector_rotated_around_vector(&b, &axe, lattice->gamma.value);

				// randomize c
				hkl_svector_randomize_svector(&axe, &a);
				hkl_svector_rotated_around_vector(&c, &axe, lattice->beta.value);

				//compute the alpha angle.
				lattice->alpha.value = hkl_svector_angle(&b, &c);
			} else if (!lattice->beta.not_to_fit) {
				// beta
				a = b = svector_x;

				// randomize b
				hkl_svector_randomize_svector(&axe, &a);
				hkl_svector_rotated_around_vector(&b, &axe, lattice->gamma.value);

				// randomize c
				c = b;
				hkl_svector_randomize_svector(&axe, &b);
				hkl_svector_rotated_around_vector(&c, &axe, lattice->alpha.value);

				//compute beta
				lattice->beta.value = hkl_svector_angle(&a, &c);
			} else {
				// gamma
				a = c = svector_x;

				// randomize c
				hkl_svector_randomize_svector(&axe, &a);
				hkl_svector_rotated_around_vector(&c, &axe, lattice->beta.value);

				// randomize b
				b = c;
				hkl_svector_randomize_svector(&axe, &c);
				hkl_svector_rotated_around_vector(&b, &axe, lattice->alpha.value);

				//compute gamma
				lattice->gamma.value = hkl_svector_angle(&a, &b);
			}
			break;
		case 2:
			if (!lattice->alpha.not_to_fit) {
				if (!lattice->beta.not_to_fit) {// alpha + beta
					a = b = svector_x;

					// randomize b
					hkl_svector_randomize_svector(&axe, &a);
					hkl_svector_rotated_around_vector(&b, &axe, lattice->gamma.value);

					//randomize c
					hkl_svector_randomize_svector_svector(&c, &a, &b);

					lattice->alpha.value = hkl_svector_angle(&b, &c);
					lattice->beta.value = hkl_svector_angle(&a, &c);
				} else {
					// alpha + gamma
					a = c = svector_x;

					// randomize c
					hkl_svector_randomize_svector(&axe, &a);
					hkl_svector_rotated_around_vector(&c, &axe, lattice->beta.value);

					//randomize c
					hkl_svector_randomize_svector_svector(&b, &a, &c);

					lattice->alpha.value = hkl_svector_angle(&b, &c);
					lattice->gamma.value = hkl_svector_angle(&a, &b);
				}
			} else {
				// beta + gamma
				b = c = svector_x;

				// randomize c
				hkl_svector_randomize_svector(&axe, &b);
				hkl_svector_rotated_around_vector(&c, &axe, lattice->alpha.value);

				//randomize c
				hkl_svector_randomize_svector_svector(&a, &b, &c);

				lattice->beta.value = hkl_svector_angle(&a, &c);
				lattice->gamma.value = hkl_svector_angle(&a, &b);
			}
			break;
		case 3:
			hkl_svector_randomize(&a);
			hkl_svector_randomize_svector(&b, &a);
			hkl_svector_randomize_svector_svector(&c, &b, &a);

			lattice->alpha.value = hkl_svector_angle(&b, &c);
			lattice->beta.value = hkl_svector_angle(&a, &c);
			lattice->gamma.value = hkl_svector_angle(&a, &b);
			break;
	}
}
