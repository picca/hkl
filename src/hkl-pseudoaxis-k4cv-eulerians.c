#include <gsl/gsl_math.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_sf_trig.h>

#include <hkl/hkl-pseudoaxis-k4cv-eulerians.h>
#include <hkl/hkl-pseudoaxis-common.h>

static int kappa_to_eulerian(double komega, double kappa, double kphi,
			     double *omega, double *chi, double *phi,
			     double alpha, int solution)
{
	double p = atan(tan(kappa/2.) * cos(alpha));

 	if (solution){
		*omega = komega + p - M_PI_2;
		*chi = 2 * asin(sin(kappa/2.) * sin(alpha));
		*phi = kphi + p + M_PI_2;
	}else{
		*omega = komega + p + M_PI_2;
		*chi = -2 * asin(sin(kappa/2.) * sin(alpha));
		*phi = kphi + p - M_PI_2;
	}

	return HKL_SUCCESS;
}

static int eulerian_to_kappa(double omega, double chi, double phi,
			     double *komega, double *kappa, double *kphi,
			     double alpha, double solution)
{
	int status = HKL_SUCCESS;

	if (fabs(chi) <= alpha * 2){
		double p = asin(tan(chi/2.)/tan(alpha));
		
		if (solution){
			*komega = omega - p + M_PI_2;
			*kappa = 2 * asin(sin(chi/2.)/sin(alpha));
			*kphi = phi - p - M_PI_2;
		}else{
			*komega = omega + p - M_PI_2;
			*kappa = -2 * asin(sin(chi/2.)/sin(alpha));
			*kphi = phi + p + M_PI_2;
		}
	}else
		status = HKL_FAIL;

	return status;
}

static int hkl_pseudo_axis_engine_get_set_get_k4cv_eulerians(HklPseudoAxisEngine *engine,
							     HklGeometry *geometry,
							     HklDetector const *detector,
							     HklSample const *sample)
{
	double komega, kappa, kphi;
	int solution;
	double alpha;

	hkl_geometry_update(geometry);

	solution = engine->getset->parameters[0].value;
	komega = hkl_geometry_get_axis_by_name(geometry, "komega")->config.value;
	kappa = hkl_geometry_get_axis_by_name(geometry, "kappa")->config.value;
	kphi = hkl_geometry_get_axis_by_name(geometry, "kphi")->config.value;

	return kappa_to_eulerian(komega, kappa, kphi,
				 &engine->pseudoAxes[0].config.value,
				 &engine->pseudoAxes[1].config.value,
				 &engine->pseudoAxes[2].config.value,
				 50 * HKL_DEGTORAD, solution);
}

static int hkl_pseudo_axis_engine_get_set_set_k4cv_eulerians(HklPseudoAxisEngine *engine,
							     HklGeometry *geometry,
							     HklDetector *detector,
							     HklSample *sample)
{
	int status = HKL_SUCCESS;
	int solution;

	double angles[3];

	hkl_pseudo_axis_engine_prepare_internal(engine, geometry, detector, sample);

	solution = engine->getset->parameters[0].value;

	status |= eulerian_to_kappa(engine->pseudoAxes[0].config.value,
				    engine->pseudoAxes[1].config.value,
				    engine->pseudoAxes[2].config.value,
				    &angles[0], &angles[1], &angles[2],
				    50 * HKL_DEGTORAD, solution);

	if (status == HKL_SUCCESS)
		hkl_pseudo_axis_engine_add_geometry(engine, angles);

	return status;
}

HklPseudoAxisEngine *hkl_pseudo_axis_engine_k4cv_eulerians_new(void)
{
	HklPseudoAxisEngine *self;
	HklPseudoAxisEngineGetSet *getset;
	HklParameter parameter = {NULL, {0, 1}, 1., 0};

	self = hkl_pseudo_axis_engine_new("eulerians", 3, "omega", "chi", "phi");

	// eulerians
	parameter.name = "solution";
	getset = hkl_pseudo_axis_engine_get_set_new(
		"eulerians",
		NULL,
		hkl_pseudo_axis_engine_get_set_get_k4cv_eulerians,
		hkl_pseudo_axis_engine_get_set_set_k4cv_eulerians,
		1, &parameter,
		3, "komega", "kappa", "kphi");
	hkl_pseudo_axis_engine_add_get_set(self, getset);

	return self;
}
