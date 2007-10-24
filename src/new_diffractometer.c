#include "new_diffractometer.h"

struct hkl_diffractometer * create_empty_diffractometer(void)
{
	diff = malloc(sizeof(struct hkl_diffractometer));
	if (!diff)
		die("Can not create an empty diffractometer Oops");
	
	diff->geometry = NULL;
}

struct hkl_diffractometer * create_diffractometer(diff_type type, ...)
{
	struct hkl_diffractometer * diff = NULL;
	double alpha;
	va_list ap;

	diff = create_empty_diffractometer();
	switch(type)
	{
		case DIFF_TYPE_2C: init_2C_diffractometer(diff);
				   break;
		case DIFF_TYPE_E4CV: init_E4CV_diffractometer(diff);
				     break;
		case DIFF_TYPE_E6C: init_E6C_diffractometer(diff);
				    break;
		case DIFF_TYPE_K4CV:
				    va_start(ap, type);
				    alpha = va_arg(ap, double);
				    va_end(ap);
				    init_K4CV_diffractometer(diff, alpha);
				    break;
		case DIFF_TYPE_K6C: 
				    va_start(ap, type);
				    alpha = va_arg(ap, double);
				    va_end(ap);
				    init_K6C_diffractometer(diff, alpha);
				    break;
		default:
			die("Not a known diffractometer");
	}
	return diff;
}

static void free_diffractometer(struct hkl_diffractometer * diff)
{
	free_geometry(diff->geometry);
}

struct hkl_diffractometer_config * hkl_diffractometer_get_config(struct hkl_diffractometer * diff)
{
	struct hkl_diffractometer_config * config;

	config = malloc(sizeof(struct hkl_diffractometer_config));
	if (!config)
		die("Can not create a diffractometer config");
	
	config->geometry = hkl_geometry_get_config(diff->geometry);

	return config;
}

void hkl_diffractometer_set_config(struct hkl_diffractometer * diff, struct hkl_diffractometer_config const * config)
{
	hkl_geometry_set_config(diff->geometry, config->geometry);
}

void free_diffractometer_config(struct hkl_diffractometer_config * config)
{
	free_geometry_config(config->geometry);
	if (config)
		free(config);
	config = NULL;
}
