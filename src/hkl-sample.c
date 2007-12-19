#include <hkl/hkl-sample.h>

void hkl_sample_new(HklSample *sample, HklGeometry *geometry, char const *name, HklSampleType type)
{
	HklSample *sample = NULL;
	sample = malloc(sizeof(sample));
	if (sample)
		hkl_sample_init(sample, geometry, name, type);
}

void hkl_sample_init(HklSample *sample, HklGeometry *geometry, char const *name, HklSampleType type)
{
	sample->geometry = geometry;
	sample->name = name;
	sample->type = type;
}

void hkl_sample_release(HklSample *sample)
{
}

void hkl_sample_free(HklSample *sample)
{
	if (sample) {
		hkl_sample_release(sample);
		free sample;
		sample = NULL;
	}
}

void hkl_sample_copy(HklSample *src, HklSample *dst)
{
	dst = src;
}

void hkl_sample_get_UB(HklSample *sample, HklMatrix *matrix)
{
}

void hkl_sample_affine(HklSample *sample)
{
	
}

void hkl_sample_fprintf(HklSample *sample, FILE *f)
{
	fprintf(f, "\"%s\"\n", sample->name);
}
