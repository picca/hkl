#ifndef __HKL_GEOMETRY_FACTORY_H__
#define __HKL_GEOMETRY_FACTORY_H__

#include <stdarg.h>
#include <hkl/hkl-geometry.h>

HKL_BEGIN_DECLS

typedef enum _HklGeometryType HklGeometryType;

enum _HklGeometryType
{
	HKL_GEOMETRY_TWOC_VERTICAL,
	HKL_GEOMETRY_EULERIAN4C_VERTICAL,
	HKL_GEOMETRY_KAPPA4C_VERTICAL,
	HKL_GEOMETRY_EULERIAN6C,
	HKL_GEOMETRY_KAPPA6C,
};

static void init_geometry_twoC_vertical(HklGeometry *geom)
{
	HklHolder *h;

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void init_geometry_eulerian4C_vertical(HklGeometry *geom)
{
	HklHolder *h;

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void init_geometry_kappa4C_vertical(HklGeometry *geom, double alpha)
{
	HklHolder *h;

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void init_geometry_eulerian6C(HklGeometry *geom)
{
	HklHolder *h;

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
}

static void init_geometry_kappa6C(HklGeometry *geom, double alpha)
{
	HklHolder *h;

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h= hkl_geometry_add_holder(geom);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
}


static HklGeometry *hkl_geometry_factory_new(HklGeometryType type, ...)
{
	HklGeometry *geom;
	double alpha;
	va_list ap;

	geom = hkl_geometry_new();
	switch(type) {
		case HKL_GEOMETRY_TWOC_VERTICAL:
			init_geometry_twoC_vertical(geom);
			break;
		case HKL_GEOMETRY_EULERIAN4C_VERTICAL:
			init_geometry_eulerian4C_vertical(geom);
			break;
		case HKL_GEOMETRY_KAPPA4C_VERTICAL:
			va_start(ap, type);
			alpha = va_arg(ap, double);
			va_end(ap);
			init_geometry_kappa4C_vertical(geom, alpha);
			break;
		case HKL_GEOMETRY_EULERIAN6C:
			init_geometry_eulerian6C(geom);
			break;
		case HKL_GEOMETRY_KAPPA6C:
			va_start(ap, type);
			alpha = va_arg(ap, double);
			va_end(ap);
			init_geometry_kappa6C(geom, alpha);
			break;
	}

	return geom;
}

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_FACTORY_H__ */
