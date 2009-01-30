#ifndef __HKL_GEOMETRY_FACTORY_H__
#define __HKL_GEOMETRY_FACTORY_H__

#include <stdarg.h>
#include <hkl/hkl-geometry.h>

HKL_BEGIN_DECLS

enum _HklGeometryType
{
	HKL_GEOMETRY_TWOC_VERTICAL,
	HKL_GEOMETRY_EULERIAN4C_VERTICAL,
	HKL_GEOMETRY_KAPPA4C_VERTICAL,
	HKL_GEOMETRY_EULERIAN6C,
	HKL_GEOMETRY_KAPPA6C,
};

typedef enum _HklGeometryType HklGeometryType;

static void hkl_geometry_init_twoC_vertical(HklGeometry *self)
{
	HklHolder *h;

	self->name = "TwoC";
	h= hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);

	h= hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void hkl_geometry_init_eulerian4C_vertical(HklGeometry *self)
{
	HklHolder *h;

	self->name = "E4C";
	h= hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h= hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void hkl_geometry_init_kappa4C_vertical(HklGeometry *self, double alpha)
{
	HklHolder *h;

	self->name = "K4C";
	h= hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h= hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "tth", 0, -1, 0);
}

static void hkl_geometry_init_eulerian6C(HklGeometry *self)
{
	HklHolder *h;

	self->name = "E6C";
	h= hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "omega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "chi", 1, 0, 0);
	hkl_holder_add_rotation_axis(h, "phi", 0, -1, 0);

	h= hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "gamma", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "delta", 0, -1, 0);
}

static void hkl_geometry_init_kappa6C(HklGeometry *self, double alpha)
{
	HklHolder *h;

	self->name = "K6C";
	h= hkl_geometry_add_holder(self);
	hkl_holder_add_rotation_axis(h, "mu", 0, 0, 1);
	hkl_holder_add_rotation_axis(h, "komega", 0, -1, 0);
	hkl_holder_add_rotation_axis(h, "kappa", 0, -cos(alpha), -sin(alpha));
	hkl_holder_add_rotation_axis(h, "kphi", 0, -1, 0);

	h= hkl_geometry_add_holder(self);
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
			hkl_geometry_init_twoC_vertical(geom);
			break;
		case HKL_GEOMETRY_EULERIAN4C_VERTICAL:
			hkl_geometry_init_eulerian4C_vertical(geom);
			break;
		case HKL_GEOMETRY_KAPPA4C_VERTICAL:
			va_start(ap, type);
			alpha = va_arg(ap, double);
			va_end(ap);
			hkl_geometry_init_kappa4C_vertical(geom, alpha);
			break;
		case HKL_GEOMETRY_EULERIAN6C:
			hkl_geometry_init_eulerian6C(geom);
			break;
		case HKL_GEOMETRY_KAPPA6C:
			va_start(ap, type);
			alpha = va_arg(ap, double);
			va_end(ap);
			hkl_geometry_init_kappa6C(geom, alpha);
			break;
	}

	return geom;
}

HKL_END_DECLS

#endif /* __HKL_GEOMETRY_FACTORY_H__ */
