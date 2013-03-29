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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_FACTORY_H__
#define __HKL_FACTORY_H__

#include <hkl/hkl-geometry.h>
#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

typedef struct _HklFactory HklFactory;

extern const char *hkl_factory_name(const HklFactory *self);

extern HklGeometry *hkl_factory_create_new_geometry(const HklFactory *self);

extern HklEngineList *hkl_factory_create_new_engine_list(const HklFactory *self);

#define HKL_GEOMETRY_TWOC_DESCRIPTION					\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 1 axes for the sample\n"					\
	"\n"								\
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

#define HKL_GEOMETRY_EULERIAN4C_VERTICAL_DESCRIPTION					\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n"	\
	"  + **phi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"


#define HKL_GEOMETRY_EULERIAN6C_DESCRIPTION				\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 4 axes for the sample\n"					\
	"\n"								\
	"  + **mu** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **phi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 2 axes for the detector\n"					\
	"\n"								\
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

#define HKL_GEOMETRY_KAPPA4C_VERTICAL_DESCRIPTION			\
	"For this geometry there is a special parameters called :math:`\\alpha` which is the\n" \
	"angle between the kappa rotation axis and the  :math:`\\vec{y}` direction.\n" \
	"\n"								\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **komega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **kappa** : rotating around the :math:`\\vec{x}` direction (0, :math:`-\\cos\\alpha`, :math:`-\\sin\\alpha`)\n"	\
	"  + **kphi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`-\\\vec{y}` direction (0, -1, 0)\n"

#define HKL_GEOMETRY_KAPPA6C_DESCRIPTION				\
	"For this geometry there is a special parameters called :math:`\\alpha` which is the\n" \
	"angle between the kappa rotation axis and the  :math:`\\vec{y}` direction.\n" \
	"\n"								\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 4 axes for the sample\n"					\
	"\n"								\
	"  + **mu** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **komega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **kappa** : rotating around the :math:`\\vec{x}` direction (0, :math:`-\\cos\\alpha`, :math:`-\\sin\\alpha`)\n" \
	"  + **kphi** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 2 axes for the detector\n"					\
	"\n"								\
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

#define HKL_GEOMETRY_TYPE_ZAXIS_DESCRIPTION				\
	"For this geometry the **mu** axis is common to the sample and the detector.\n" \
	"\n"								\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 2 axes for the sample\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n"

#define HKL_GEOMETRY_TYPE_SOLEIL_MARS_DESCRIPTION			\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **omega** : rotating around the :math:`\\vec{z}` direction (0, -1, 0)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (-1, 0, 0)\n" \
	"  + **phi** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`\\vec{z}` direction (0, -1, 0)\n"

#define HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 2 axes for the sample\n"					\
	"\n"								\
	"  + **pitch** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **pitch** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

#define HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

#define HKL_GEOMETRY_TYPE_PETRA3_P09_EH2_DESCRIPTION			\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 4 axes for the sample\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **omega** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **phi** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 3 axis for the detector\n"					\
	"\n"								\
	"  + **mu** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **delta** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **gamma** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n"

#define HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **mu** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **omega** : rotating around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"\n"								\
	"+ 4 axis for the detector\n"					\
	"\n"								\
	"  + **beta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **gamma** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **delta** : rotation around the :math:`-\\vec{y}` direction (0, -1, 0)\n" \
	"  + **eta_a** : rotation around the :math:`-\\vec{x}` direction (-1, 0, 0)\n"

#define HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL_DESCRIPTION		\
	"+ xrays source fix allong the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"+ 3 axes for the sample\n"					\
	"\n"								\
	"  + **omega** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"  + **chi** : rotating around the :math:`\\vec{x}` direction (1, 0, 0)\n" \
	"  + **phi** : rotating around the :math:`\\vec{z}` direction (0, 0, 1)\n" \
	"\n"								\
	"+ 1 axis for the detector\n"					\
	"\n"								\
	"  + **tth** : rotation around the :math:`\\vec{z}` direction (0, 0, 1)\n"

static const HklGeometryConfig hkl_geometry_factory_configs[] =
{
	{"TwoC", HKL_GEOMETRY_TYPE_TWOC_VERTICAL, HKL_GEOMETRY_TWOC_DESCRIPTION},
	{"E4CV", HKL_GEOMETRY_TYPE_EULERIAN4C_VERTICAL, HKL_GEOMETRY_EULERIAN4C_VERTICAL_DESCRIPTION},
	{"K4CV", HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL, HKL_GEOMETRY_KAPPA4C_VERTICAL_DESCRIPTION},
	{"E6C", HKL_GEOMETRY_TYPE_EULERIAN6C, HKL_GEOMETRY_EULERIAN6C_DESCRIPTION},
	{"K6C", HKL_GEOMETRY_TYPE_KAPPA6C, HKL_GEOMETRY_KAPPA6C_DESCRIPTION},
	{"ZAXIS", HKL_GEOMETRY_TYPE_ZAXIS, HKL_GEOMETRY_TYPE_ZAXIS_DESCRIPTION},
	{"SOLEIL SIXS MED2+2", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2, HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_2_DESCRIPTION},
	{"SOLEIL MARS", HKL_GEOMETRY_TYPE_SOLEIL_MARS, HKL_GEOMETRY_TYPE_SOLEIL_MARS_DESCRIPTION},
	{"SOLEIL SIXS MED1+2", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2, HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_1_2_DESCRIPTION},
	{"PETRA3 P09 EH2", HKL_GEOMETRY_TYPE_PETRA3_P09_EH2, HKL_GEOMETRY_TYPE_PETRA3_P09_EH2_DESCRIPTION},
	{"SOLEIL SIXS MED2+3", HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3, HKL_GEOMETRY_TYPE_SOLEIL_SIXS_MED_2_3_DESCRIPTION},
	{"E4CH", HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL, HKL_GEOMETRY_TYPE_EULERIAN4C_HORIZONTAL_DESCRIPTION},
	{NULL}
};

extern const HklGeometryConfig *hkl_geometry_factory_get_config_from_type(HklGeometryType type);

extern HklGeometry *hkl_geometry_factory_new(const HklGeometryConfig *config, ...);

extern HklGeometry *hkl_geometry_factory_newv(const HklGeometryConfig *config,
					      const double parameters[], const int len);

extern HklEngineList *hkl_engine_list_factory(const HklGeometryConfig *config);

HKL_END_DECLS

#endif /* __HKL_FACTORY_H__ */
