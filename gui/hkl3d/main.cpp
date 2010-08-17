/* 
 * This file is part of the hkl3d library.
 * inspired from logo-model.c of the GtkGLExt logo models.
 * written by Naofumi Yasufuku  <naofumi@users.sourceforge.net>
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
 * Copyright (C) 2010      Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Oussama Sboui <oussama.sboui@synchrotron-soleil.fr>
 *          Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include <iostream>
#include <cstring>

#include <gtkglmm.h>

#include "hkl.h"
#include "hkl3d-gui-application.h"

#define MODEL_FILE "../../data/diffabs.yaml"

int main(int argc, char** argv)
{
	const HklGeometryConfig *config;
	HklGeometry *geometry;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);

	Hkl3D hkl3d(MODEL_FILE, geometry);
	Gtk::Main kit(argc, argv);
	// Init gtkglextmm.
	//
	Gtk::GL::init(argc, argv);
	
	std::cout << "HKL3D Demo\n"<< std::endl;

	//
	// Instantiate and run the application.
	//
	Hkl3dGui::Application application(hkl3d);

	kit.run();

	hkl_geometry_free(geometry);

	return 0;
}
