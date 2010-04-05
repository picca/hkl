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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl/hkl-pseudoaxis-common.h>

int hkl_pseudo_axis_engine_init_func(HklPseudoAxisEngineMode *mode,
				     HklPseudoAxisEngine *self,
				     HklGeometry *geometry,
				     HklDetector const *detector,
				     HklSample const *sample)
{
	if (!self || !mode || !geometry || !detector || !sample)
		return HKL_FAIL;

	/* update the geometry internals */
	hkl_geometry_update(geometry);

	if(mode->geometry_init)
		hkl_geometry_free(mode->geometry_init);
	mode->geometry_init = hkl_geometry_new_copy(geometry);
	
	if(mode->detector_init)
		hkl_detector_free(mode->detector_init);
	mode->detector_init = hkl_detector_new_copy(detector);

	if(mode->sample_init)
		hkl_sample_free(mode->sample_init);
	mode->sample_init = hkl_sample_new_copy(sample);

	return HKL_SUCCESS;
}
