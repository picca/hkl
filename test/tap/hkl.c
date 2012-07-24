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
#include <tap/hkl.h>

int check_pseudoaxes(HklPseudoAxisEngine *engine, ...)
{
	int res = HKL_TRUE;
	va_list ap;
	int i;

	/* extract the variable part of the method */
	va_start(ap, engine);
	for(i=0; i<engine->pseudoAxes_len; ++i){
		double value;

		value = va_arg(ap, double);
		res &= fabs(value - engine->pseudoAxes[i]->parent.value) <= HKL_EPSILON;
		if (!res){
			fprintf(stderr, "current: %f, expected: %f, epsilon: %f\n",
				engine->pseudoAxes[i]->parent.value, value, HKL_EPSILON);
		}
	}
	va_end(ap);
	return res;
}
