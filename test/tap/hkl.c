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

int check_pseudoaxes_v(HklPseudoAxisEngine *engine, ...)
{
	uint i;
	va_list ap;
	double values[engine->info->n_pseudo_axes];

	/* extract the variable part of the method */
	va_start(ap, engine);
	for(i=0; i<engine->info->n_pseudo_axes; ++i)
		values[i] = va_arg(ap, double);
	va_end(ap);

	return check_pseudoaxes(engine, values, engine->info->n_pseudo_axes);
}

int check_pseudoaxes(HklPseudoAxisEngine *engine, double expected[], uint len)
{
	int res = HKL_TRUE;
	uint i;
	HklParameter *parameter;

	hkl_assert(engine->info->n_pseudo_axes == len);

	i = 0;
	list_for_each(&engine->pseudo_axes.parameters, parameter, list){
		const double current = hkl_parameter_get_value(parameter);
		res &= fabs(current - expected[i]) <= HKL_EPSILON;
		if (!res){
			fprintf(stderr, "current: %f, expected: %f, epsilon: %f\n",
				current, expected[i], HKL_EPSILON);
		}
		i++;
	}
	return res;
}

/**
 * hkl_pseudo_axis_engine_set_values_v: (skip)
 * @self: the PseudoAxisEngine
 * @values: the values to set 
 *
 * set the values of the PseudoAxes with the given values. This method
 * is only available for test as it is sort of brittle.
 **/
void hkl_pseudo_axis_engine_set_values_v(HklPseudoAxisEngine *self, ...)
{
	uint i;
	va_list ap;
	double values[self->info->n_pseudo_axes];

	va_start(ap, self);
	for(i=0; i<self->info->n_pseudo_axes; ++i)
		values[i] = va_arg(ap, double);
		
	va_end(ap);
	hkl_parameter_list_set_values(&self->pseudo_axes,
				      values, self->info->n_pseudo_axes,
				      NULL);
}
