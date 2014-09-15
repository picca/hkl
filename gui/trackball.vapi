/*
 * This file is part of the hkl library.
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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

[CCode (lower_case_cprefix ="", cheader_filename="trackball.h")]
namespace Trackball
{
	public static void trackball([CCode (array_length=false)] float[] q,
								 float p1x, float p1y, float p2x, float p2y);

	public static void add_quats(float *q1, float *q2, float *dest);

	public static void build_rotmatrix([CCode (array_length=false, type="float *")] float[] m,
									   [CCode (array_length=false)] float[] q);

	public static void axis_to_quat(float *a, float phi, float *q);
}
