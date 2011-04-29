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
 * Copyright (C) 2010-2011 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

[CCode (cheader_filename="g3d/g3d.h")]
namespace G3D
{
	public struct Vector : float {}

	[CCode (ref_function="", unref_function="")]
	public class Face
	{
		public Material material;
		public uint32 *vertex_indices;

	}

	[Compact]
	public class Material
	{
		public float a;
	}

	public struct Transformation
	{
		public float matrix[16];
	}

	[Compact]
	public class Object
	{
		public GLib.SList<Face> faces;
		public G3D.Vector *vertex_data;
		public Transformation? transformation;
	}
}