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
 * Copyright (C) 2003-2007 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef _GEOMETRY_EULERIAN6C_TEST_H
#define _GEOMETRY_EULERIAN6C_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "eulerian6C_geometry.h"

class GeometryEulerian6CTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( GeometryEulerian6CTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( otherConstructors );
    CPPUNIT_TEST( setAngles );
    CPPUNIT_TEST( get_sample_quaternion );
    CPPUNIT_TEST( get_sample_rotation_matrix );
    CPPUNIT_TEST( get_Q );
    CPPUNIT_TEST( get_kf );
    CPPUNIT_TEST( get_distance );
    CPPUNIT_TEST( setFromGeometry );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::eulerian6C::Geometry * _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void equal(void);
    void copyConstructor(void);
    void otherConstructors(void);
    void setAngles(void);
    void addSampleDetectorAxe(void);
    void operateurs(void);
    void get_sample_quaternion(void);
    void get_sample_rotation_matrix(void);
    void get_Q(void);
    void get_kf(void);
    void get_distance(void);
    void setFromGeometry(void);
    void persistanceIO(void);
  };

#endif /* _GEOMETRY_EULERIAN6C_TEST_H */
