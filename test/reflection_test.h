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
 * Copyright (C) 2003-2008 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef _REFLECTION_TEST_H
#define _REFLECTION_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <string>

#include "source.h"
#include "reflection.h"
#include "eulerian4C_vertical_geometry.h"

using std::cout;
using std::string;

class ReflectionTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( ReflectionTest );
    CPPUNIT_TEST( Constructor );
    CPPUNIT_TEST( Equal );
    CPPUNIT_TEST( GetSet );
    CPPUNIT_TEST( GetHKL );
    CPPUNIT_TEST( ComputeAngle );
    CPPUNIT_TEST( isColinear );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::Reflection * _reflections;
    hkl::eulerian4C::vertical::Geometry * _geometry;
    hkl::Source _source;

  public:

    void setUp(void);
    void tearDown(void);

    void Constructor(void);
    void Equal(void);
    void GetSet(void);
    void GetHKL(void);
    void ComputeAngle(void);
    void isColinear(void);
    void persistanceIO(void);
  };

#endif /* _REFLECTION_TEST_H */
