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
#ifndef _MODE_EULERIAN4C_TEST_H_
#define _MODE_EULERIAN4C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <sstream>

#include "sample_monocrystal.h"
#include "eulerian4C_vertical_mode.h"

class Mode_Eulerian4C_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( Mode_Eulerian4C_Test );

    CPPUNIT_TEST( Bissector );
    CPPUNIT_TEST( Delta_Theta );
    CPPUNIT_TEST( Constant_Omega );
    CPPUNIT_TEST( Constant_Chi );
    CPPUNIT_TEST( Constant_Phi );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::sample::MonoCrystal * _sample;
    hkl::eulerian4C::vertical::Geometry _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Bissector(void);
    void Delta_Theta(void);
    void Constant_Omega(void);
    void Constant_Chi(void);
    void Constant_Phi(void);
    void persistanceIO(void);
  };

#endif //_MODE_EULERIAN4C_TEST_H_
