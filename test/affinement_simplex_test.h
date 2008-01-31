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
#ifndef _AFFINEMENT_SIMPLEX_TEST_H_
#define _AFFINEMENT_SIMPLEX_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "affinement_simplex.h"
#include "samplefactory.h"
#include "eulerian4C_vertical_geometry.h"

using std::vector;

class Affinement_SimplexTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( Affinement_SimplexTest );

    CPPUNIT_TEST( Fit );
    CPPUNIT_TEST( Fit2 );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::affinement::Simplex _simplex;
    hkl::Sample * _sample;
    hkl::eulerian4C::vertical::Geometry * _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void Fit(void);
    void Fit2(void);
    void persistanceIO(void);
  };

#endif //_AFFINEMENT_SIMPLEX_TEST_H_
