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
// File to test matrix and vector implementation.

#ifndef SOURCE_TEST_H
#define SOURCE_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>

#include "source.h"

using namespace hkl;

class sourceTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( sourceTest );
    CPPUNIT_TEST( Constructor );
    CPPUNIT_TEST( Equal );
    CPPUNIT_TEST( CopyConstructor );
    CPPUNIT_TEST( SetWaveLength );
    CPPUNIT_TEST( SetDirection );
    CPPUNIT_TEST( GetSetKi );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    svector m_v;

  public:

    void setUp(void);
    void tearDown(void);

    void Constructor(void);
    void Equal(void);
    void CopyConstructor(void);
    void SetWaveLength(void);
    void SetDirection(void);
    void GetSetKi(void);
    void persistanceIO(void);
  };

#endif //SOURCE_TEST_H
