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
#ifndef _VALUE_TEST_H_
#define _VALUE_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include "value.h"

using namespace hkl;

class valueTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( valueTest );

    CPPUNIT_TEST( Constructors );
    CPPUNIT_TEST( GetSet );
    CPPUNIT_TEST( Comparisons );
    CPPUNIT_TEST( PlusEqual );
    CPPUNIT_TEST( DivideEqual );
    CPPUNIT_TEST( operators );
    CPPUNIT_TEST( fabs );

    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    Value m_value;

  public:

    void setUp(void);
    void tearDown(void);

    void Constructors(void);
    void GetSet(void);
    void Comparisons(void);
    void PlusEqual(void);
    void DivideEqual(void);
    void operators(void);
    void fabs(void);
    void persistanceIO(void);
  };

#endif //_VALUE_TEST_H_
