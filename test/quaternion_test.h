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

#ifndef _QUATERNION_TEST_H_
#define _QUATERNION_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>

#include "quaternion.h"

using std::vector;
using namespace hkl;

class QuaternionTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( QuaternionTest );
    CPPUNIT_TEST( Constructor1 );
    CPPUNIT_TEST( Constructor2 );
    CPPUNIT_TEST( Constructor3 );
    CPPUNIT_TEST( Constructor4 );
    CPPUNIT_TEST( CopyConstructor );
    CPPUNIT_TEST( Equal );
    CPPUNIT_TEST( Affectation );
    CPPUNIT_TEST( PlusEqual );
    CPPUNIT_TEST( MinusEqual );
    CPPUNIT_TEST( TimesEqual );
    CPPUNIT_TEST( DivideEqual );
    CPPUNIT_TEST( Norm2 );
    CPPUNIT_TEST( Conjugate );
    CPPUNIT_TEST( DotProduct );
    CPPUNIT_TEST( Invert );
    CPPUNIT_TEST( AsMatrix );
    CPPUNIT_TEST( getAngleAndAxe );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

  public:

    void setUp(void);
    void tearDown(void);

    void Constructor1(void);
    void Constructor2(void);
    void Constructor3(void);
    void Constructor4(void);
    void CopyConstructor(void);
    void Equal(void);
    void Affectation(void);
    void PlusEqual(void);
    void MinusEqual(void);
    void TimesEqual(void);
    void DivideEqual(void);
    void Norm2(void);
    void Conjugate(void);
    void DotProduct(void);
    void Invert(void);
    void AsMatrix(void);
    void getAngleAndAxe(void);
    void persistanceIO(void);
  };

#endif //_QUATERNION_TEST_H_
