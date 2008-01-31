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
#ifndef _INTERVAL_TEST_H
#define _INTERVAL_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <interval.h>

class IntervalTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( IntervalTest );
    CPPUNIT_TEST( Constructors );
    CPPUNIT_TEST( Equal );
    CPPUNIT_TEST( GetSet );
    CPPUNIT_TEST( operators );
    CPPUNIT_TEST( cos );
    CPPUNIT_TEST( acos );
    CPPUNIT_TEST( sin );
    CPPUNIT_TEST( asin );
    CPPUNIT_TEST( tan );
    CPPUNIT_TEST( atan );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::Interval _interval;

  public:

    void setUp(void);
    void tearDown(void);

    void Constructors(void);
    void Equal(void);
    void GetSet(void);
    void operators(void);
    void cos(void);
    void acos(void);
    void sin(void);
    void asin(void);
    void tan(void);
    void atan(void);
    void persistanceIO(void);
  };

#endif //_RANGE_TEST_H
