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
#ifndef _SAMPLELIST_TEST_H
#define _SAMPLELIST_TEST_H

#include <cppunit/extensions/HelperMacros.h>

#include "samplelist.h"
#include "eulerian4C_vertical_geometry.h"

class SampleListTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( SampleListTest );
    CPPUNIT_TEST( operators );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    hkl::SampleList * _sampleList;
    hkl::eulerian4C::vertical::Geometry _geometry;

  public:

    void setUp(void);
    void tearDown(void);

    void operators(void);
    void persistanceIO(void);
  };

#endif /* _SAMPLELIST_TEST_H */
