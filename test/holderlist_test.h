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
#ifndef _HOLDERLIST_TEST_H
#define _HOLDERLIST_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <holder.h>

class HolderListTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( HolderListTest );
#ifndef PROFILE
    CPPUNIT_TEST( add );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( persistanceIO );
#else
    CPPUNIT_TEST( profile );
#endif

    CPPUNIT_TEST_SUITE_END();

  private:
    hkl::HolderList * _holderList;

  public:

    void setUp(void);
    void tearDown(void);

    void add(void);
    void equal(void);
    void copyConstructor(void);
    void persistanceIO(void);
    void profile(void);
  };

#endif /* _HOLDERLIST_TEST_H */
