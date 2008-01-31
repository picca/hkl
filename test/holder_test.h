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
#ifndef _HOLDER_TEST_H
#define _HOLDER_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "axe.h"
#include "holder.h"
#include "quaternion.h"

class HolderTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( HolderTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( add );
    CPPUNIT_TEST( apply );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

  private:
    hkl::HolderList * _holderList;
    hkl::Holder * _holder;

  public:

    void setUp(void);
    void tearDown(void);

    void equal(void);
    void copyConstructor(void);
    void add(void);
    void apply(void);
    void persistanceIO(void);
  };

#endif /* _HOLDER_TEST_H */
