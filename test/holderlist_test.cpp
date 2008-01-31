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
#include "holderlist_test.h"
#include <axe_rotation.h>
#include <holder.h>

CPPUNIT_TEST_SUITE_REGISTRATION( HolderListTest );

void
HolderListTest::setUp(void)
{
  _holderList = new hkl::HolderList;
  hkl::Holder * holder = _holderList->add();
  holder->add_rotation("A", hkl::svector(0, 0, 1));
  holder->add_rotation("B", hkl::svector(0, 0, 1));
  holder = _holderList->add();
  holder->add_rotation("A", hkl::svector(0, 0, 1));
  holder->add_rotation("C", hkl::svector(0, 0, 1));
}

void
HolderListTest::tearDown(void)
{
  delete _holderList;
}

void
HolderListTest::add(void)
{
  hkl::HolderList holderList;

  // check the size of the empty holderList
  CPPUNIT_ASSERT_EQUAL((unsigned int)0, holderList.size());
  // check if an empty holder has been add.
  holderList.add();
  CPPUNIT_ASSERT_EQUAL((unsigned int)1, holderList.size());

  // add a second holder
  holderList.add();
  CPPUNIT_ASSERT_EQUAL((unsigned int)2, holderList.size());
}

void
HolderListTest::equal(void)
{
  // compare two empty HolderList
  CPPUNIT_ASSERT_EQUAL(_holderList, _holderList);
}

void
HolderListTest::copyConstructor(void)
{
  hkl::HolderList holderList(*_holderList);

  CPPUNIT_ASSERT_EQUAL(*_holderList, holderList);
}

void
HolderListTest::persistanceIO(void)
{
  hkl::HolderList holderList1(*_holderList);
  hkl::HolderList holderList2(*_holderList);
  std::stringstream flux;

  // modification of the holderList before saving it.
  hkl::Holder * holder = _holderList->add();
  holder->add_rotation("A", hkl::svector(0, 0, 1));

  _holderList->toStream(flux);
  _holderList->toStream(flux);
  holderList1.fromStream(flux);
  holderList2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*_holderList, holderList1);
  CPPUNIT_ASSERT_EQUAL(*_holderList, holderList2);
}

void
HolderListTest::profile(void)
{
  std::stringstream flux;
  //copy profile
  for (unsigned int i=0; i<100000; i++)
    {
      hkl::HolderList holder(*_holderList);
      holder.toStream(flux);
      holder.fromStream(flux);
    }

}
