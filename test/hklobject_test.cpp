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
#include "hklobject_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( HKLObjectTest );

void
HKLObjectTest::setUp(void) {}

void
HKLObjectTest::tearDown(void) {}

void
HKLObjectTest::constructors(void)
{
  CPPUNIT_ASSERT_THROW(HKLObject("", ""), HKLException);
  CPPUNIT_ASSERT_THROW(HKLObject("titi", ""), HKLException);
  CPPUNIT_ASSERT_NO_THROW(HKLObject("titi", "toto"));

  // 1st constructor
  HKLObject hklObject("titi", "toto");
  CPPUNIT_ASSERT_EQUAL(std::string("titi"), hklObject.get_name());
  CPPUNIT_ASSERT_EQUAL(std::string("toto"), hklObject.get_description());

  // copy constructor
  HKLObject hklObject1(hklObject);

  CPPUNIT_ASSERT_EQUAL(hklObject, hklObject1);
}

void
HKLObjectTest::persistanceIO(void)
{
  HKLObject hklObject_ref("toto","titi");
  HKLObject hklObject1_ref("titi","toto");

  HKLObject hklObject("tutu","tata");
  HKLObject hklObject1("tata","tutu");

  std::stringstream flux;
  hklObject_ref.toStream(flux);
  hklObject1_ref.toStream(flux);
  hklObject.fromStream(flux);
  hklObject1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(hklObject_ref, hklObject);
  CPPUNIT_ASSERT_EQUAL(hklObject1_ref, hklObject1);
}
