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
#include "reflectionlist_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ReflectionListTest );

void
ReflectionListTest::setUp(void)
{
  _reflectionList = new hkl::ReflectionList(_geometry, hkl::REFLECTION_MONOCRYSTAL);
}

void
ReflectionListTest::tearDown(void)
{
  delete _reflectionList;
}

void
ReflectionListTest::operators(void)
{
  // ==
  CPPUNIT_ASSERT_EQUAL(_reflectionList, _reflectionList);

  // []
  CPPUNIT_ASSERT_THROW((*_reflectionList)[0], HKLException);
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_NO_THROW((*_reflectionList)[0]);
  CPPUNIT_ASSERT_THROW((*_reflectionList)[1], HKLException);
}

void
ReflectionListTest::clone(void)
{
  _geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  hkl::ReflectionList * factory = NULL;
  CPPUNIT_ASSERT_NO_THROW( factory = _reflectionList->clone());
  CPPUNIT_ASSERT_EQUAL(*_reflectionList, *factory);
  delete factory;
}

void
ReflectionListTest::add(void)
{
  // add 2 timesd the last reflection and test
  // if the last reflection have the flag set to false.
  // we can not have two identical reflection (h1, k1, l1) == (h2, k2, l2) active
  // for calculation.
  _geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_EQUAL(true, (*_reflectionList)[0]->flag());
  CPPUNIT_ASSERT_EQUAL(false, (*_reflectionList)[1]->flag());
}

void
ReflectionListTest::del(void)
{
  // add 2 timesd the last reflection and test
  // if deletion is ok.
  _geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->del(0));
  CPPUNIT_ASSERT_THROW(_reflectionList->del(1), HKLException);
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->del(0));
}

void
ReflectionListTest::size(void)
{
  _geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_EQUAL((unsigned int)0, _reflectionList->size());
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_EQUAL((unsigned int)1, _reflectionList->size());
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_EQUAL((unsigned int)2, _reflectionList->size());
}

void
ReflectionListTest::size_indep(void)
{
  _geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_EQUAL((unsigned int)0, _reflectionList->size_indep());
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_EQUAL((unsigned int)1, _reflectionList->size_indep());
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 1, 1)));
  CPPUNIT_ASSERT_EQUAL((unsigned int)1, _reflectionList->size_indep());
  CPPUNIT_ASSERT_NO_THROW(_reflectionList->add(hkl::svector(1, 0, 1)));
  CPPUNIT_ASSERT_EQUAL((unsigned int)2, _reflectionList->size_indep());
}

void
ReflectionListTest::persistanceIO(void)
{
  hkl::ReflectionList * reflectionList = new hkl::ReflectionList(_geometry, hkl::REFLECTION_MONOCRYSTAL);
  std::stringstream flux;

  _reflectionList->toStream(flux);
  reflectionList->fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*_reflectionList, *reflectionList);
  delete reflectionList;
}
