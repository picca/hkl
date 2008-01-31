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
#include "samplelist_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( SampleListTest );

void
SampleListTest::setUp(void)
{
  _sampleList = new hkl::SampleList(_geometry);
}

void
SampleListTest::tearDown(void)
{
  delete _sampleList;
}

void
SampleListTest::operators(void)
{
  // ==
  CPPUNIT_ASSERT_EQUAL(*_sampleList, *_sampleList);

  CPPUNIT_ASSERT_EQUAL((unsigned int)0, _sampleList->size());
  //CPPUNIT_ASSERT_THROW((*_sampleList)[0], HKLException);
  CPPUNIT_ASSERT_NO_THROW(_sampleList->add("Mono-Crystal", hkl::SAMPLE_MONOCRYSTAL));
  CPPUNIT_ASSERT_EQUAL((unsigned int)1, _sampleList->size());
  CPPUNIT_ASSERT_NO_THROW((*_sampleList)["Mono-Crystal"]);

  //erase
  std::vector<hkl::Sample *>::iterator iter = _sampleList->begin();
  CPPUNIT_ASSERT_NO_THROW(_sampleList->erase(iter));
}

void
SampleListTest::persistanceIO(void)
{
  hkl::SampleList * sampleList = new hkl::SampleList(_geometry);
  std::stringstream flux;

  _sampleList->add("Mono-Crystal", hkl::SAMPLE_MONOCRYSTAL);
  _sampleList->add("CuGeO3", hkl::SAMPLE_MONOCRYSTAL);
  _sampleList->toStream(flux);

  sampleList->fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*_sampleList, *sampleList);
  delete sampleList;
}
