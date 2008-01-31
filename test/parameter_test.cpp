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
#include "parameter_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ParameterTest );

void
ParameterTest::setUp(void) {}

void
ParameterTest::tearDown(void) {}

void
ParameterTest::constructors(void)
{
  CPPUNIT_ASSERT_THROW(Parameter("", "", 2, 1, 3), HKLException);
  CPPUNIT_ASSERT_THROW(Parameter("", "coucou", 2, 1, 3), HKLException);
  CPPUNIT_ASSERT_THROW(Parameter("toto", "coucou", 2, 1, 3), HKLException);
  CPPUNIT_ASSERT_NO_THROW(Parameter("toto", "coucou", 1, 2, 3));

  Parameter parameter_ref("toto", "coucou", 1, 2, 3);
  Parameter parameter(parameter_ref);
  CPPUNIT_ASSERT_EQUAL(parameter_ref, parameter);
}

void
ParameterTest::persistanceIO(void)
{
  Parameter parameter_ref("ca le fait grave", "de la balle je vous le dit\ncoucou", -7.432165432, 1.34e-32, 8.);
  Parameter parameter("titi", "tutu", 1., 2, 3);
  Parameter parameter1_ref("another object", "with a nice description", 2, 3, 4);
  Parameter parameter1(parameter);

  std::stringstream flux;
  parameter_ref.toStream(flux);
  parameter1_ref.toStream(flux);
  parameter.fromStream(flux);
  parameter1.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(parameter_ref, parameter);
  CPPUNIT_ASSERT_EQUAL(parameter1_ref, parameter1);
}
