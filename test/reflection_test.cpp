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
#include "reflection_test.h"
#include "reflection_monocrystal.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ReflectionTest );

void
ReflectionTest::setUp(void)
{
  _geometry = new hkl::eulerian4C::vertical::Geometry(1, 2, 3, 1);
  _geometry->get_source().setKi(hkl::svector(1., 0., 0.));
}

void
ReflectionTest::tearDown(void)
{
  delete _geometry;
}

void
ReflectionTest::Constructor(void)
{
  hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(1., 0., 0.), true);

  CPPUNIT_ASSERT_EQUAL(hkl::svector(1., 0., 0.), reflection->get_hkl());
  CPPUNIT_ASSERT_EQUAL(true, reflection->flag());

  delete reflection;
}

void
ReflectionTest::Equal(void)
{
  hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(1., 0., 0.), true);
  CPPUNIT_ASSERT_EQUAL(*reflection, *reflection);
  delete reflection;
}

void
ReflectionTest::GetSet(void)
{
  hkl::Reflection  * reflection = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(1., 0., 0.), true);

  reflection->set_hkl(hkl::svector(1.5, 1.5, 1.5));
  CPPUNIT_ASSERT_EQUAL(hkl::svector(1.5, 1.5, 1.5), reflection->get_hkl());

  reflection->flag() = false;
  CPPUNIT_ASSERT_EQUAL(false, reflection->flag());
  delete reflection;
}

void
ReflectionTest::GetHKL(void)
{
  hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(1., 0., 0.), true);
  hkl::svector vref(1., 0., 0.);

  CPPUNIT_ASSERT_EQUAL(vref, reflection->get_hkl());

  delete reflection;
}

void
ReflectionTest::ComputeAngle(void)
{
  double angle;
  const hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(1., 0., 0.), true);
  const hkl::Reflection * reflection1 = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(1., 1., .5), true);

  angle = reflection->computeAngle(hkl::svector(1., 0., 0.)).get_value();
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., angle, hkl::constant::math::epsilon);

  angle = reflection->computeAngle(hkl::svector(1., 1., 0.)).get_value();
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./sqrt(2.)), angle, hkl::constant::math::epsilon);

  angle = reflection1->computeAngle(hkl::svector(1, .5, -1.)).get_value();
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./2.25), angle, hkl::constant::math::epsilon);

  delete reflection;
  delete reflection1;
}

void
ReflectionTest::isColinear(void)
{
  hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(1., 0., 0.), true);
  hkl::Reflection * reflection1 = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(2., 0., 0.), true);
  hkl::Reflection * reflection2 = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(1., 1., .5), true);

  CPPUNIT_ASSERT_EQUAL(true, reflection->isColinear(*reflection));
  CPPUNIT_ASSERT_EQUAL(true, reflection->isColinear(*reflection1));
  CPPUNIT_ASSERT_EQUAL(false, reflection->isColinear(*reflection2));

  delete reflection;
  delete reflection1;
  delete reflection2;
}

void
ReflectionTest::persistanceIO(void)
{
  hkl::Reflection * reflection_ref = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(1., 0., 0.), true);
  hkl::Reflection * reflection1_ref = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(2., 0., 0.), true);
  hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(0, 0, 0), true);
  hkl::Reflection * reflection1 = new hkl::reflection::MonoCrystal(*_geometry, hkl::svector(0, 0, 0), true);
  std::stringstream flux;

  reflection_ref->toStream(flux);
  reflection1_ref->toStream(flux);
  reflection->fromStream(flux);
  reflection1->fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*reflection_ref, *reflection);
  CPPUNIT_ASSERT_EQUAL(*reflection1_ref, *reflection1);

  delete reflection_ref;
  delete reflection1_ref;
  delete reflection;
  delete reflection1;
}
