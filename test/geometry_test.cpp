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
#include "geometry_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryTest );

void
GeometryTest::setUp(void)
{
  m_geometry = new hkl::Geometry("test", "test");
}

void
GeometryTest::tearDown(void)
{
  delete m_geometry;
}

void
GeometryTest::equal(void)
{
  CPPUNIT_ASSERT_EQUAL(m_geometry, m_geometry);
}

void
GeometryTest::copyConstructor(void)
{
  hkl::Geometry geometry(*m_geometry);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryTest::operateurs(void)
{
  /*
  hkl::Axe * Omega = new hkl::Axe("omega", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * Gamma = new hkl::Axe("gamma", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);

  // on verifie que les exceptions sont bien lancees lorsque
  // l'on recherche un axe qui n'existe pas.
  CPPUNIT_ASSERT_THROW(m_geometry->get_axe("toto"), hkl::HKLException);

  // et que tout se passe bien le cas contraire.
  m_geometry->add_sample_axe(Omega);
  m_geometry->add_sample_axe(Gamma);
  CPPUNIT_ASSERT_NO_THROW(m_geometry->get_axe("omega"));
  CPPUNIT_ASSERT_NO_THROW(m_geometry->get_axe("gamma"));

  // On verifie que les valeurs retourn�es sont les bonnes.
  CPPUNIT_ASSERT_EQUAL(Omega, m_geometry->get_axe("omega"));
  CPPUNIT_ASSERT_EQUAL(Gamma, m_geometry->get_axe("gamma"));
  */
  //Test de l'assignation (memory leak)
  /*
  hkl::Geometry geometry = *m_geometry;
  geometry.add_sample_axe(hkl::Axe("delta", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi));
  geometry = *m_geometry;
  CPPUNIT_ASSERT_EQUAL(geometry, *m_geometry);
  */
  /*
    delete Omega;
    delete Gamma;
  */
}

void
GeometryTest::persistanceIO(void)
{
  hkl::Geometry geometry1("1", "1");
  hkl::Geometry geometry2("2", "2");
  std::stringstream flux;

  m_geometry->toStream(flux);
  m_geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry1);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry2);
}
