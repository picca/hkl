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
#include <sstream>
#include "geometry_kappa6C_test.h"
#include "eulerian4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryKappa6CTest );

void
GeometryKappa6CTest::setUp(void)
{
  m_alpha = 50 * hkl::constant::math::degToRad;
  m_geometry = new hkl::kappa6C::Geometry(m_alpha);
}

void
GeometryKappa6CTest::tearDown(void)
{
  delete m_geometry;
}

void
GeometryKappa6CTest::equal(void)
{
  hkl::kappa6C::Geometry geometry(m_alpha);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryKappa6CTest::copyConstructor(void)
{
  hkl::kappa6C::Geometry geometry(*m_geometry);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryKappa6CTest::otherConstructors(void)
{
  double mu = 9 * hkl::constant::math::degToRad;
  double komega = 10 * hkl::constant::math::degToRad;
  double kappa = 11 * hkl::constant::math::degToRad;
  double kphi = 12 * hkl::constant::math::degToRad;

  double gamma =13 * hkl::constant::math::degToRad;
  double delta =14 * hkl::constant::math::degToRad;

  hkl::kappa6C::Geometry geometry_ref(m_alpha);
  hkl::kappa6C::Geometry geometry(m_alpha, mu, komega, kappa, kphi, gamma, delta);

  geometry_ref.get_axe("mu")->set_current(mu);
  geometry_ref.get_axe("komega")->set_current(komega);
  geometry_ref.get_axe("kappa")->set_current(kappa);
  geometry_ref.get_axe("kphi")->set_current(kphi);
  geometry_ref.get_axe("gamma")->set_current(gamma);
  geometry_ref.get_axe("delta")->set_current(delta);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry));

  geometry_ref.get_axe("mu")->set_consign(mu);
  geometry_ref.get_axe("komega")->set_consign(komega);
  geometry_ref.get_axe("kappa")->set_consign(kappa);
  geometry_ref.get_axe("kphi")->set_consign(kphi);
  geometry_ref.get_axe("gamma")->set_consign(gamma);
  geometry_ref.get_axe("delta")->set_consign(delta);
  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryKappa6CTest::get_sample_quaternion(void)
{
  m_geometry->get_axe("mu")->set_current(90 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, 0, 1./sqrt(2)), m_geometry->get_sample_quaternion());
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(), m_geometry->get_sample_quaternion_consign());

  m_geometry->get_axe("mu")->set_consign(90 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, 0, 1./sqrt(2)), m_geometry->get_sample_quaternion());
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, 0, 1./sqrt(2)), m_geometry->get_sample_quaternion_consign());
}

void
GeometryKappa6CTest::get_sample_rotation_matrix(void)
{
  m_geometry->get_axe("mu")->set_current(90. * hkl::constant::math::degToRad);
  hkl::smatrix M( 0.,-1., 0.,
                  1., 0., 0.,
                  0., 0., 1.);
  CPPUNIT_ASSERT_EQUAL(M, m_geometry->get_sample_rotation_matrix());
  CPPUNIT_ASSERT_EQUAL(hkl::smatrix(1,0,0,0,1,0,0,0,1), m_geometry->get_sample_rotation_matrix_consign());

  m_geometry->get_axe("mu")->set_consign(90. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(M, m_geometry->get_sample_rotation_matrix());
  CPPUNIT_ASSERT_EQUAL(M, m_geometry->get_sample_rotation_matrix_consign());
}

void
GeometryKappa6CTest::get_Q(void)
{
  m_geometry->get_axe("gamma")->set_current(0. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(0., 0., 0.), m_geometry->get_Q());

  m_geometry->get_source().setKi(hkl::svector(1, 0, 0));
  m_geometry->get_axe("gamma")->set_current(45. * hkl::constant::math::degToRad);
  m_geometry->get_axe("delta")->set_current(45. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(-.5, .5, sqrt(2.)/2.), m_geometry->get_Q());
  CPPUNIT_ASSERT_EQUAL(hkl::svector(), m_geometry->get_Q_consign());

  m_geometry->get_axe("gamma")->set_consign(45. * hkl::constant::math::degToRad);
  m_geometry->get_axe("delta")->set_consign(45. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(-.5, .5, sqrt(2.)/2.), m_geometry->get_Q());
  CPPUNIT_ASSERT_EQUAL(hkl::svector(-.5, .5, sqrt(2.)/2.), m_geometry->get_Q_consign());
}

void
GeometryKappa6CTest::get_distance(void)
{
  hkl::kappa6C::Geometry g1(m_alpha,
                            10 * hkl::constant::math::degToRad,
                            20 * hkl::constant::math::degToRad,
                            30 * hkl::constant::math::degToRad,
                            40 * hkl::constant::math::degToRad,
                            50 * hkl::constant::math::degToRad,
                            60 * hkl::constant::math::degToRad);

  hkl::kappa6C::Geometry g2(m_alpha,
                            11 * hkl::constant::math::degToRad,
                            21 * hkl::constant::math::degToRad,
                            31 * hkl::constant::math::degToRad,
                            41 * hkl::constant::math::degToRad,
                            51 * hkl::constant::math::degToRad,
                            61 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_DOUBLES_EQUAL(6. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(6. * hkl::constant::math::degToRad, g1.get_distance_consign(g2), hkl::constant::math::epsilon);

  g2.set_angles(10 * hkl::constant::math::degToRad,
                20 * hkl::constant::math::degToRad,
                30 * hkl::constant::math::degToRad,
                40 * hkl::constant::math::degToRad,
                50 * hkl::constant::math::degToRad,
                60 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(6. * hkl::constant::math::degToRad, g1.get_distance_consign(g2), hkl::constant::math::epsilon);

  g2.set_angles_consign(10 * hkl::constant::math::degToRad,
                        20 * hkl::constant::math::degToRad,
                        30 * hkl::constant::math::degToRad,
                        40 * hkl::constant::math::degToRad,
                        50 * hkl::constant::math::degToRad,
                        60 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.get_distance_consign(g2), hkl::constant::math::epsilon);
}

void
GeometryKappa6CTest::setFromGeometry(void)
{
  hkl::kappa6C::Geometry K6C(m_alpha);
  hkl::kappa6C::Geometry K6C_ref(m_alpha,
                                 0. * hkl::constant::math::degToRad,
                                 0. * hkl::constant::math::degToRad,
                                 0. * hkl::constant::math::degToRad,
                                 0. * hkl::constant::math::degToRad,
                                 0. * hkl::constant::math::degToRad,
                                 40. * hkl::constant::math::degToRad);

  //eulerian4C::Vertical
  hkl::eulerian4C::vertical::Geometry E4CV(-90. * hkl::constant::math::degToRad,
                                           0. * hkl::constant::math::degToRad,
                                           90. * hkl::constant::math::degToRad,
                                           40. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E4CV, true));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E4CV, false));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);

  E4CV.get_axe("chi")->set_current(110 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E4CV, true), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E4CV, false), hkl::HKLException);

  //Eulerian6C
  hkl::eulerian6C::Geometry E6C(0. * hkl::constant::math::degToRad,
                                -90. * hkl::constant::math::degToRad,
                                0. * hkl::constant::math::degToRad,
                                90. * hkl::constant::math::degToRad,
                                0. * hkl::constant::math::degToRad,
                                40. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E6C, true));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);
  CPPUNIT_ASSERT_NO_THROW(K6C.setFromGeometry(E6C, false));
  CPPUNIT_ASSERT_EQUAL(K6C_ref, K6C);

  E6C.get_axe("chi")->set_current(110 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E6C, true), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(K6C.setFromGeometry(E6C, false), hkl::HKLException);
}

void
GeometryKappa6CTest::persistanceIO(void)
{
  hkl::kappa6C::Geometry geometry1(m_alpha);
  hkl::kappa6C::Geometry geometry2(m_alpha);
  std::stringstream flux;

  m_geometry->get_axe("komega")->set_current(2.);
  m_geometry->toStream(flux);
  m_geometry->get_axe("komega")->set_current(3.);
  m_geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  m_geometry->get_axe("komega")->set_current(2.);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry1);
  m_geometry->get_axe("komega")->set_current(3.);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry2);
}
