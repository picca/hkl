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
// File to test matrix and vector implementation.
#include "lattice_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( LatticeTest );

void
LatticeTest::setUp(void) {}

void
LatticeTest::tearDown(void) {}

void
LatticeTest::constructors(void)
{
  // default constructor
  CPPUNIT_ASSERT_EQUAL(1.54, _lattice.a().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(1.54, _lattice.b().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(1.54, _lattice.c().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(90. * hkl::constant::math::degToRad, _lattice.alpha().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(90. * hkl::constant::math::degToRad, _lattice.beta().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(90. * hkl::constant::math::degToRad, _lattice.gamma().get_current().get_value());

  // copy constructor
  _lattice.a().set_current(2.54);
  hkl::Lattice lattice(_lattice);

  CPPUNIT_ASSERT_EQUAL(2.54, lattice.a().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(1.54, lattice.b().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(1.54, lattice.c().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(90. * hkl::constant::math::degToRad, lattice.alpha().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(90. * hkl::constant::math::degToRad, lattice.beta().get_current().get_value());
  CPPUNIT_ASSERT_EQUAL(90. * hkl::constant::math::degToRad, lattice.gamma().get_current().get_value());
}

void
LatticeTest::equal(void)
{
  hkl::Lattice lattice(_lattice);
  CPPUNIT_ASSERT_EQUAL(_lattice, lattice);
}

void
LatticeTest::reciprocal(void)
{
  hkl::Lattice reciprocal;

  hkl::FitParameter & a = _lattice.a();
  hkl::FitParameter & b = _lattice.b();
  hkl::FitParameter & c = _lattice.c();
  hkl::FitParameter & alpha = _lattice.alpha();
  hkl::FitParameter & beta = _lattice.beta();
  hkl::FitParameter & gamma = _lattice.gamma();

  hkl::FitParameter & a_star = reciprocal.a();
  hkl::FitParameter & b_star = reciprocal.b();
  hkl::FitParameter & c_star = reciprocal.c();
  hkl::FitParameter & alpha_star = reciprocal.alpha();
  hkl::FitParameter & beta_star = reciprocal.beta();
  hkl::FitParameter & gamma_star = reciprocal.gamma();

  // cubic
  a.set_current(1.54);
  b.set_current(1.54);
  c.set_current(1.54);
  alpha.set_current(90 * hkl::constant::math::degToRad);
  beta.set_current(90 * hkl::constant::math::degToRad);
  gamma.set_current(90 * hkl::constant::math::degToRad);

  a_star.set_current(hkl::constant::physic::tau / 1.54);
  b_star.set_current(hkl::constant::physic::tau / 1.54);
  c_star.set_current(hkl::constant::physic::tau / 1.54);
  alpha_star.set_current(90 * hkl::constant::math::degToRad);
  beta_star.set_current(90 * hkl::constant::math::degToRad);
  gamma_star.set_current(90 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(reciprocal, _lattice.reciprocal());

  //orthorombic
  a.set_current(1.);
  b.set_current(3.);
  c.set_current(4.);
  alpha.set_current(90 * hkl::constant::math::degToRad);
  beta.set_current(90 * hkl::constant::math::degToRad);
  gamma.set_current(90 * hkl::constant::math::degToRad);

  a_star.set_current(hkl::constant::physic::tau / 1.);
  b_star.set_current(hkl::constant::physic::tau / 3.);
  c_star.set_current(hkl::constant::physic::tau / 4.);
  alpha_star.set_current(90 * hkl::constant::math::degToRad);
  beta_star.set_current(90 * hkl::constant::math::degToRad);
  gamma_star.set_current(90 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(reciprocal, _lattice.reciprocal());

  // hexagonal1
  a.set_current(1.);
  b.set_current(2.);
  c.set_current(1.);
  alpha.set_current(90 * hkl::constant::math::degToRad);
  beta.set_current(120 * hkl::constant::math::degToRad);
  gamma.set_current(90 * hkl::constant::math::degToRad);

  a_star.set_current(hkl::constant::physic::tau * 2./sqrt(3));
  b_star.set_current(hkl::constant::physic::tau / 2.);
  c_star.set_current(hkl::constant::physic::tau * 2./sqrt(3));
  alpha_star.set_current(90 * hkl::constant::math::degToRad);
  beta_star.set_current(60 * hkl::constant::math::degToRad);
  gamma_star.set_current(90 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(reciprocal, _lattice.reciprocal());

  // hexagonal2
  a.set_current(2.);
  b.set_current(1.);
  c.set_current(1.);
  alpha.set_current(120 * hkl::constant::math::degToRad);
  beta.set_current(90 * hkl::constant::math::degToRad);
  gamma.set_current(90 * hkl::constant::math::degToRad);

  a_star.set_current(hkl::constant::physic::tau / 2.);
  b_star.set_current(hkl::constant::physic::tau * 2./sqrt(3));
  c_star.set_current(hkl::constant::physic::tau * 2./sqrt(3));
  alpha_star.set_current(60 * hkl::constant::math::degToRad);
  beta_star.set_current(90 * hkl::constant::math::degToRad);
  gamma_star.set_current(90 * hkl::constant::math::degToRad);

  CPPUNIT_ASSERT_EQUAL(reciprocal, _lattice.reciprocal());

  // triclinic1
  a.set_current(9.32);
  b.set_current(8.24);
  c.set_current(13.78);
  alpha.set_current(91.23 * hkl::constant::math::degToRad);
  beta.set_current(93.64 * hkl::constant::math::degToRad);
  gamma.set_current(122.21 * hkl::constant::math::degToRad);

  a_star.set_current(hkl::constant::physic::tau * 0.1273130168);
  b_star.set_current(hkl::constant::physic::tau * 0.1437422974);
  c_star.set_current(hkl::constant::physic::tau * 0.0728721120);
  alpha_star.set_current(1.5052513337);
  beta_star.set_current(1.482101482);
  gamma_star.set_current(1.0055896011);

  CPPUNIT_ASSERT_EQUAL(reciprocal, _lattice.reciprocal());

  // triclinic2
  a.set_current(18.423);
  b.set_current(18.417);
  c.set_current(18.457);
  alpha.set_current(89.99 * hkl::constant::math::degToRad);
  beta.set_current(89.963 * hkl::constant::math::degToRad);
  gamma.set_current(119.99 * hkl::constant::math::degToRad);

  a_star.set_current(hkl::constant::physic::tau * 0.0626708259);
  b_star.set_current(hkl::constant::physic::tau * 0.0626912310);
  c_star.set_current(hkl::constant::physic::tau * 0.0541800061);
  alpha_star.set_current(1.5713705262);
  beta_star.set_current(1.5716426508);
  gamma_star.set_current(1.0473718249);

  CPPUNIT_ASSERT_EQUAL(reciprocal, _lattice.reciprocal());
}

void
LatticeTest::get_B(void)
{
  hkl::Lattice reciprocal;

  hkl::FitParameter & a = _lattice.a();
  hkl::FitParameter & b = _lattice.b();
  hkl::FitParameter & c = _lattice.c();
  hkl::FitParameter & alpha = _lattice.alpha();
  hkl::FitParameter & beta = _lattice.beta();
  hkl::FitParameter & gamma = _lattice.gamma();

  // cubic
  a.set_current(1.54);
  b.set_current(1.54);
  c.set_current(1.54);
  alpha.set_current(90 * hkl::constant::math::degToRad);
  beta.set_current(90 * hkl::constant::math::degToRad);
  gamma.set_current(90 * hkl::constant::math::degToRad);

  hkl::smatrix m(hkl::constant::physic::tau / 1.54,                                 0,                                 0,
                 0, hkl::constant::physic::tau / 1.54,                                 0,
                 0,                                 0, hkl::constant::physic::tau / 1.54);
  CPPUNIT_ASSERT_EQUAL(m, _lattice.get_B());

  //exception
  alpha.set_current(90 * hkl::constant::math::degToRad);
  beta.set_current(10 * hkl::constant::math::degToRad);
  gamma.set_current(120 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_THROW(_lattice.get_B(), HKLException);
}

void
LatticeTest::randomize(void)
{
  // no exception
  for (unsigned int i=0;i<100;i++)
    CPPUNIT_ASSERT_NO_THROW(_lattice.randomize());
}

void
LatticeTest::persistanceIO(void)
{
  hkl::Lattice lattice;
  lattice.a().set_current(1);
  lattice.b().set_current(1);
  lattice.c().set_current(1);
  lattice.alpha().set_current(1);
  lattice.beta().set_current(1);
  lattice.gamma().set_current(1);
  std::stringstream flux;

  _lattice.toStream(flux);
  lattice.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(_lattice, lattice);
}
