#include "geometry_twoC_test.h"
#include "eulerian4C_vertical_geometry.h"
#include "kappa4C_vertical_geometry.h"
#include "kappa6C_geometry.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryTwoCTest );

void
GeometryTwoCTest::setUp(void)
{
  _geometry = new hkl::twoC::vertical::Geometry;
  _lattice.a().set_current(1.54);
  _lattice.b().set_current(1.54);
  _lattice.c().set_current(1.54);
  _lattice.alpha().set_current(90 * hkl::constant::math::degToRad);
  _lattice.beta().set_current(90 * hkl::constant::math::degToRad);
  _lattice.gamma().set_current(90 * hkl::constant::math::degToRad);
}

void
GeometryTwoCTest::tearDown(void)
{
  delete _geometry;
}

void
GeometryTwoCTest::equal(void)
{
  CPPUNIT_ASSERT_EQUAL(*_geometry, *_geometry);
}

void
GeometryTwoCTest::copyConstructor(void)
{
  hkl::twoC::vertical::Geometry geometry(*_geometry);

  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry);
}

void
GeometryTwoCTest::otherConstructors(void)
{
  double omega = 10 * hkl::constant::math::degToRad;
  double two_theta = 13 * hkl::constant::math::degToRad;

  hkl::twoC::vertical::Geometry geometry_ref;
  hkl::twoC::vertical::Geometry geometry(omega, two_theta);

  //set the right current value but the wrong consign
  geometry_ref.get_axe("omega")->set_current(omega);
  geometry_ref.get_axe("tth")->set_current(two_theta);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry));

  // now set the right consign
  geometry_ref.get_axe("omega")->set_consign(omega);
  geometry_ref.get_axe("tth")->set_consign(two_theta);
  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryTwoCTest::get_sample_quaternion(void)
{
  hkl::axe::Rotation * omega = _geometry->omega();

  omega->set_current(90 * hkl::constant::math::degToRad);
  // check that the sample_quaternion was well update.
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, -1./sqrt(2), 0.), _geometry->get_sample_quaternion());
  // but not the consign part.
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(), _geometry->get_sample_quaternion_consign());

  omega->set_consign(90 * hkl::constant::math::degToRad);
  // current and consign must be equal
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, -1./sqrt(2), 0.), _geometry->get_sample_quaternion());
  CPPUNIT_ASSERT_EQUAL(hkl::Quaternion(1./sqrt(2), 0, -1./sqrt(2), 0.), _geometry->get_sample_quaternion_consign());
}

void
GeometryTwoCTest::get_sample_rotation_matrix(void)
{
  hkl::axe::Rotation * omega = _geometry->omega();
  omega->set_current(90 * hkl::constant::math::degToRad);
  hkl::smatrix M(0., 0.,-1.,
                 0., 1., 0.,
                 1., 0., 0.);
  CPPUNIT_ASSERT_EQUAL(M, _geometry->get_sample_rotation_matrix());
  CPPUNIT_ASSERT_EQUAL(hkl::smatrix(1, 0, 0, 0, 1, 0, 0, 0, 1), _geometry->get_sample_rotation_matrix_consign());

  omega->set_consign(90 * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(M, _geometry->get_sample_rotation_matrix());
  CPPUNIT_ASSERT_EQUAL(M, _geometry->get_sample_rotation_matrix_consign());
}

void
GeometryTwoCTest::get_Q(void)
{
  hkl::axe::Rotation * tth = _geometry->tth();
  tth->set_current(0. * hkl::constant::math::degToRad);
  tth->set_consign(0. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(0., 0., 0.), _geometry->get_Q());
  CPPUNIT_ASSERT_EQUAL(hkl::svector(0., 0., 0.), _geometry->get_Q_consign());

  // only change the current value
  tth->set_current(45. * hkl::constant::math::degToRad);
  _geometry->get_source().setKi(hkl::svector(1, 0, 0));
  CPPUNIT_ASSERT_EQUAL(hkl::svector(1./sqrt(2)-1., 0, sqrt(2.)/2.), _geometry->get_Q());
  CPPUNIT_ASSERT_EQUAL(hkl::svector(0., 0., 0.), _geometry->get_Q_consign());

  // and now the consign value
  tth->set_consign(45. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_EQUAL(hkl::svector(1./sqrt(2)-1., 0, sqrt(2.)/2.), _geometry->get_Q());
  CPPUNIT_ASSERT_EQUAL(hkl::svector(1./sqrt(2)-1., 0, sqrt(2.)/2.), _geometry->get_Q_consign());
}

void
GeometryTwoCTest::get_distance(void)
{
    hkl::twoC::vertical::Geometry g1(10 * hkl::constant::math::degToRad,
                                     40 * hkl::constant::math::degToRad);

    hkl::twoC::vertical::Geometry g2(11 * hkl::constant::math::degToRad,
                                     41 * hkl::constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2. * hkl::constant::math::degToRad, g1.get_distance_consign(g2), hkl::constant::math::epsilon);

    hkl::axe::Rotation * omega2 = g2.omega();
    hkl::axe::Rotation * tth2 = g2.tth();
    // change the current value
    omega2->set_current(10 * hkl::constant::math::degToRad);
    tth2->set_current(40 * hkl::constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(2. * hkl::constant::math::degToRad, g1.get_distance_consign(g2), hkl::constant::math::epsilon);

    // change the consign value
    omega2->set_consign(10 * hkl::constant::math::degToRad);
    tth2->set_consign(40 * hkl::constant::math::degToRad);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.get_distance(g2), hkl::constant::math::epsilon);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * hkl::constant::math::degToRad, g1.get_distance_consign(g2), hkl::constant::math::epsilon);
}

void
GeometryTwoCTest::computeHKL(void)
{
  hkl::smatrix UB = _lattice.get_B();
  double h, k ,l;

  _geometry->get_source().setWaveLength(1.54);
  _geometry->set_angles(30 * hkl::constant::math::degToRad, 60 * hkl::constant::math::degToRad);
  _geometry->computeHKL(h, k, l, UB);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, h, hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, k, hkl::constant::math::epsilon);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1, l, hkl::constant::math::epsilon);
}

void
GeometryTwoCTest::setFromGeometry(void)
{
  hkl::twoC::vertical::Geometry twoC;
  hkl::twoC::vertical::Geometry twoC_ref(10. * hkl::constant::math::degToRad,
                                         40. * hkl::constant::math::degToRad);

  //eulerian4C::Vertical
  hkl::eulerian4C::vertical::Geometry E4CV(10. * hkl::constant::math::degToRad,
                                           0. * hkl::constant::math::degToRad,
                                           0. * hkl::constant::math::degToRad,
                                           40. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(twoC.setFromGeometry(E4CV, true));
  CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

  //kappa4C::Vertical
  hkl::kappa4C::vertical::Geometry K4CV(50. * hkl::constant::math::degToRad, // alpha
                                        10. * hkl::constant::math::degToRad,
                                        0. * hkl::constant::math::degToRad,
                                        0. * hkl::constant::math::degToRad,
                                        40. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(twoC.setFromGeometry(K4CV, true));
  CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

  //Kappa6C
  hkl::kappa6C::Geometry K6C(50. * hkl::constant::math::degToRad, // alpha
                             0. * hkl::constant::math::degToRad,
                             10. * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             0. * hkl::constant::math::degToRad,
                             40. * hkl::constant::math::degToRad);
  CPPUNIT_ASSERT_NO_THROW(twoC.setFromGeometry(K6C, true));
  CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

  // exceptions
  // eulerian4C
  E4CV.get_axe("chi")->set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(E4CV, true), hkl::HKLException);
  // kappa4C
  K4CV.get_axe("kappa")->set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K4CV, true), hkl::HKLException);
  // kappa6C
  K6C.get_axe("mu")->set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K6C, true), hkl::HKLException);
  K6C.get_axe("mu")->set_current(0.);
  K6C.get_axe("gamma")->set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K6C, true), hkl::HKLException);
  K6C.get_axe("mu")->set_current(1.);
  CPPUNIT_ASSERT_THROW(twoC.setFromGeometry(K6C, true), hkl::HKLException);
}

void
GeometryTwoCTest::persistanceIO(void)
{
  hkl::twoC::vertical::Geometry geometry1;
  hkl::twoC::vertical::Geometry geometry2;
  std::stringstream flux;

  _geometry->toStream(flux);
  _geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry1);
  CPPUNIT_ASSERT_EQUAL(*_geometry, geometry2);
}
