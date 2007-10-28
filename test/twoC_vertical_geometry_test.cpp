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
  _lattice.alpha().set_current(90 * HKL_DEGTORAD);
  _lattice.beta().set_current(90 * HKL_DEGTORAD);
  _lattice.gamma().set_current(90 * HKL_DEGTORAD);
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
  double omega = 10 * HKL_DEGTORAD;
  double two_theta = 13 * HKL_DEGTORAD;

  hkl::twoC::vertical::Geometry geometry_ref;
  hkl::twoC::vertical::Geometry geometry(omega, two_theta);

  //set the right current value but the wrong consign
  geometry_ref.get_axe("omega")->set_current(omega);
  geometry_ref.get_axe("tth")->set_current(two_theta);
  CPPUNIT_ASSERT_EQUAL(false, geometry_ref == geometry);

  // now set the right consign
  geometry_ref.get_axe("omega")->set_consign(omega);
  geometry_ref.get_axe("tth")->set_consign(two_theta);
  CPPUNIT_ASSERT_EQUAL(geometry_ref, geometry);
}

void
GeometryTwoCTest::get_sample_quaternion(void)
{
  static hkl_quaternion q_ref = {{1./sqrt(2), 0, -1./sqrt(2), 0.}};
  hkl_quaternion q;

  hkl::axe::Rotation * omega = _geometry->omega();

  // check that the sample_quaternion was well update.
  omega->set_current(90 * HKL_DEGTORAD);
  _geometry->get_sample_quaternion(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));

  // but not the consign part.
  q_ref.data[0] = 1;
  q_ref.data[1] = 0;
  q_ref.data[2] = 0;
  q_ref.data[3] = 0;
  _geometry->get_sample_quaternion_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));

  // current and consign must be equal
  omega->set_consign(90 * HKL_DEGTORAD);
  q_ref.data[0] = 1./sqrt(2);
  q_ref.data[1] = 0;
  q_ref.data[2] = -1./sqrt(2);
  q_ref.data[3] = 0;
  _geometry->get_sample_quaternion(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
  _geometry->get_sample_quaternion_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}

void
GeometryTwoCTest::get_sample_rotation_matrix(void)
{
  static hkl_smatrix smatrix_I = {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}};
  static hkl_smatrix m_ref = {{{0., 0.,-1.},{0., 1., 0.},{1., 0., 0.}}};
  hkl_smatrix m;

  // test the current part
  hkl::axe::Rotation * omega = _geometry->omega();
  omega->set_current(90 * HKL_DEGTORAD);
  _geometry->get_sample_rotation_matrix(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
  _geometry->get_sample_rotation_matrix_consign(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&smatrix_I, &m));

  // test the consign part
  omega->set_consign(90 * HKL_DEGTORAD);
  _geometry->get_sample_rotation_matrix(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
  _geometry->get_sample_rotation_matrix_consign(&m);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, &m));
}

void
GeometryTwoCTest::get_Q(void)
{
  static hkl_svector svector_null = {{0, 0, 0}};
  static hkl_svector svector_X = {{1, 0, 0}};
  static hkl_svector q_ref = {{1./sqrt(2)-1., 0, sqrt(2.)/2.}};
  hkl_svector q;

  hkl::axe::Rotation * tth = _geometry->tth();
  tth->set_current(0. * HKL_DEGTORAD);
  tth->set_consign(0. * HKL_DEGTORAD);
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));

  // only change the current value
  tth->set_current(45. * HKL_DEGTORAD);
  _geometry->source.wave_length = HKL_TAU;
  _geometry->source.direction = svector_X;
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&q_ref, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&svector_null, &q));

  // and now the consign value
  tth->set_consign(45. * HKL_DEGTORAD);
  _geometry->get_Q(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&q_ref, &q));
  _geometry->get_Q_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&q_ref, &q));
}

void
GeometryTwoCTest::get_distance(void)
{
  hkl::twoC::vertical::Geometry g1(10 * HKL_DEGTORAD,
                                   40 * HKL_DEGTORAD);

  hkl::twoC::vertical::Geometry g2(11 * HKL_DEGTORAD,
                                   41 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);

  hkl::axe::Rotation * omega2 = g2.omega();
  hkl::axe::Rotation * tth2 = g2.tth();
  // change the current value
  omega2->set_current(10 * HKL_DEGTORAD);
  tth2->set_current(40 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(2. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);

  // change the consign value
  omega2->set_consign(10 * HKL_DEGTORAD);
  tth2->set_consign(40 * HKL_DEGTORAD);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance(g2), HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0. * HKL_DEGTORAD, g1.get_distance_consign(g2), HKL_EPSILON);
}

void
GeometryTwoCTest::compute_HKL(void)
{
  hkl_smatrix const * UB = _lattice.get_B();
  double h, k ,l;

  _geometry->source.wave_length = 1.54;

  _geometry->set_angles(30 * HKL_DEGTORAD, 60 * HKL_DEGTORAD);
  _geometry->compute_HKL(h, k, l, UB);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, h, HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, k, HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1, l, HKL_EPSILON);

  _geometry->set_angles_consign(30 * HKL_DEGTORAD, 60 * HKL_DEGTORAD);
  _geometry->compute_HKL_consign(h, k, l, UB);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, h, HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0, k, HKL_EPSILON);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(1, l, HKL_EPSILON);
}

void
GeometryTwoCTest::setFromGeometry(void)
{
  hkl::twoC::vertical::Geometry twoC;
  hkl::twoC::vertical::Geometry twoC_ref(10. * HKL_DEGTORAD,
                                         40. * HKL_DEGTORAD);

  //eulerian4C::Vertical
  hkl::eulerian4C::vertical::Geometry E4CV(10. * HKL_DEGTORAD,
                                           0. * HKL_DEGTORAD,
                                           0. * HKL_DEGTORAD,
                                           40. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(twoC.setFromGeometry(E4CV, true));
  CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

  //kappa4C::Vertical
  hkl::kappa4C::vertical::Geometry K4CV(50. * HKL_DEGTORAD, // alpha
                                        10. * HKL_DEGTORAD,
                                        0. * HKL_DEGTORAD,
                                        0. * HKL_DEGTORAD,
                                        40. * HKL_DEGTORAD);
  CPPUNIT_ASSERT_NO_THROW(twoC.setFromGeometry(K4CV, true));
  CPPUNIT_ASSERT_EQUAL(twoC_ref, twoC);

  //Kappa6C
  hkl::kappa6C::Geometry K6C(50. * HKL_DEGTORAD, // alpha
                             0. * HKL_DEGTORAD,
                             10. * HKL_DEGTORAD,
                             0. * HKL_DEGTORAD,
                             0. * HKL_DEGTORAD,
                             0. * HKL_DEGTORAD,
                             40. * HKL_DEGTORAD);
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
