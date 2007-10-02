#include "config.h"
#include "reflection_test.h"
#include "reflection_monocrystal.h"

CPPUNIT_TEST_SUITE_REGISTRATION( ReflectionTest );

void
ReflectionTest::setUp(void)
{
  static hkl_svector svector_X = {{1,0,0}};

  _geometry = new hkl::eulerian4C::vertical::Geometry(1, 2, 3, 1);
  _geometry->source.wave_length = HKL_TAU;
  _geometry->source.direction = svector_X;
}

void
ReflectionTest::tearDown(void)
{
  delete _geometry;
}

void
ReflectionTest::Constructor(void)
{
  static hkl_svector hkl_ref = {{1,0,0}};
  hkl_svector const * hkl;

  hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, &hkl_ref, true);
  hkl = reflection->get_hkl();
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&hkl_ref, hkl));
  CPPUNIT_ASSERT_EQUAL(true, reflection->flag());
  delete reflection;
}

void
ReflectionTest::Equal(void)
{
  static hkl_svector hkl_ref = {{1,0,0}};

  hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, &hkl_ref, true);
  CPPUNIT_ASSERT_EQUAL(*reflection, *reflection);
  delete reflection;
}

void
ReflectionTest::GetSet(void)
{
  static hkl_svector svector_X = {{1,0,0}};
  static hkl_svector hkl_ref = {{1.5, 1.5, 1.5}};
  hkl_svector const * hkl;

  hkl::Reflection  * reflection = new hkl::reflection::MonoCrystal(*_geometry, &svector_X, true);
  reflection->set_hkl(&hkl_ref);
  hkl = reflection->get_hkl();
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&hkl_ref, hkl));
  reflection->flag() = false;
  CPPUNIT_ASSERT_EQUAL(false, reflection->flag());
  delete reflection;
}

void
ReflectionTest::GetHKL(void)
{
  static hkl_svector hkl_ref = {{1,0,0}};
  hkl_svector const * hkl;

  hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, &hkl_ref, true);
  hkl = reflection->get_hkl();
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_svector_cmp(&hkl_ref, hkl));
  delete reflection;
}

void
ReflectionTest::ComputeAngle(void)
{
  static hkl_svector hkl1 = {{1,0,0}};
  static hkl_svector hkl2 = {{1,1,.5}};
  static hkl_svector hkl3 = {{1,1,0}};
  static hkl_svector hkl4 = {{1,.5,-1}};

  double angle;
  const hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, &hkl1, true);
  const hkl::Reflection * reflection1 = new hkl::reflection::MonoCrystal(*_geometry, &hkl2, true);

  angle = reflection->computeAngle(&hkl1).get_value();
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., angle, HKL_EPSILON);

  angle = reflection->computeAngle(&hkl3).get_value();
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./sqrt(2.)), angle, HKL_EPSILON);

  angle = reflection1->computeAngle(&hkl4).get_value();
  CPPUNIT_ASSERT_DOUBLES_EQUAL(acos(1./2.25), angle, HKL_EPSILON);

  delete reflection;
  delete reflection1;
}

void
ReflectionTest::isColinear(void)
{
  static hkl_svector hkl1 = {{1,0,0}};
  static hkl_svector hkl2 = {{2,0,0}};
  static hkl_svector hkl3 = {{1,1,.5}};

  hkl::Reflection * reflection = new hkl::reflection::MonoCrystal(*_geometry, &hkl1, true);
  hkl::Reflection * reflection1 = new hkl::reflection::MonoCrystal(*_geometry, &hkl2, true);
  hkl::Reflection * reflection2 = new hkl::reflection::MonoCrystal(*_geometry, &hkl3, true);

  CPPUNIT_ASSERT_EQUAL(true, reflection->isColinear(*reflection));
  CPPUNIT_ASSERT_EQUAL(true, reflection->isColinear(*reflection1));
  CPPUNIT_ASSERT_EQUAL(false, reflection->isColinear(*reflection2));

  delete reflection;
  delete reflection1;
  delete reflection2;
}
