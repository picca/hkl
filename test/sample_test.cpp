#include "sample_test.h"
#include "reflectionlist.h"

CPPUNIT_TEST_SUITE_REGISTRATION( SampleTest );

void
SampleTest::setUp(void)
{
  _sample = new hkl::sample::MonoCrystal(_geometry, "Crystal");
  Lattice & lattice = _sample->lattice();
  lattice.a().set_current(1.54);
  lattice.b().set_current(1.54);
  lattice.c().set_current(1.54);
  lattice.alpha().set_current(90 * HKL_DEGTORAD);
  lattice.beta().set_current(90 * HKL_DEGTORAD);
  lattice.gamma().set_current(90 * HKL_DEGTORAD);

  _geometry.get_source().setWaveLength(1.54);
}

void
SampleTest::tearDown(void)
{
  delete _sample;
}

void
SampleTest::Constructor()
{
  static hkl_smatrix m_I = {{{1,0,0}, {0,1,0}, {0, 0, 1}}};

  CPPUNIT_ASSERT_EQUAL(std::string("Crystal"), _sample->get_name());
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_I, dynamic_cast<hkl::sample::MonoCrystal *>(_sample)->get_U()));
}

void
SampleTest::Equal()
{
  static hkl_svector hkl = {{1, 1, 1}};
  hkl::Sample * sample = new hkl::sample::MonoCrystal(_geometry, "toto");

  sample->reflections().add(&hkl);

  CPPUNIT_ASSERT_EQUAL(*_sample, *_sample);
  CPPUNIT_ASSERT_EQUAL(*sample, *sample);
  CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(*_sample, *sample));

  delete sample;
}

void
SampleTest::clone()
{
  hkl::Sample * sample = _sample->clone();

  CPPUNIT_ASSERT_EQUAL(*_sample, *sample);

  delete sample;
}

void
SampleTest::ComputeU()
{
  static hkl_svector hkl_null = {{0,0,0}};
  static hkl_svector hkl1 = {{0,0,1}};
  static hkl_svector hkl2 = {{-1,0,0}};
  static hkl_svector hkl3 = {{1,0,0}};
  static hkl_svector hkl4 = {{0,1,0}};
  static hkl_smatrix m_I = {{{1,0,0}, {0,1,0}, {0, 0, 1}}};
  static hkl_smatrix m_ref = {{{1., 0., 0.}, {0., 0., 1.}, {0.,-1., 0.}}};

  hkl::sample::MonoCrystal & sample = dynamic_cast<hkl::sample::MonoCrystal &>(*_sample);

  // without reflection an exception is throw
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  // add a non valid for computation reflection
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(&hkl_null));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  // add a second non valid for computation reflection
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(&hkl_null));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  CPPUNIT_ASSERT_NO_THROW(sample.reflections().del(1));
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().del(0));

  //with only one valid reflection exception
  CPPUNIT_ASSERT_NO_THROW(_geometry.set_angles(30.* HKL_DEGTORAD,
                                               0.* HKL_DEGTORAD,
                                               0.* HKL_DEGTORAD,
                                               60.* HKL_DEGTORAD));
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(&hkl1));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  //with two valid reflection, no exception
  CPPUNIT_ASSERT_NO_THROW(_geometry.set_angles(30.* HKL_DEGTORAD,
                                               0.* HKL_DEGTORAD,
                                               -90.* HKL_DEGTORAD,
                                               60.* HKL_DEGTORAD);
                          CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(&hkl2)));
  CPPUNIT_ASSERT_NO_THROW(sample.computeU(0, 1));
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_I, sample.get_U()));

  // exception if not enough reflections.
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().del(1));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  CPPUNIT_ASSERT_NO_THROW(sample.reflections().del(0));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  // test with two other reflections.
  CPPUNIT_ASSERT_NO_THROW(_geometry.set_angles(30.* HKL_DEGTORAD,
                                               0.* HKL_DEGTORAD,
                                               90.* HKL_DEGTORAD,
                                               60.* HKL_DEGTORAD));
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(&hkl3));

  CPPUNIT_ASSERT_NO_THROW(_geometry.set_angles(30.* HKL_DEGTORAD,
                                               0.* HKL_DEGTORAD,
                                               180.* HKL_DEGTORAD,
                                               60.* HKL_DEGTORAD));
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(&hkl4));
  CPPUNIT_ASSERT_NO_THROW(sample.computeU(0, 1));
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_smatrix_cmp(&m_ref, sample.get_U()));
}

void
SampleTest::Fitness()
{
  static hkl_svector hkl1 = {{0, 0, 1}};
  static hkl_svector hkl2 = {{-1, 0, 0}};

  hkl::sample::MonoCrystal & sample = dynamic_cast<hkl::sample::MonoCrystal &>(*_sample);

  CPPUNIT_ASSERT_THROW(sample.fitness(), HKLException);

  _geometry.set_angles(30.* HKL_DEGTORAD,
                       0.* HKL_DEGTORAD,
                       0.* HKL_DEGTORAD,
                       60.* HKL_DEGTORAD);
  sample.reflections().add(&hkl1);
  _geometry.set_angles(30.* HKL_DEGTORAD,
                       0.* HKL_DEGTORAD,
                       -90.* HKL_DEGTORAD,
                       60.* HKL_DEGTORAD);
  sample.reflections().add(&hkl2);

  sample.computeU(0, 1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., sample.fitness(), HKL_EPSILON);
}
