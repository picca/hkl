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
  lattice.alpha().set_current(90 * constant::math::degToRad);
  lattice.beta().set_current(90 * constant::math::degToRad);
  lattice.gamma().set_current(90 * constant::math::degToRad);

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
  CPPUNIT_ASSERT_EQUAL(std::string("Crystal"), _sample->get_name());
  CPPUNIT_ASSERT_EQUAL(smatrix(1., 0., 0., 0., 1., 0., 0., 0., 1.), dynamic_cast<hkl::sample::MonoCrystal *>(_sample)->get_U());
}

void
SampleTest::Equal()
{
  hkl::Sample * sample = new hkl::sample::MonoCrystal(_geometry, "toto");

  sample->reflections().add(hkl::svector(1, 1, 1));

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
  hkl::sample::MonoCrystal & sample = dynamic_cast<hkl::sample::MonoCrystal &>(*_sample);

  smatrix M(1., 0., 0.,
            0., 1., 0.,
            0., 0., 1.);

  // without reflection an exception is throw
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  // add a non valid for computation reflection
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(hkl::svector(0, 0, 0)));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  // add a second non valid for computation reflection
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(hkl::svector(0, 0, 0)));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  CPPUNIT_ASSERT_NO_THROW(sample.reflections().del(1));
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().del(0));

  //with only one valid reflection exception
  CPPUNIT_ASSERT_NO_THROW(_geometry.set_angles(30.* constant::math::degToRad,
                                               0.* constant::math::degToRad,
                                               0.* constant::math::degToRad,
                                               60.* constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(hkl::svector(0, 0, 1)));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  //with two valid reflection, no exception
  CPPUNIT_ASSERT_NO_THROW(_geometry.set_angles(30.* constant::math::degToRad,
                                               0.* constant::math::degToRad,
                                               -90.* constant::math::degToRad,
                                               60.* constant::math::degToRad);
                          CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(hkl::svector(-1, 0, 0))));
  CPPUNIT_ASSERT_NO_THROW(sample.computeU(0, 1));
  CPPUNIT_ASSERT_EQUAL(M, sample.get_U());

  // exception if not enough reflections.
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().del(1));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  CPPUNIT_ASSERT_NO_THROW(sample.reflections().del(0));
  CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

  // test with two other reflections.
  CPPUNIT_ASSERT_NO_THROW(_geometry.set_angles(30.* constant::math::degToRad,
                                               0.* constant::math::degToRad,
                                               90.* constant::math::degToRad,
                                               60.* constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(hkl::svector(1, 0, 0)));

  CPPUNIT_ASSERT_NO_THROW(_geometry.set_angles(30.* constant::math::degToRad,
                                               0.* constant::math::degToRad,
                                               180.* constant::math::degToRad,
                                               60.* constant::math::degToRad));
  CPPUNIT_ASSERT_NO_THROW(sample.reflections().add(hkl::svector(0, 1, 0)));
  CPPUNIT_ASSERT_NO_THROW(sample.computeU(0, 1));
  M.set(1., 0., 0.,
        0., 0., 1.,
        0.,-1., 0.);
  CPPUNIT_ASSERT_EQUAL(M, sample.get_U());
}

void
SampleTest::Fitness()
{
  hkl::sample::MonoCrystal & sample = dynamic_cast<hkl::sample::MonoCrystal &>(*_sample);

  smatrix M(1., 0., 0.,
            0., 1., 0.,
            0., 0., 1.);

  CPPUNIT_ASSERT_THROW(sample.fitness(), HKLException);

  _geometry.set_angles(30.* constant::math::degToRad,
                       0.* constant::math::degToRad,
                       0.* constant::math::degToRad,
                       60.* constant::math::degToRad);
  sample.reflections().add(hkl::svector(0, 0, 1));
  _geometry.set_angles(30.* constant::math::degToRad,
                       0.* constant::math::degToRad,
                       -90.* constant::math::degToRad,
                       60.* constant::math::degToRad);
  sample.reflections().add(hkl::svector(-1, 0, 0));

  sample.computeU(0, 1);
  CPPUNIT_ASSERT_DOUBLES_EQUAL(0., sample.fitness(), constant::math::epsilon);
}

void
SampleTest::persistanceIO(void)
{
  hkl::Sample * crystal_ref = new hkl::sample::MonoCrystal(_geometry, "tutu");
  hkl::Sample * crystal1_ref = new hkl::sample::MonoCrystal(_geometry, "toto");
  hkl::Sample * crystal = new hkl::sample::MonoCrystal(_geometry, "toto");
  hkl::Sample * crystal1 = new hkl::sample::MonoCrystal(_geometry, "tutu");
  std::stringstream flux;

  // set the lattice of the crystal_ref
  Lattice * lattice = &crystal_ref->lattice();
  lattice->a().set_current(1);
  lattice->b().set_current(1);
  lattice->c().set_current(1.54);
  lattice->alpha().set_current(90 * constant::math::degToRad);
  lattice->beta().set_current(90 * constant::math::degToRad);
  lattice->gamma().set_current(90 * constant::math::degToRad);

  crystal_ref->reflections().add(hkl::svector(1, 1, 1));

  // set the lattice of the crystal1_ref
  lattice = &crystal1_ref->lattice();
  lattice->a().set_current(1);
  lattice->b().set_current(2.);
  lattice->c().set_current(1.54);
  lattice->alpha().set_current(60 * constant::math::degToRad);
  lattice->beta().set_current(90 * constant::math::degToRad);
  lattice->gamma().set_current(91 * constant::math::degToRad);

  crystal_ref->toStream(flux);
  crystal1_ref->toStream(flux);
  crystal->fromStream(flux);
  crystal1->fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*crystal_ref, *crystal);
  CPPUNIT_ASSERT_EQUAL(*crystal1_ref, *crystal1);

  delete crystal_ref;
  delete crystal1_ref;
  delete crystal;
  delete crystal1;
}
