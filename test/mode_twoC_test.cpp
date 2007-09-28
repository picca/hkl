#include "mode_twoC_test.h"
#include "reflectionlist.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_TwoC_Test );

void
Mode_TwoC_Test::setUp(void)
{
  hkl_svector hkl;

  _geometry.get_source().setWaveLength(1.54);
  _sample = new hkl::sample::MonoCrystal(_geometry, "test");
  hkl::Lattice lattice = _sample->lattice();
  lattice.a().set_current(1.54);
  lattice.b().set_current(1.54);
  lattice.c().set_current(1.54);
  lattice.alpha().set_current(90 * HKL_DEGTORAD);
  lattice.beta().set_current(90 * HKL_DEGTORAD);
  lattice.gamma().set_current(90 * HKL_DEGTORAD);

  _geometry.set_angles(30.*HKL_DEGTORAD,
                       60.*HKL_DEGTORAD);
  hkl.data[X] = 0;
  hkl.data[Y] = 0;
  hkl.data[Z] = 1;
  _sample->reflections().add(&hkl);

  _geometry.set_angles(120.*HKL_DEGTORAD,
                       60.*HKL_DEGTORAD);
  hkl.data[X] = 0;
  hkl.data[Y] = 1;
  hkl.data[Z] = 0;
  _sample->reflections().add(&hkl);

  _sample->computeU(0, 1);

  _geometry.set_angles(0, 0);
}

void
Mode_TwoC_Test::tearDown(void)
{
  delete _sample;
}

void
Mode_TwoC_Test::Symetric(void)
{
  hkl_smatrix UB;
  _sample->get_UB(&UB);

  hkl::twoC::vertical::mode::Symetric mode("symetric", "test", _geometry);

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., &UB), hkl::HKLException);
  // exception if the wavelength is null.
  CPPUNIT_ASSERT_THROW(mode.computeAngles(3., 0, 0, &UB), hkl::HKLException);
  //exception with unobtainable reflection.
  _geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_THROW(mode.computeAngles(3., 0, 0, &UB), hkl::HKLException);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 0., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., 1., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., -1., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(30*HKL_DEGTORAD), _geometry.omega()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(hkl::Value(90*HKL_DEGTORAD), _geometry.tth()->get_consign());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(45*HKL_DEGTORAD), _geometry.omega()->get_consign());
}

void
Mode_TwoC_Test::Fix_Incidence(void)
{
  hkl_smatrix UB;
  _sample->get_UB(&UB);

  hkl::twoC::vertical::mode::Fix_Incidence mode("incidence fix", "test", _geometry);

  // omega must not change in this mode.
  hkl::Value omega(_geometry.omega()->get_current());

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., &UB), hkl::HKLException);
  // exception if the wavelength is null.
  CPPUNIT_ASSERT_THROW(mode.computeAngles(3., 0, 0, &UB), hkl::HKLException);
  //exception with unobtainable reflection.
  _geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_THROW(mode.computeAngles(3., 0, 0, &UB), hkl::HKLException);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(omega, _geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(omega, _geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(omega, _geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(60*HKL_DEGTORAD), _geometry.tth()->get_consign());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., &UB));
  CPPUNIT_ASSERT_EQUAL(omega, _geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(hkl::Value(90*HKL_DEGTORAD), _geometry.tth()->get_consign());
}
