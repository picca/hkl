#include "mode_twoC_test.h"
#include "reflectionlist.h"

CPPUNIT_TEST_SUITE_REGISTRATION( Mode_TwoC_Test );

void
Mode_TwoC_Test::setUp(void)
{
  _geometry.get_source().setWaveLength(1.54);
  _sample = new hkl::sample::MonoCrystal(_geometry, "test");
  hkl::Lattice lattice = _sample->lattice();
  lattice.a().set_current(1.54);
  lattice.b().set_current(1.54);
  lattice.c().set_current(1.54);
  lattice.alpha().set_current(90 * constant::math::degToRad);
  lattice.beta().set_current(90 * constant::math::degToRad);
  lattice.gamma().set_current(90 * constant::math::degToRad);

  _geometry.setAngles(30.*constant::math::degToRad,
                      60.*constant::math::degToRad);
  _sample->reflections().add(svector(0., 0., 1.));

  _geometry.setAngles(120.*constant::math::degToRad,
                      60.*constant::math::degToRad);
  _sample->reflections().add(svector(0., 1., 0.));

  _sample->computeU(0, 1);

  _geometry.setAngles(0, 0);
}

void
Mode_TwoC_Test::tearDown(void)
{
  delete _sample;
}

void
Mode_TwoC_Test::Symetric(void)
{
  smatrix UB = _sample->get_UB();

  mode::twoC::vertical::Symetric mode("symetric", "test", _geometry);

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., UB), HKLException);
  // exception if the wavelength is null.
  CPPUNIT_ASSERT_THROW(mode.computeAngles(3., 0, 0, UB), HKLException);
  //exception with unobtainable reflection.
  _geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_THROW(mode.computeAngles(3., 0, 0, UB), HKLException);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., 1., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 0., -1., UB));
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(30*constant::math::degToRad), _geometry.omega()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(Value(90*constant::math::degToRad), _geometry.tth()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(45*constant::math::degToRad), _geometry.omega()->get_current());
}

void
Mode_TwoC_Test::Fix_Incidence(void)
{
  smatrix UB = _sample->get_UB();

  mode::twoC::vertical::Fix_Incidence mode("incidence fix", "test", _geometry);

  // omega must not change in this mode.
  Value omega(_geometry.omega()->get_current());

  // Exception if try to compute [h,k,l]=[0,0,0]
  CPPUNIT_ASSERT_THROW(mode.computeAngles(0., 0., 0., UB), HKLException);
  // exception if the wavelength is null.
  CPPUNIT_ASSERT_THROW(mode.computeAngles(3., 0, 0, UB), HKLException);
  //exception with unobtainable reflection.
  _geometry.get_source().setWaveLength(1.54);
  CPPUNIT_ASSERT_THROW(mode.computeAngles(3., 0, 0, UB), HKLException);

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(-1., 0., 0., UB));
  CPPUNIT_ASSERT_EQUAL(omega, _geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(omega, _geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(0.,-1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(omega, _geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(60*constant::math::degToRad), _geometry.tth()->get_current());

  CPPUNIT_ASSERT_NO_THROW(mode.computeAngles(1., 1., 0., UB));
  CPPUNIT_ASSERT_EQUAL(omega, _geometry.omega()->get_current());
  CPPUNIT_ASSERT_EQUAL(Value(90*constant::math::degToRad), _geometry.tth()->get_current());
}

void
Mode_TwoC_Test::persistanceIO(void)
{
  mode::twoC::vertical::Symetric symetric_ref("symetric ref", "test", _geometry);
  mode::twoC::vertical::Symetric symetric("symetric", "test", _geometry);
  mode::twoC::vertical::Fix_Incidence fix_incidence_ref("incidence fixe ref", "test", _geometry);
  mode::twoC::vertical::Fix_Incidence fix_incidence("incidence fixe", "test", _geometry);
  stringstream flux;

  symetric_ref.toStream(flux);
  fix_incidence_ref.toStream(flux);
  symetric.fromStream(flux);
  fix_incidence.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(symetric_ref, symetric);
  CPPUNIT_ASSERT_EQUAL(fix_incidence_ref, fix_incidence);
}
