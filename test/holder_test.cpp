#include "holder_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( HolderTest );

void
HolderTest::setUp(void)
{
  _axeList = new hkl::AxeList;
  _holder = new hkl::Holder(*_axeList);
}

void
HolderTest::tearDown(void)
{
  delete _holder;
  delete _axeList;
}

void
HolderTest::equal(void)
{
  CPPUNIT_ASSERT_EQUAL(_holder, _holder);
}

void
HolderTest::copyConstructor(void)
{
  hkl::Holder holder(*_holder);

  CPPUNIT_ASSERT_EQUAL(*_holder, holder);
}

void
HolderTest::add(void)
{
  hkl::Axe * A1 = new hkl::Axe("a", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * A2 = new hkl::Axe("a", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * B1 = new hkl::Axe("b", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * B2 = new hkl::Axe("b", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);

  // On peut ajouter un Axe dans la partie sample et dans la partie detecteur
  CPPUNIT_ASSERT_NO_THROW(_holder->add(A1));
  CPPUNIT_ASSERT_NO_THROW(_holder->add(B1));

  // On vérifie que l'on ne peut pas mettre deux fois le même axe.
  CPPUNIT_ASSERT_THROW(_holder->add(A1), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(_holder->add(B1), hkl::HKLException);

  // On verifie que l'on ne peut pas rajouter à detector un axe qui porte le même nom mais qui est différent
  CPPUNIT_ASSERT_THROW(_holder->add(A2), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(_holder->add(B2), hkl::HKLException);

  delete A1;
  delete A2;
  delete B1;
  delete B2;
}

void
HolderTest::apply(void)
{
  hkl::Axe * Omega = new hkl::Axe("omega", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * Gamma = new hkl::Axe("gamma", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  _holder->add(Omega);
  _holder->add(Gamma);

  // Verification of the apply method of the Axe class.
  hkl::Quaternion q(10 * hkl::constant::math::degToRad, hkl::svector(1, 0, 0));
  hkl::Quaternion q_ref(10 * hkl::constant::math::degToRad, hkl::svector(1, 0, 0));
  _holder->apply(q);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);

  delete Omega;
  delete Gamma;
}

void
HolderTest::persistanceIO(void)
{
  hkl::Holder holder1(*_axeList);
  hkl::Holder holder2(*_axeList);
  stringstream flux;

  // _modification of the _holder before saving it.
  hkl::Axe * Omega = new hkl::Axe("omega", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * Gamma = new hkl::Axe("gamma", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  _holder->add(Omega);
  _holder->add(Gamma);

  _holder->toStream(flux);
  _holder->toStream(flux);
  holder1.fromStream(flux);
  holder2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*_holder, holder1);
  CPPUNIT_ASSERT_EQUAL(*_holder, holder2);
  delete Omega;
  delete Gamma;
}
