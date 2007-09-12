#include "holder_test.h"
#include <axe_rotation.h>

CPPUNIT_TEST_SUITE_REGISTRATION( HolderTest );

void
HolderTest::setUp(void)
{
  _holderList = new hkl::HolderList;
  _holder = _holderList->add();
}

void
HolderTest::tearDown(void)
{
  delete _holderList;
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
  // On peut ajouter un Axe dans la partie sample et dans la partie detecteur
  CPPUNIT_ASSERT_NO_THROW(_holder->add_rotation("a", hkl::svector(1, 0, 0)));
  CPPUNIT_ASSERT_NO_THROW(_holder->add_rotation("b", hkl::svector(1, 0, 0)));

  // On vérifie que l'on ne peut pas mettre deux fois le même axe.
  CPPUNIT_ASSERT_THROW(_holder->add_rotation("a", hkl::svector(1, 0, 0)), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(_holder->add_rotation("b", hkl::svector(1, 0, 0)), hkl::HKLException);

  // on ne peut pas mettre un axe avec le même nom mais different.
  CPPUNIT_ASSERT_THROW(_holder->add_rotation("a", hkl::svector(1, 1, 0)), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(_holder->add_rotation("b", hkl::svector(1, 1, 0)), hkl::HKLException);

  // test de la présence d'un axe dans un autre holder imaginaire en ajoutant à la main un axe dans l'axeList.
  _holderList->axes().push_back(new hkl::axe::Rotation("c", "rotation", -2*hkl::constant::math::pi, 0, 2*hkl::constant::math::pi, hkl::svector(0, -1, 0)));
  // on peut ajouter cet axe au holder
  CPPUNIT_ASSERT_NO_THROW(_holder->add_rotation("c", hkl::svector(0, -1, 0)));
}

void
HolderTest::apply(void)
{
  _holder->add_rotation("omega", hkl::svector(0, -1, 0));
  _holder->add_rotation("gamma", hkl::svector(0, 0, 1));

  // Verification of the apply method of the Axe class.
  hkl::Quaternion q(10 * hkl::constant::math::degToRad, hkl::svector(1, 0, 0));
  hkl::Quaternion q_ref(10 * hkl::constant::math::degToRad, hkl::svector(1, 0, 0));
  _holder->apply(q);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
  _holder->apply_consign(q);
  CPPUNIT_ASSERT_EQUAL(q_ref, q);
}

void
HolderTest::persistanceIO(void)
{
  hkl::Holder holder1(_holderList);
  hkl::Holder holder2(_holderList);
  std::stringstream flux;

  // _modification of the _holder before saving it.
  _holder->add_rotation("omega", hkl::svector(0, -1, 0));
  _holder->add_rotation("gamma", hkl::svector(0, 0, 1));

  _holder->toStream(flux);
  _holder->toStream(flux);
  holder1.fromStream(flux);
  holder2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*_holder, holder1);
  CPPUNIT_ASSERT_EQUAL(*_holder, holder2);
}
