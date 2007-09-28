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
  hkl_svector axe =  {{1, 0, 0}};
  hkl_svector axe1 = {{0, -1, 0}};

  // On peut ajouter un Axe dans la partie sample et dans la partie detecteur
  CPPUNIT_ASSERT_NO_THROW(_holder->add_rotation("a", &axe));
  CPPUNIT_ASSERT_NO_THROW(_holder->add_rotation("b", &axe));

  // On vérifie que l'on ne peut pas mettre deux fois le même axe.
  CPPUNIT_ASSERT_THROW(_holder->add_rotation("a", &axe), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(_holder->add_rotation("b", &axe), hkl::HKLException);

  // on ne peut pas mettre un axe avec le même nom mais different.
  CPPUNIT_ASSERT_THROW(_holder->add_rotation("a", &axe), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(_holder->add_rotation("b", &axe), hkl::HKLException);

  // test de la présence d'un axe dans un autre holder imaginaire en ajoutant à la main un axe dans l'axeList.
  _holderList->axes().push_back(new hkl::axe::Rotation("c", "rotation", -2*M_PI, 0, 2*M_PI, &axe1));
  // on peut ajouter cet axe au holder
  CPPUNIT_ASSERT_NO_THROW(_holder->add_rotation("c", &axe1));
}

void
HolderTest::apply(void)
{
  hkl_svector axe =  {{0, -1, 0}};
  hkl_svector axe1 = {{0, 0, 1}};
  hkl_svector axe2 = {{1, 0, 0}};

  _holder->add_rotation("omega", &axe);
  _holder->add_rotation("gamma", &axe1);

  // Verification of the apply method of the Axe class.
  hkl_quaternion q;
  hkl_quaternion q_ref;

  ::hkl_quaternion_from_angle_and_axe(&q, 10 * HKL_DEGTORAD, &axe2);
  ::hkl_quaternion_from_angle_and_axe(&q_ref, 10 * HKL_DEGTORAD, &axe2);
  _holder->apply(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
  _holder->apply_consign(&q);
  CPPUNIT_ASSERT_EQUAL(HKL_TRUE, ::hkl_quaternion_cmp(&q_ref, &q));
}
