#include "holderlist_test.h"
#include <axe_rotation.h>
#include <holder.h>

CPPUNIT_TEST_SUITE_REGISTRATION( HolderListTest );

void
HolderListTest::setUp(void)
{
  hkl_svector axe = {{0, 0, 1}};

  _holderList = new hkl::HolderList;
  hkl::Holder * holder = _holderList->add();
  holder->add_rotation("A", &axe);
  holder->add_rotation("B", &axe);
  holder = _holderList->add();
  holder->add_rotation("A", &axe);
  holder->add_rotation("C", &axe);
}

void
HolderListTest::tearDown(void)
{
  delete _holderList;
}

void
HolderListTest::add(void)
{
  hkl::HolderList holderList;

  // check the size of the empty holderList
  CPPUNIT_ASSERT_EQUAL((unsigned int)0, holderList.size());
  // check if an empty holder has been add.
  holderList.add();
  CPPUNIT_ASSERT_EQUAL((unsigned int)1, holderList.size());

  // add a second holder
  holderList.add();
  CPPUNIT_ASSERT_EQUAL((unsigned int)2, holderList.size());
}

void
HolderListTest::equal(void)
{
  // compare two empty HolderList
  CPPUNIT_ASSERT_EQUAL(_holderList, _holderList);
}

void
HolderListTest::copyConstructor(void)
{
  hkl::HolderList holderList(*_holderList);

  CPPUNIT_ASSERT_EQUAL(*_holderList, holderList);
}

void
HolderListTest::profile(void)
{
  std::stringstream flux;
  //copy profile
  for (unsigned int i=0; i<100000; i++)
    {
      hkl::HolderList holder(*_holderList);
      holder.toStream(flux);
      holder.fromStream(flux);
    }

}
