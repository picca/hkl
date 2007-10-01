#include "samplelist_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( SampleListTest );

void
SampleListTest::setUp(void)
{
  _sampleList = new hkl::SampleList(_geometry);
}

void
SampleListTest::tearDown(void)
{
  delete _sampleList;
}

void
SampleListTest::operators(void)
{
  // ==
  CPPUNIT_ASSERT_EQUAL(*_sampleList, *_sampleList);

  CPPUNIT_ASSERT_EQUAL((unsigned int)0, _sampleList->size());
  //CPPUNIT_ASSERT_THROW((*_sampleList)[0], HKLException);
  CPPUNIT_ASSERT_NO_THROW(_sampleList->add("Mono-Crystal", hkl::SAMPLE_MONOCRYSTAL));
  CPPUNIT_ASSERT_EQUAL((unsigned int)1, _sampleList->size());
  CPPUNIT_ASSERT_NO_THROW((*_sampleList)["Mono-Crystal"]);

  //erase
  std::vector<hkl::Sample *>::iterator iter = _sampleList->begin();
  CPPUNIT_ASSERT_NO_THROW(_sampleList->erase(iter));
}
