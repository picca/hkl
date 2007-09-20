#ifndef _HOLDERLIST_TEST_H
#define _HOLDERLIST_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <holder.h>

class HolderListTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( HolderListTest );
#ifndef PROFILE
    CPPUNIT_TEST( add );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
#else
    CPPUNIT_TEST( profile );
#endif

    CPPUNIT_TEST_SUITE_END();

  private:
    hkl::HolderList * _holderList;

  public:

    void setUp(void);
    void tearDown(void);

    void add(void);
    void equal(void);
    void copyConstructor(void);
    void profile(void);
  };

#endif /* _HOLDERLIST_TEST_H */
