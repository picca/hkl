#ifndef _HOLDERLIST_TEST_H
#define _HOLDERLIST_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include <holderlist.h>

class HolderListTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( HolderListTest );
    CPPUNIT_TEST( add );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( copyConstructor );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

  private:
    hkl::HolderList * _holderList;

  public:

    void setUp(void);
    void tearDown(void);

    void add(void);
    void equal(void);
    void copyConstructor(void);
    void persistanceIO(void);
  };

#endif /* _HOLDERLIST_TEST_H */
