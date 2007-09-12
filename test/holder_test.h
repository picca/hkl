#ifndef _HOLDER_TEST_H
#define _HOLDER_TEST_H

#include <cppunit/extensions/HelperMacros.h>
#include "axe.h"
#include "holder.h"
#include "quaternion.h"

class HolderTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( HolderTest );
    CPPUNIT_TEST( equal );
    CPPUNIT_TEST( add );
    CPPUNIT_TEST( apply );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

  private:
    hkl::HolderList * _holderList;
    hkl::Holder * _holder;

  public:

    void setUp(void);
    void tearDown(void);

    void equal(void);
    void copyConstructor(void);
    void add(void);
    void apply(void);
    void persistanceIO(void);
  };

#endif /* _HOLDER_TEST_H */
