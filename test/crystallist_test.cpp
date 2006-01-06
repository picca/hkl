#include "crystallist_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( CrystalListTest );

void
CrystalListTest::setUp(void)
{}

void 
CrystalListTest::tearDown(void) 
{}

void
CrystalListTest::constructor(void)
{
  CrystalList crystalList;
  
  // Check if the DEFAULT_CRYSTAL_NAME is présent.
  CPPUNIT_ASSERT_NO_THROW(crystalList[DEFAULT_CRYSTAL_NAME]);
  
  // Check if the default crystal is an empty one.
  CPPUNIT_ASSERT_EQUAL(Crystal(DEFAULT_CRYSTAL_NAME), crystalList[DEFAULT_CRYSTAL_NAME]);
}

void
CrystalListTest::remove(void)
{
  CrystalList crystalList;
  
  crystalList.add(Crystal("toto"));
  CPPUNIT_ASSERT_NO_THROW(crystalList.remove("toto"));
  CPPUNIT_ASSERT_NO_THROW(crystalList.remove(DEFAULT_CRYSTAL_NAME));
  
  // The default crystal must be present
  CPPUNIT_ASSERT_NO_THROW(crystalList[DEFAULT_CRYSTAL_NAME]);  
}

void
CrystalListTest::clear(void)
{
  CrystalList crystalList;
  
  crystalList.add(Crystal("toto"));
  CPPUNIT_ASSERT_NO_THROW(crystalList.clear());
  
  CPPUNIT_ASSERT_THROW(crystalList["toto"], HKLException);
  // The default crystal must be present
  CPPUNIT_ASSERT_NO_THROW(crystalList[DEFAULT_CRYSTAL_NAME]);  
}

void
CrystalListTest::persistanceIO(void)
{
  CrystalList crystalList_ref; 
  CrystalList crystalList1_ref;
  crystalList_ref.add(Crystal("toto"));
  crystalList_ref.add(Crystal("tutu"));
  crystalList1_ref.add(Crystal("titi"));
  crystalList1_ref.add(Crystal("popo"));   
  stringstream flux;

  CrystalList crystalList;
  CrystalList crystalList1;
  
  crystalList_ref.toStream(flux);
  crystalList1_ref.toStream(flux);
  crystalList.fromStream(flux);
  crystalList1.fromStream(flux);
  
  CPPUNIT_ASSERT_EQUAL(crystalList1_ref, crystalList1);
  CPPUNIT_ASSERT_EQUAL(crystalList_ref, crystalList);
}
