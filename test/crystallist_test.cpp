#include "crystallist_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( CrystalListTest );

void
CrystalListTest::setUp(void)
{
    m_crystalList = CrystalList<geometry::eulerian4C::Vertical>();
}

void 
CrystalListTest::tearDown(void) 
{}

void
CrystalListTest::constructor(void)
{
  // Check if the DEFAULT_CRYSTAL_NAME is présent.
  CPPUNIT_ASSERT_NO_THROW(m_crystalList[DEFAULT_CRYSTAL_NAME]);
  
  // Check if the default crystal is an empty one.
  CPPUNIT_ASSERT_EQUAL(Crystal<geometry::eulerian4C::Vertical>(DEFAULT_CRYSTAL_NAME), m_crystalList[DEFAULT_CRYSTAL_NAME]);
}

void
CrystalListTest::remove(void)
{
  m_crystalList.add(Crystal<geometry::eulerian4C::Vertical>("toto"));
  CPPUNIT_ASSERT_NO_THROW(m_crystalList.remove("toto"));
  CPPUNIT_ASSERT_NO_THROW(m_crystalList.remove(DEFAULT_CRYSTAL_NAME));
  
  // The default crystal must be present
  CPPUNIT_ASSERT_NO_THROW(m_crystalList[DEFAULT_CRYSTAL_NAME]);  
}

void
CrystalListTest::clear(void)
{
  m_crystalList.add(Crystal<geometry::eulerian4C::Vertical>("toto"));
  CPPUNIT_ASSERT_NO_THROW(m_crystalList.clear());
  
  CPPUNIT_ASSERT_THROW(m_crystalList["toto"], HKLException);
  // The default crystal must be present
  CPPUNIT_ASSERT_NO_THROW(m_crystalList[DEFAULT_CRYSTAL_NAME]);  
}

void
CrystalListTest::persistanceIO(void)
{
  CrystalList<geometry::eulerian4C::Vertical> crystalList_ref; 
  CrystalList<geometry::eulerian4C::Vertical> crystalList1_ref;
  crystalList_ref.add(Crystal<geometry::eulerian4C::Vertical>("toto"));
  crystalList_ref.add(Crystal<geometry::eulerian4C::Vertical>("tutu"));
  crystalList1_ref.add(Crystal<geometry::eulerian4C::Vertical>("titi"));
  crystalList1_ref.add(Crystal<geometry::eulerian4C::Vertical>("popo"));   
  stringstream flux;

  CrystalList<geometry::eulerian4C::Vertical> crystalList;
  CrystalList<geometry::eulerian4C::Vertical> crystalList1;
  
  crystalList_ref.toStream(flux);
  crystalList1_ref.toStream(flux);
  crystalList.fromStream(flux);
  crystalList1.fromStream(flux);
  
  CPPUNIT_ASSERT_EQUAL(crystalList1_ref, crystalList1);
  CPPUNIT_ASSERT_EQUAL(crystalList_ref, crystalList);
}
