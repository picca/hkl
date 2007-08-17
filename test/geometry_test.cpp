#include "geometry_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( GeometryTest );

void
GeometryTest::setUp(void)
{
  m_geometry = new hkl::Geometry("test", "test");
}

void
GeometryTest::tearDown(void)
{
  delete m_geometry;
}

void
GeometryTest::equal(void)
{
  CPPUNIT_ASSERT_EQUAL(m_geometry, m_geometry);
}

void
GeometryTest::copyConstructor(void)
{
  hkl::Geometry geometry(*m_geometry);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry);
}

void
GeometryTest::operateurs(void)
{
  /*
  hkl::Axe * Omega = new hkl::Axe("omega", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * Gamma = new hkl::Axe("gamma", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);

  // on verifie que les exceptions sont bien lancees lorsque
  // l'on recherche un axe qui n'existe pas.
  CPPUNIT_ASSERT_THROW(m_geometry->get_axe("toto"), hkl::HKLException);

  // et que tout se passe bien le cas contraire.
  m_geometry->add_sample_axe(Omega);
  m_geometry->add_sample_axe(Gamma);
  CPPUNIT_ASSERT_NO_THROW(m_geometry->get_axe("omega"));
  CPPUNIT_ASSERT_NO_THROW(m_geometry->get_axe("gamma"));

  // On verifie que les valeurs retournées sont les bonnes.
  CPPUNIT_ASSERT_EQUAL(Omega, m_geometry->get_axe("omega"));
  CPPUNIT_ASSERT_EQUAL(Gamma, m_geometry->get_axe("gamma"));
  */
  //Test de l'assignation (memory leak)
  /*
  hkl::Geometry geometry = *m_geometry;
  geometry.add_sample_axe(hkl::Axe("delta", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi));
  geometry = *m_geometry;
  CPPUNIT_ASSERT_EQUAL(geometry, *m_geometry);
  */
/*
  delete Omega;
  delete Gamma;
*/
}

void
GeometryTest::persistanceIO(void)
{
  hkl::Geometry geometry1("1", "1");
  hkl::Geometry geometry2("2", "2");
  std::stringstream flux;

  m_geometry->toStream(flux);
  m_geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry1);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry2);
}
