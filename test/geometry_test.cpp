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
GeometryTest::addSampleDetectorAxe(void)
{
  hkl::Geometry geometry("toto", "titi");

  hkl::Axe A1("a", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi, hkl::svector(0., 0., 1.), 1);
  hkl::Axe A2("a", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi, hkl::svector(0., 0., 1.), -1);
  hkl::Axe B1("b", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi, hkl::svector(0., 0., 1.), 1);
  hkl::Axe B2("b", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi, hkl::svector(0., 0., 1.), -1);

  // On peut ajouter un Axe dans la partie sample et dans la partie detecteur
  CPPUNIT_ASSERT_NO_THROW(geometry.addSampleAxe(A1));
  CPPUNIT_ASSERT_NO_THROW(geometry.addDetectorAxe(B1));

  // On vérifie que l'on ne peut pas mettre deux fois le même axe.
  CPPUNIT_ASSERT_THROW(geometry.addSampleAxe(A1), HKLException);
  CPPUNIT_ASSERT_THROW(geometry.addDetectorAxe(B1), HKLException);

  // On verifie que l'on ne peut pas rajouter à detector un axe qui porte le même nom mais qui est différent
  CPPUNIT_ASSERT_THROW(geometry.addDetectorAxe(A2), HKLException);
  // de même pour le sample
  CPPUNIT_ASSERT_THROW(geometry.addSampleAxe(B2), HKLException);
  // Par contre on peut ajouter le même axe dans la partie detecteur.
  CPPUNIT_ASSERT_NO_THROW(geometry.addDetectorAxe(A1));
  // idem pour sample
  CPPUNIT_ASSERT_NO_THROW(geometry.addSampleAxe(B1));
}

void
GeometryTest::operateurs(void)
{
  hkl::Axe Omega("omega", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi, hkl::svector(0., 1., 0.), -1);
  hkl::Axe Gamma("gamma", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi, hkl::svector(0., 0., 1.), 1);

  // on verifie que les exceptions sont bien lancees lorsque
  // l'on recherche un axe qui n'existe pas.
  CPPUNIT_ASSERT_THROW(m_geometry->get_axe("toto"), HKLException);

  // et que tout se passe bien le cas contraire.
  m_geometry->addSampleAxe(Omega);
  m_geometry->addSampleAxe(Gamma);
  CPPUNIT_ASSERT_NO_THROW(m_geometry->get_axe("omega"));
  CPPUNIT_ASSERT_NO_THROW(m_geometry->get_axe("gamma"));

  // On verifie que les valeurs retournées sont les bonnes.
  CPPUNIT_ASSERT_EQUAL(Omega, m_geometry->get_axe("omega"));
  CPPUNIT_ASSERT_EQUAL(Gamma, m_geometry->get_axe("gamma"));

  //Test de l'assignation (memory leak)
  hkl::Geometry geometry = *m_geometry;
  geometry.addSampleAxe(hkl::Axe("delta", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi, hkl::svector(0., 1., 0.), -1));
  geometry = *m_geometry;
  CPPUNIT_ASSERT_EQUAL(geometry, *m_geometry);
}

void
GeometryTest::persistanceIO(void)
{
  hkl::Geometry geometry1("1", "1");
  hkl::Geometry geometry2("2", "2");
  stringstream flux;

  m_geometry->toStream(flux);
  m_geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry1);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry2);
}
