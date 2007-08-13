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

  hkl::Axe * A1 = new hkl::Axe("a", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * A2 = new hkl::Axe("a", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * B1 = new hkl::Axe("b", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);
  hkl::Axe * B2 = new hkl::Axe("b", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi);

  // On peut ajouter un Axe dans la partie sample et dans la partie detecteur
  CPPUNIT_ASSERT_NO_THROW(geometry.add_sample_axe(A1));
  CPPUNIT_ASSERT_NO_THROW(geometry.add_detector_axe(B1));

  // On vérifie que l'on ne peut pas mettre deux fois le même axe.
  CPPUNIT_ASSERT_THROW(geometry.add_sample_axe(A1), hkl::HKLException);
  CPPUNIT_ASSERT_THROW(geometry.add_detector_axe(B1), hkl::HKLException);

  // On verifie que l'on ne peut pas rajouter à detector un axe qui porte le même nom mais qui est différent
  CPPUNIT_ASSERT_THROW(geometry.add_detector_axe(A2), hkl::HKLException);
  // de même pour le sample
  CPPUNIT_ASSERT_THROW(geometry.add_sample_axe(B2), hkl::HKLException);
  // Par contre on peut ajouter le même axe dans la partie detecteur.
  CPPUNIT_ASSERT_NO_THROW(geometry.add_detector_axe(A1));
  // idem pour sample
  CPPUNIT_ASSERT_NO_THROW(geometry.add_sample_axe(B1));

  delete A1;
  delete A2;
  delete B1;
  delete B2;
}

void
GeometryTest::operateurs(void)
{
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

  //Test de l'assignation (memory leak)
  /*
  hkl::Geometry geometry = *m_geometry;
  geometry.add_sample_axe(hkl::Axe("delta", "t", -hkl::constant::math::pi, 0, hkl::constant::math::pi));
  geometry = *m_geometry;
  CPPUNIT_ASSERT_EQUAL(geometry, *m_geometry);
  */

  delete Omega;
  delete Gamma;
}

void
GeometryTest::persistanceIO(void)
{
  hkl::Geometry geometry1("1", "1", 2);
  hkl::Geometry geometry2("2", "2", 2);
  stringstream flux;

  m_geometry->toStream(flux);
  m_geometry->toStream(flux);
  geometry1.fromStream(flux);
  geometry2.fromStream(flux);

  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry1);
  CPPUNIT_ASSERT_EQUAL(*m_geometry, geometry2);
}
