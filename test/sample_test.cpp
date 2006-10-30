#include "sample_test.h"

CPPUNIT_TEST_SUITE_REGISTRATION( SampleTest );

void
SampleTest::setUp(void)
{
    _sample = new hkl::sample::MonoCrystal(_geometry, "Crystal");
    Lattice & lattice = _sample->lattice();
    lattice.a().set_current(1.54);
    lattice.b().set_current(1.54);
    lattice.c().set_current(1.54);
    lattice.alpha().set_current(90 * constant::math::degToRad);
    lattice.beta().set_current(90 * constant::math::degToRad);
    lattice.gamma().set_current(90 * constant::math::degToRad);
}

void 
SampleTest::tearDown(void) 
{
  delete _sample;
}

void
SampleTest::Constructor()
{
    CPPUNIT_ASSERT_EQUAL(MyString("Crystal"), _sample->get_name());
    CPPUNIT_ASSERT_EQUAL(smatrix(1., 0., 0., 0., 1., 0., 0., 0., 1.), _sample->get_UB());
}

void
SampleTest::Equal()
{
    hkl::Sample * sample = new hkl::sample::MonoCrystal(_geometry, "toto");

    CPPUNIT_ASSERT_EQUAL(*_sample, *_sample);
    CPPUNIT_ASSERT_EQUAL(*sample, *sample);
    CPPUNIT_ASSERT_ASSERTION_FAIL(CPPUNIT_ASSERT_EQUAL(*_sample, *sample));

    delete sample;
}

void
SampleTest::clone()
{ 
    hkl::Sample * sample = _sample->clone();

    CPPUNIT_ASSERT_EQUAL(*_sample, *sample);

    delete sample;
}

/*
void
SampleTest::GetLattice()
{
    Crystal<geometry::eulerian4C::Vertical> crystal("crystal1");
    double a, b, c, alpha, beta, gamma;

    crystal.getLattice(&a, &b, &c, &alpha, &beta, &gamma);
    CPPUNIT_ASSERT_EQUAL(0., a);
    CPPUNIT_ASSERT_EQUAL(0., b);
    CPPUNIT_ASSERT_EQUAL(0., c);
    CPPUNIT_ASSERT_EQUAL(0., alpha);
    CPPUNIT_ASSERT_EQUAL(0., beta);
    CPPUNIT_ASSERT_EQUAL(0., gamma);
}

void
SampleTest::SetLattice()
{
    Crystal<geometry::eulerian4C::Vertical> crystal("crystal1");
    double a, b, c, alpha, beta, gamma;

    crystal.setLattice(1., 1., 1.54, 3., 2., 1.);
    crystal.getLattice(&a, &b, &c, &alpha, &beta, &gamma);
    CPPUNIT_ASSERT_EQUAL(1., a);
    CPPUNIT_ASSERT_EQUAL(1., b);
    CPPUNIT_ASSERT_EQUAL(1.54, c);
    CPPUNIT_ASSERT_EQUAL(3., alpha);
    CPPUNIT_ASSERT_EQUAL(2., beta);
    CPPUNIT_ASSERT_EQUAL(1., gamma);
}

void
SampleTest::GetReciprocalLattice()
{
    Crystal<geometry::eulerian4C::Vertical> crystal("crystal1");
    double a, b, c, alpha, beta, gamma;

    // cubic
    crystal.setLattice(1.54, 1.54, 1.54,
                       90.* constant::math::degToRad, 90.* constant::math::degToRad, 90.* constant::math::degToRad);
    crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 1.54, a, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 1.54, b, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 1.54, c, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, alpha, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, beta, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, gamma, constant::math::epsilon_0);

    //orthorombic
    crystal.setLattice(1., 3., 4.,
                       90.* constant::math::degToRad, 90.* constant::math::degToRad, 90.* constant::math::degToRad);
    crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 1., a, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 3., b, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 4., c, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, alpha, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, beta, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, gamma, constant::math::epsilon_0);

    // hexagonal1
    crystal.setLattice(1., 2., 1.,
                       90.* constant::math::degToRad, 120.* constant::math::degToRad, 90.* constant::math::degToRad);
    crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 2./sqrt(3.), a, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 2., b, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 2./sqrt(3.), c, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, alpha, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60. *  constant::math::degToRad, beta, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90. *  constant::math::degToRad, gamma, constant::math::epsilon_0);

    // hexagonal2
    crystal.setLattice(2., 1., 1.,
                       120.* constant::math::degToRad, 90.* constant::math::degToRad, 90.* constant::math::degToRad);
    crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau / 2., a, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 2. / sqrt(3.), b, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 2. / sqrt(3.), c, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(60.* constant::math::degToRad, alpha, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90.* constant::math::degToRad, beta, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(90.* constant::math::degToRad, gamma, constant::math::epsilon_0);

    // triclinic1
    crystal.setLattice(9.32, 8.24, 13.78,
                       91.23* constant::math::degToRad, 93.64* constant::math::degToRad, 122.21* constant::math::degToRad);
    crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 0.1273130168, a, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 0.1437422974, b, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 0.0728721120, c, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1.5052513337, alpha, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1.482101482, beta, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1.0055896011, gamma, constant::math::epsilon_0);

    // triclinic2
    crystal.setLattice(18.423, 18.417, 18.457,
                       89.99* constant::math::degToRad, 89.963* constant::math::degToRad, 119.99* constant::math::degToRad);
    crystal.getReciprocalLattice(&a, &b, &c, &alpha, &beta, &gamma);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 0.0626708259, a, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 0.0626912310, b, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(constant::physic::tau * 0.0541800061, c, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1.5713705262, alpha, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1.5716426508, beta, constant::math::epsilon_0);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(1.0473718249, gamma, constant::math::epsilon_0);
}

void
SampleTest::PlusEqual(void)
{
    double a, b, c, alpha, beta, gamma;

    Crystal<geometry::eulerian4C::Vertical> C1("crystal1");
    Crystal<geometry::eulerian4C::Vertical> C2("crystal2");
    C1.setLattice(1., 1., 1., 1., 2., 3.);
    C2.setLattice(2., 3., 4., 5., 6., 7.);

    //C1 += C2;
    C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

    CPPUNIT_ASSERT_EQUAL(3., a);
    CPPUNIT_ASSERT_EQUAL(4., b);
    CPPUNIT_ASSERT_EQUAL(5., c);
    CPPUNIT_ASSERT_EQUAL(6., alpha);
    CPPUNIT_ASSERT_EQUAL(8., beta);
    CPPUNIT_ASSERT_EQUAL(10., gamma);

    //C1 += C1;
    C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

    CPPUNIT_ASSERT_EQUAL(6., a);
    CPPUNIT_ASSERT_EQUAL(8., b);
    CPPUNIT_ASSERT_EQUAL(10., c);
    CPPUNIT_ASSERT_EQUAL(12., alpha);
    CPPUNIT_ASSERT_EQUAL(16., beta);
    CPPUNIT_ASSERT_EQUAL(20., gamma);
}

void
SampleTest::MinusEqual(void)
{
    double a, b, c, alpha, beta, gamma;

    Crystal<geometry::eulerian4C::Vertical> C1("crystal1");
    Crystal<geometry::eulerian4C::Vertical> C2("crystal2");
    C1.setLattice(1., 1., 1., 1., 2., 3.);
    C2.setLattice(2., 3., 4., 5., 6., 7.);

    //C1 -= C2;
    C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

    CPPUNIT_ASSERT_EQUAL(-1., a);
    CPPUNIT_ASSERT_EQUAL(-2., b);
    CPPUNIT_ASSERT_EQUAL(-3., c);
    CPPUNIT_ASSERT_EQUAL(-4., alpha);
    CPPUNIT_ASSERT_EQUAL(-4., beta);
    CPPUNIT_ASSERT_EQUAL(-4., gamma);

    //C1 -= C1;
    C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

    CPPUNIT_ASSERT_EQUAL(0., a);
    CPPUNIT_ASSERT_EQUAL(0., b);
    CPPUNIT_ASSERT_EQUAL(0., c);
    CPPUNIT_ASSERT_EQUAL(0., alpha);
    CPPUNIT_ASSERT_EQUAL(0., beta);
    CPPUNIT_ASSERT_EQUAL(0., gamma);
}

void
SampleTest::TimesEqual(void)
{
    double a, b, c, alpha, beta, gamma;

    Crystal<geometry::eulerian4C::Vertical> C1("crystal1");
    C1.setLattice(1., 1., 1., 1., 2., 3.);

    //C1 *= 2.;
    C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

    CPPUNIT_ASSERT_EQUAL(2., a);
    CPPUNIT_ASSERT_EQUAL(2., b);
    CPPUNIT_ASSERT_EQUAL(2., c);
    CPPUNIT_ASSERT_EQUAL(2., alpha);
    CPPUNIT_ASSERT_EQUAL(4., beta);
    CPPUNIT_ASSERT_EQUAL(6., gamma);
}

void
SampleTest::DivideEqual(void)
{
    double a, b, c, alpha, beta, gamma;

    Crystal<geometry::eulerian4C::Vertical> C1("crystal1");
    C1.setLattice(1., 1., 1., 1., 2., 3.);

    //C1 /= 2.;
    C1.getLattice(&a, &b, &c, &alpha, &beta, &gamma);

    CPPUNIT_ASSERT_EQUAL(.5, a);
    CPPUNIT_ASSERT_EQUAL(.5, b);
    CPPUNIT_ASSERT_EQUAL(.5, c);
    CPPUNIT_ASSERT_EQUAL(.5, alpha);
    CPPUNIT_ASSERT_EQUAL(1., beta);
    CPPUNIT_ASSERT_EQUAL(1.5, gamma);
}

void
SampleTest::ReflectionPart()
{
    Crystal<geometry::eulerian4C::Vertical> crystal("crystal1");

    //add
    // test if there is an exception if the source is not properly set by using a non initialize geometry.
    geometry::eulerian4C::Vertical geometry;
    Reflection<geometry::eulerian4C::Vertical> reflection(geometry,
                                                          0.,0.,1.,
                                                          Best, true);
    CPPUNIT_ASSERT_THROW(crystal.addReflection(reflection), HKLException);

    // add
    // test if there is no exception when the source is correctly set.
    reflection = Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C,
                                                            0.,0.,1.,
                                                            Best, true);
    CPPUNIT_ASSERT_NO_THROW(crystal.addReflection(reflection));

    //test if the last reflection have the flag set to false.
    // we can not have two identical reflection (h1, k1, l1) == (h2, k2, l2) active
    // for calculation.
    crystal.addReflection(reflection); 
    CPPUNIT_ASSERT_EQUAL(false, crystal.getReflection(1).get_flag());

    // get
    CPPUNIT_ASSERT_NO_THROW(crystal.getReflection(1));
    CPPUNIT_ASSERT_NO_THROW(crystal.getReflection(0));
    CPPUNIT_ASSERT_THROW(crystal.getReflection(2), HKLException);

    //del
    CPPUNIT_ASSERT_NO_THROW(crystal.delReflection(1));
    CPPUNIT_ASSERT_NO_THROW(crystal.delReflection(0));
    CPPUNIT_ASSERT_THROW(crystal.delReflection(0), HKLException);

    //delAllReflection
    crystal.addReflection(reflection);
    crystal.addReflection(reflection);
    crystal.addReflection(reflection);
    CPPUNIT_ASSERT_EQUAL((unsigned int)3, crystal.getReflectionListSize());
    crystal.delAllReflections();
    CPPUNIT_ASSERT_EQUAL((unsigned int)0, crystal.getReflectionListSize());
}

void
SampleTest::ComputeB()
{
    smatrix matrice(constant::physic::tau/1.54, 0.                        , 0.,
                    0.                        , constant::physic::tau/1.54, 0.,
                    0.                        , 0.                        , constant::physic::tau/1.54);

    CPPUNIT_ASSERT_EQUAL(matrice, m_crystal.get_B());
}

void
SampleTest::isEnoughReflections(void)
{
    Reflection<geometry::eulerian4C::Vertical> reflection(m_geometry_E4C,
                                                          0., 0., 1.,
                                                          Best, true);

    m_crystal.addReflection(reflection);

    // this 2nd identical reflection will be set to false
    // so not comtabilize for the calculation.
    m_crystal.addReflection(reflection);

    reflection = Reflection<geometry::eulerian4C::Vertical>(m_geometry_E4C,
                                                            -1., 0., 0.,
                                                            Best, true);

    m_crystal.addReflection(reflection);

    CPPUNIT_ASSERT_EQUAL(true, m_crystal.isEnoughReflections(0));
    CPPUNIT_ASSERT_EQUAL(true, m_crystal.isEnoughReflections(1));
    CPPUNIT_ASSERT_EQUAL(true, m_crystal.isEnoughReflections(2));
    CPPUNIT_ASSERT_EQUAL(false, m_crystal.isEnoughReflections(3));
}

*/

void
SampleTest::ComputeU()
{
    hkl::sample::MonoCrystal & sample = dynamic_cast<hkl::sample::MonoCrystal &>(*_sample);

    smatrix M(1., 0., 0.,
              0., 1., 0.,
              0., 0., 1.);

    // without reflection an exception is throw
    CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

    // add a non valid for computation reflection
    sample.reflections().add(0, 0, 0);
    CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

    // add a second non valid for computation reflection
    sample.reflections().add(0, 0, 0);
    CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

    sample.reflections().del(1);
    sample.reflections().del(0);

    //with only one valid reflection exception
    _geometry.setAngles(30.* constant::math::degToRad,
                        0.* constant::math::degToRad,
                        0.* constant::math::degToRad,
                        60.* constant::math::degToRad);
    sample.reflections().add(0, 0, 1);
    CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

    //with two valid reflection, no exception
    _geometry.setAngles(30.* constant::math::degToRad,
                        0.* constant::math::degToRad,
                        -90.* constant::math::degToRad,
                        60.* constant::math::degToRad);
    sample.reflections().add(-1, 0, 0);
    CPPUNIT_ASSERT_NO_THROW(sample.computeU(0, 1));
    CPPUNIT_ASSERT_EQUAL(M, sample.get_U());

    // exception if not enough reflections.
    sample.reflections().del(1);
    CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);
    sample.reflections().del(0);
    CPPUNIT_ASSERT_THROW(sample.computeU(0, 1), HKLException);

    // test with two other reflections.
    _geometry.setAngles(30.* constant::math::degToRad,
                        0.* constant::math::degToRad,
                        90.* constant::math::degToRad,
                        60.* constant::math::degToRad);
    sample.reflections().add(1, 0, 0);

    _geometry.setAngles(30.* constant::math::degToRad,
                        0.* constant::math::degToRad,
                        180.* constant::math::degToRad,
                        60.* constant::math::degToRad);
    sample.reflections().add(0, 1, 0);
    CPPUNIT_ASSERT_NO_THROW(sample.computeU(0, 1));
    M.set(1., 0., 0.,
          0., 0., 1.,
          0.,-1., 0.);
    CPPUNIT_ASSERT_EQUAL(M, sample.get_U());
}

void
SampleTest::Fitness()
{
    hkl::sample::MonoCrystal & sample = dynamic_cast<hkl::sample::MonoCrystal &>(*_sample);

    smatrix M(1., 0., 0.,
              0., 1., 0.,
              0., 0., 1.);

    CPPUNIT_ASSERT_THROW(sample.fitness(), HKLException);

    _geometry.setAngles(30.* constant::math::degToRad,
                        0.* constant::math::degToRad,
                        0.* constant::math::degToRad,
                        60.* constant::math::degToRad);
    sample.reflections().add(0,0,1);
    _geometry.setAngles(30.* constant::math::degToRad,
                        0.* constant::math::degToRad,
                        -90.* constant::math::degToRad,
                        60.* constant::math::degToRad);
    sample.reflections().add(-1, 0, 0);

    sample.computeU(0, 1);
    CPPUNIT_ASSERT_DOUBLES_EQUAL(0., sample.fitness(), constant::math::epsilon_1);
}

void
SampleTest::persistanceIO(void)
{
    hkl::Sample * crystal_ref = new hkl::sample::MonoCrystal(_geometry, "tutu");
    hkl::Sample * crystal1_ref = new hkl::sample::MonoCrystal(_geometry, "toto");
    hkl::Sample * crystal = new hkl::sample::MonoCrystal(_geometry, "toto");
    hkl::Sample * crystal1 = new hkl::sample::MonoCrystal(_geometry, "tutu");
    stringstream flux;

    // set the lattice of the crystal_ref
    Lattice & lattice = crystal_ref->lattice();
    lattice.a().set_current(1);
    lattice.b().set_current(1);
    lattice.c().set_current(1.54);
    lattice.alpha().set_current(90 * constant::math::degToRad);
    lattice.beta().set_current(90 * constant::math::degToRad);
    lattice.gamma().set_current(90 * constant::math::degToRad);

    // set the lattice of the crystal1_ref
    lattice = crystal1_ref->lattice();
    lattice.a().set_current(1);
    lattice.b().set_current(2.);
    lattice.c().set_current(1.54);
    lattice.alpha().set_current(60 * constant::math::degToRad);
    lattice.beta().set_current(90 * constant::math::degToRad);
    lattice.gamma().set_current(91 * constant::math::degToRad);

    crystal_ref->toStream(flux);
    crystal1_ref->toStream(flux);  
    crystal->fromStream(flux);
    crystal1->fromStream(flux);

    CPPUNIT_ASSERT_EQUAL(*crystal1_ref, *crystal1);
    CPPUNIT_ASSERT_EQUAL(*crystal_ref, *crystal);
}
