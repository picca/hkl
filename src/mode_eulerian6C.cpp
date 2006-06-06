#include "mode_eulerian6C.h"

namespace hkl {
    namespace mode {
        namespace eulerian6C {
            namespace eulerian4C {

                Vertical::Vertical(void)
                  {}

                Vertical::~Vertical(void)
                  {}

                namespace vertical {

                    /*************************/
                    /* VERTICAL 4C BISSECTOR */
                    /*************************/
                    Bissector::Bissector(void) :
                      mode::eulerian6C::eulerian4C::Vertical(),
#ifdef MSVC6
                      Mode()
#else
                      mode::eulerian4C::vertical::Bissector()
#endif
                        {
                          set_name("Vertical Eulerian 4C Bissector");
#ifdef MSVC6
                          set_valueList(m_mode.get_valueList());
                          set_description(m_mode.get_description());
#endif
                        }

                    Bissector::~Bissector(void)
                      {}

                    void 
                    Bissector::computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException)
                      {
                        m_E4CV.setFromGeometry(geometry, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_E4CV);
#else
                        mode::eulerian4C::vertical::Bissector::computeAngles(h, k, l, UB, m_E4CV);
#endif
                        geometry.setFromGeometry(m_E4CV, true);
                      }

                    /***************************/
                    /* VERTICAL 4C DELTA THETA */
                    /***************************/
                    Delta_Theta::Delta_Theta(void) :
                      mode::eulerian6C::eulerian4C::Vertical(),
#ifdef MSVC6
                      Mode()
#else
                      mode::eulerian4C::vertical::Delta_Theta()
#endif
                        {
                          set_name("Vertical Eulerian 4C Delta Theta");
#ifdef MSVC6
                          set_valueList(m_mode.get_valueList());
                          set_description(m_mode.get_description());
#endif
                        }

                    Delta_Theta::~Delta_Theta(void)
                      {}

                    void 
                    Delta_Theta::computeAngles(double h, double k, double l,
                                               smatrix const & UB,
                                               Geometry & geometry) const throw (HKLException)
                      {
                        m_E4CV.setFromGeometry(geometry, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_E4CV);
#else
                        mode::eulerian4C::vertical::Delta_Theta::computeAngles(h, k, l, UB, m_E4CV);
#endif
                        geometry.setFromGeometry(m_E4CV, true);
                      }

                    /******************************/
                    /* VERTICAL 4C CONSTANT OMEGA */
                    /******************************/
                    Constant_Omega::Constant_Omega(void) :
                      mode::eulerian6C::eulerian4C::Vertical(),
#ifdef MSVC6
                      Mode()
#else
                      mode::eulerian4C::vertical::Constant_Omega()
#endif
                        {
                          set_name("Vertical Eulerian 4C Constant Omega");
#ifdef MSVC6
                          set_valueList(m_mode.get_valueList());
                          set_description(m_mode.get_description());
#endif
                        }

                    Constant_Omega::~Constant_Omega(void) {}

                    void 
                    Constant_Omega::computeAngles(double h, double k, double l,
                                                  smatrix const & UB,
                                                  Geometry & geometry) const throw (HKLException)
                      {
                        m_E4CV.setFromGeometry(geometry, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_E4CV);
#else
                        mode::eulerian4C::vertical::Constant_Omega::computeAngles(h, k, l, UB, m_E4CV);
#endif
                        geometry.setFromGeometry(m_E4CV, true);
                      }

                    /****************************/
                    /* VERTICAL 4C CONSTANT CHI */
                    /****************************/
                    Constant_Chi::Constant_Chi(void) :
                      mode::eulerian6C::eulerian4C::Vertical(),
#ifdef MSVC6
                      Mode()
#else
                      mode::eulerian4C::vertical::Constant_Chi()
#endif
                        {
                          set_name("Vertical Eulerian 4C Constant Chi");
#ifdef MSVC6
                          set_valueList(m_mode.get_valueList());
                          set_description(m_mode.get_description());
#endif
                        }

                    Constant_Chi::~Constant_Chi(void) {}

                    void 
                    Constant_Chi::computeAngles(double h, double k, double l,
                                                smatrix const & UB,
                                                Geometry & geometry) const throw (HKLException)
                      {
                        m_E4CV.setFromGeometry(geometry, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_E4CV);
#else
                        mode::eulerian4C::vertical::Constant_Chi::computeAngles(h, k, l, UB, m_E4CV);
#endif
                        geometry.setFromGeometry(m_E4CV, true);
                      }

                    /****************************/
                    /* VERTICAL 4C CONSTANT PHI */
                    /****************************/
                    Constant_Phi::Constant_Phi(void) :
                      mode::eulerian6C::eulerian4C::Vertical(),
#ifdef MSVC6
                      Mode()
#else
                      mode::eulerian4C::vertical::Constant_Phi()
#endif
                        {
                          set_name("Vertical Eulerian 4C Constant Phi");
#ifdef MSVC6
                          set_valueList(m_mode.get_valueList());
                          set_description(m_mode.get_description());
#endif
                        }

                    Constant_Phi::~Constant_Phi(void) {}

                    void 
                    Constant_Phi::computeAngles(double h, double k, double l,
                                                smatrix const & UB,
                                                Geometry & geometry) const throw (HKLException)
                      {
                        m_E4CV.setFromGeometry(geometry, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_E4CV);
#else
                        mode::eulerian4C::vertical::Constant_Phi::computeAngles(h, k, l, UB, m_E4CV);
#endif
                        geometry.setFromGeometry(m_E4CV, true);
                      }

                } // namespace vertical
            } // namespace eulerian4C

            /****************************/
            /* LIFTING 3C DETECTOR MODE */
            /****************************/
            lifting3CDetector::lifting3CDetector() {}

            lifting3CDetector::~lifting3CDetector() {}

            // Solving equation (11) from :
            // H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999)
            // Z11 * hphi1 + Z12 * hphi2 + Z13 * hphi3 = k0*sin(delta)
            // Z21 * hphi1 + Z22 * hphi2 + Z23 * hphi3 = k0*(cos(delta)*cos(nu)-1.)
            // Z31 * hphi1 + Z32 * hphi2 + Z33 * hphi3 = k0*cos(delta)*sin(nu)
            // where k0 = tau/lambda = q/2sin(theta) and
            // eta = chi = phi = 0.
            //
            // delta = arcsin(hphi1 / k0)
            // nu = arccos[(1-Q²/k0²)/(2cos(delta))]
            // sin(mu)*(hphi2²+hphi3²) =-hphi3*k0*(cos(delta)*cos(nu)-1)+hphi2*k0*sin(nu)*cos(delta)
            // cos(mu)*(hphi2²+hphi3²) = hphi2*k0*(cos(delta)*cos(nu)-1)+hphi3*k0*sin(nu)*cos(delta)
            void
            lifting3CDetector::computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException)
              {
                // h(theta) = R.hphi
                double k0;
                double mu;
                double gamma;
                double omega;
                double chi;
                double phi;
                double delta;
                double theta;
                double cos_delta;
                double sin_theta;
                double cos_2theta;
                svector hphi = UB * svector(h,k,l);
                svector hphi_unitVector = hphi.normalize();
                double hphi_length = hphi.norm2();
                double lambda = geometry.get_source().get_waveLength();

                if ((fabs(hphi[Y]) < constant::math::epsilon_1) &&
                    (fabs(hphi[Z]) < constant::math::epsilon_1))
                    HKLEXCEPTION("Unobtainable reflection",
                                 "The scattering vector is perpendicular to the light ray");

                if (fabs(lambda) < constant::math::epsilon_1)
                    HKLEXCEPTION("lamdba is null",
                                 "The wave length has not been set");

                if ((fabs(h) < constant::math::epsilon_1) && 
                    (fabs(k) < constant::math::epsilon_1) &&
                    (fabs(l) < constant::math::epsilon_1))
                    HKLEXCEPTION("(h,k,l) is null",
                                 "Check your parameters");

                if (hphi.norminf() < constant::math::epsilon_1)
                    HKLEXCEPTION("hphi is null",
                                 "The matrix U has been computed from two parallel reflections or the crystal matrix is null");

                k0 = constant::physic::tau / lambda;

                // By definition in lifting 3-circles detector mode.
                omega = 0.;
                chi = 0.;
                phi = 0.;

                /////////////////////
                // Bragg relation //
                ///////////////////
                // sin(theta) = || q || * lambda * 0.5 / tau.
                sin_theta = hphi_length * lambda * 0.5;
                // We have to be consistent with the conventions previously defined when we computed the crystal reciprocal lattice.
                sin_theta = sin_theta / constant::physic::tau;

                if (fabs(sin_theta) > 1.+constant::math::epsilon_1)
                    HKLEXCEPTION("sine bigger than 1.",
                                 "hphi_length too big, maybe error in UB matrix");

                theta = asin(sin_theta);

                cos_2theta = cos(2*theta);

                // STEP NUMBER 1 : COMPUTING MU
                // We know that cos(2*theta) = cos(delta).cos(nu) and 
                // hphi2.cos(mu) - hphi3.sin(mu) = (tau/lambda).[cos(2*theta)-1.]
                // So we solve the following system where a=hphi2 and b=-hphi3 and c=(tau/lambda).[cos(2*theta)-1.]
                // ax + by = c
                // x² + y² = 1.
                double a = hphi[Y];
                double b =-hphi[Z];
                double c = k0*(cos_2theta-1.);
                double det = a*a+b*b-c*c;

                if (fabs(det) < constant::math::epsilon_1)
                    det = 0.;
                if (det < -constant::math::epsilon_1)
                    HKLEXCEPTION("Unobtainable reflection",
                                 "Unreachable Bragg condition");

                double cos_mu = (a*c - b*sqrt(det))/(a*a+b*b);
                double sin_mu = (b*c + a*sqrt(det))/(a*a+b*b);
                if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+constant::math::epsilon_0) ||
                    (cos_mu*cos_mu + sin_mu*sin_mu < 1.-constant::math::epsilon_0))
                    HKLEXCEPTION("Unobtainable reflection",
                                 "Mu circle cannot reach the diffraction position");
                if ((fabs(cos_mu) < constant::math::epsilon_1) && (fabs(sin_mu) < constant::math::epsilon_1))
                    mu = 0.;
                else
                    mu = atan2(sin_mu,cos_mu);
                smatrix MU;
                // Matrix Mu
                //  | 1.     0.       0.  |
                //  | 0.  cos_mu  -sin_mu |
                //  | 0.  sin_mu   cos_mu |
                MU.set(
                       1.,    0.,      0.,
                       0., cos_mu, -sin_mu,
                       0., sin_mu,  cos_mu);

                // STEP NUMBER 2 : COMPUTING THE SCATTERING VECTOR Q
                // Q = MU.U.B.(h,k,l)
                svector Q = MU * hphi;

                // STEP NUMBER 3 : COMPUTING THE DIFFRACTED RAY Kf
                // Kf = Q + Ki where Ki = (tau/lambda).(0,1,0)
                svector Kf(Q[X], Q[Y]+k0, Q[Z]);

                // STEP NUMBER 4 : COMPUTING DELTA AND NU ORIENTATIONS
                // if (Kf)x > 0 then  0   < delta <  Pi
                // if (Kf)x < 0 then  Pi  < delta < 2Pi
                // if (Kf)y > 0 then -Pi/2< delta <  Pi/2
                // if (Kf)y < 0 then  Pi/2< delta < 3Pi/2
                // if (Kf)z > 0 then  0   <  nu   <  Pi
                // if (Kf)z > 0 then  Pi  <  nu   < 2Pi
                double sx = hphi[X] / k0;

                if (fabs(sx) > 1.+constant::math::epsilon_1)
                    HKLEXCEPTION("Unobtainable reflection, delta sine bigger than 1.",
                                 "hphi.getX() too big or (tau/lambda) too small, maybe error in UB matrix");

                delta = asin(sx);
                // asin() returns values between -PI/2. and PI/2. According to H. You conventions hphi 3rd component sign tells 
                // whether delta belongs or not to the other half of the circle i.e. between PI/2. and 3PI/2. Figure (1) in 
                // H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999) J. Appl. Cryst., 32, 614-623.
                if (Kf[Y] < - constant::math::epsilon_1)
                    delta = constant::math::pi - delta;

                double k02 = k0*k0;

                cos_delta = cos(delta);
                //double sin_delta = sin(delta);

                if (fabs(cos_delta) < constant::math::epsilon_1)
                  {
                    // delta = PI/2. or delta = -PI/2. any value of nu is acceptable, it will
                    // not change the detector position as it is located on nu rotation axis.
                    gamma = 0.;

                    geometry.get_axe("mu").set_value(mu);
                    geometry.get_axe("omega").set_value(omega);
                    geometry.get_axe("chi").set_value(chi);
                    geometry.get_axe("phi").set_value(phi);

                    geometry.get_axe("gamma").set_value(gamma);
                    geometry.get_axe("delta").set_value(delta);

                    return;
                  }

                double cc = (2*k02-hphi_length*hphi_length) / k02;
                cc = cc / (2*cos_delta);

                if (fabs(cc) > 1.)
                    HKLEXCEPTION("cos(nu) bigger than 1.",
                                 "cos(delta) may be small, the reflection is unreachable");

                gamma = acos(cc);
                if (Kf[Z] < -constant::math::epsilon_1)
                    gamma = -gamma;

                geometry.get_axe("mu").set_value(mu);
                geometry.get_axe("omega").set_value(omega);
                geometry.get_axe("chi").set_value(chi);
                geometry.get_axe("phi").set_value(phi);

                geometry.get_axe("gamma").set_value(gamma);
                geometry.get_axe("delta").set_value(delta);

                return;


                /*
                   double sx = hphi.get_X() / k0;

                   if (fabs(sx) > 1.+mathematicalConstants::getEpsilon1())
                   throw HKLException(
                   "Unobtainable reflection, delta sine bigger than 1.",
                   "hphi.getX() too big or (tau/lambda) too small, maybe error in UB matrix",
                   "eulerian_lifting3CDetectorMode6C::computeAngles()");

                   delta = asin(sx);
                // asin() returns values between -PI/2. and PI/2. According to H. You conventions hphi 3rd component sign tells 
                // whether delta belongs or not to the other half of the circle i.e. between PI/2. and 3PI/2. Figure (1) in 
                // H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999) J. Appl. Cryst., 32, 614-623.
                //if (hphi.get_Y() < -mathematicalConstants::getEpsilon1())
                //  delta = mathematicalConstants::getPI() - delta;

                double k02 = k0*k0;

                double cos_delta = cos(delta);
                //double sin_delta = sin(delta);

                if (fabs(cos_delta) < mathematicalConstants::getEpsilon1())
                {
                // delta = PI/2. or delta = -PI/2. any value of nu is acceptable, it will
                // not change the detector position as it is located on nu rotation axis.
                nu = 0.;

                double sin_mu = (hphi.get_Z()*k0)/(hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());
                double cos_mu =(-hphi.get_Y()*k0)/(hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());

                if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+mathematicalConstants::getEpsilon0()) ||
                (cos_mu*cos_mu + sin_mu*sin_mu < 1.-mathematicalConstants::getEpsilon0()))
                throw HKLException(
                "Unobtainable reflection",
                "Mu circle cannot reach the diffraction position",
                "eulerian_lifting3CDetectorMode6C::computeAngles()");

                if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
                mu = 0.;
                else
                mu = atan2(sin_mu,cos_mu);

                ac6C = new angleConfiguration_Eulerian6C;
                ac6C->setDelta(delta);
                ac6C->setEta(eta);
                ac6C->setChi(chi);
                ac6C->setPhi(phi);
                ac6C->setNu(nu);
                ac6C->setMu(mu);

                return ac6C;
                }

                double cc = (2*k02-hphi_length*hphi_length) / k02;
                cc = cc / (2*cos_delta);

                if (fabs(cc) > 1.)
                throw HKLException(
                "cos(nu) bigger than 1.",
                "cos(delta) may be small, the reflection is unreachable",
                "eulerian_lifting3CDetectorMode6C::computeAngles()");

                nu = acos(cc);

                double cos_nu = cos(nu);
                double sin_nu = sin(nu);

                // We multiply by cos(chi) to "inject" its sign in s_phi and c_phi. 
                double sin_mu =-hphi.get_Z()*k0*(cos_delta*cos_nu-1.)+hphi.get_Y()*k0*sin_nu*cos_delta;
                double cos_mu = hphi.get_Y()*k0*(cos_delta*cos_nu-1.)+hphi.get_Z()*k0*sin_nu*cos_delta;
                sin_mu = sin_mu / (hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());
                cos_mu = cos_mu / (hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());

                if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+mathematicalConstants::getEpsilon0()) ||
                    (cos_mu*cos_mu + sin_mu*sin_mu < 1.-mathematicalConstants::getEpsilon0()))
                    throw HKLException(
                                       "Unobtainable reflection",
                                       "Mu circle cannot reach the diffraction position !",
                                       "eulerian_lifting3CDetectorMode6C::computeAngles()");

                if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
                    mu = 0.;
                else
                    mu = atan2(sin_mu,cos_mu);

                ac6C = new angleConfiguration_Eulerian6C;
                ac6C->setDelta(delta);
                ac6C->setEta(eta);
                ac6C->setChi(chi);
                ac6C->setPhi(phi);
                ac6C->setNu(nu);
                ac6C->setMu(mu);

                return ac6C;
                */


                /*
                /////////////////////
                // Bragg relation //
                ///////////////////
                // sin(theta) = || q || * lambda * 0.5 / tau.
                double sin_theta = hphi_length * lambda * 0.5;
                // We have to be consistent with the conventions 
                // previously defined when we computed the crystal 
                // reciprocal lattice.
                sin_theta = sin_theta / constant::physic::tau;

                if (fabs(sin_theta) > 1.+mathematicalConstants::getEpsilon1())
                throw HKLException(
                "sine bigger than 1.",
                "hphi_length too big, maybe error in UB matrix",
                "eulerian_lifting3CDetectorMode6C::computeAngles()");

                double theta = asin(sin_theta);

                double a = hphi.get_Y();
                double b =-hphi.get_Z();
                double c = k0*(cos(2*theta)-1.);
                double sqrt1 = sqrt(a*a+b*b-c*c);
                double cos_mu = (a*c-b*sqrt1)/(a*a+b*b);
                double sin_mu = (b*c+a*sqrt1)/(a*a+b*b);

                // Another solution.
                //double cos_mu = (a*c+b*sqrt1)/(a*a+b*b);
                //double sin_mu = (b*c-a*sqrt1)/(a*a+b*b);

                if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
                mu = 0.;
                else
                mu = atan2(sin_mu,cos_mu);





                double sx = hphi.get_X() / k0;

                if (fabs(sx) > 1.+mathematicalConstants::getEpsilon1())
                throw HKLException(
                "Unobtainable reflection, delta sine bigger than 1.",
                "hphi.getX() too big or (tau/lambda) too small, maybe error in UB matrix",
                "eulerian_lifting3CDetectorMode6C::computeAngles()");

                delta = asin(sx);
                // asin() returns values between -PI/2. and PI/2. According to H. You conventions hphi 3rd component sign tells 
                // whether delta belongs or not to the other half of the circle i.e. between PI/2. and 3PI/2. Figure (1) in 
                // H. You "Angle calculations for a `4S+2D' six-circle diffractometer" (1999) J. Appl. Cryst., 32, 614-623.
                //if (hphi.get_Y() < -mathematicalConstants::getEpsilon1())
                //  delta = mathematicalConstants::getPI() - delta;

                double k02 = k0*k0;

                double cos_delta = cos(delta);
                double sin_delta = sin(delta);

                if (fabs(cos_delta) < mathematicalConstants::getEpsilon1())
                {
                // delta = PI/2. or delta = -PI/2. any value of nu is acceptable, it will
                // not change the detector position as it is located on nu rotation axis.
                nu = 0.;

                double sin_mu = (hphi.get_Z()*k0)/(hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());
                double cos_mu =(-hphi.get_Y()*k0)/(hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());

                if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+mathematicalConstants::getEpsilon0()) ||
                (cos_mu*cos_mu + sin_mu*sin_mu < 1.-mathematicalConstants::getEpsilon0()))
                throw HKLException(
                "Unobtainable reflection",
                "Mu circle cannot reach the diffraction position",
                "eulerian_lifting3CDetectorMode6C::computeAngles()");

                if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
                    mu = 0.;
                else
                    mu = atan2(sin_mu,cos_mu);

                ac6C = new angleConfiguration_Eulerian6C;
                ac6C->setDelta(delta);
                ac6C->setEta(eta);
                ac6C->setChi(chi);
                ac6C->setPhi(phi);
                ac6C->setNu(nu);
                ac6C->setMu(mu);

                return ac6C;
              }

            double cc = (2*k02-hphi_length*hphi_length) / k02;
            cc = cc / (2*cos_delta);

            if (fabs(cc) > 1.)
                throw HKLException(
                                   "cos(nu) bigger than 1.",
                                   "cos(delta) may be small, the reflection is unreachable",
                                   "eulerian_lifting3CDetectorMode6C::computeAngles()");

            nu = acos(cc);

            double cos_nu = cos(nu);
            double sin_nu = sin(nu);

            // We multiply by cos(chi) to "inject" its sign in s_phi and c_phi. 
            double sin_mu =-hphi.get_Z()*k0*(cos_delta*cos_nu-1.)+hphi.get_Y()*k0*sin_nu*cos_delta;
            double cos_mu = hphi.get_Y()*k0*(cos_delta*cos_nu-1.)+hphi.get_Z()*k0*sin_nu*cos_delta;
            sin_mu = sin_mu / (hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());
            cos_mu = cos_mu / (hphi.get_Y()*hphi.get_Y()+hphi.get_Z()*hphi.get_Z());

            if ((cos_mu*cos_mu + sin_mu*sin_mu > 1.+mathematicalConstants::getEpsilon0()) ||
                (cos_mu*cos_mu + sin_mu*sin_mu < 1.-mathematicalConstants::getEpsilon0()))
                throw HKLException(
                                   "Unobtainable reflection",
                                   "Mu circle cannot reach the diffraction position !",
                                   "eulerian_lifting3CDetectorMode6C::computeAngles()");

            if ((fabs(cos_mu) < mathematicalConstants::getEpsilon1()) && (fabs(sin_mu) < mathematicalConstants::getEpsilon1()))
                mu = 0.;
            else
                mu = atan2(sin_mu,cos_mu);

            ac6C = new angleConfiguration_Eulerian6C;
            ac6C->setDelta(delta);
            ac6C->setEta(eta);
            ac6C->setChi(chi);
            ac6C->setPhi(phi);
            ac6C->setNu(nu);
            ac6C->setMu(mu);

            return ac6C;
            */
              }

        } // namesapce eulerian6C
    } // name space mode
} // namespace hkl
