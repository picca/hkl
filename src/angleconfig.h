//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/angleconfig.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class angleConfiguration, eulerian_angleConfiguration4C, kappa_angleConfiguration4C

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.10 $

//

// $Log: angleconfig.h,v $
// Revision 1.10  2005/02/10 16:47:04  picca
// documentation update
//
// Revision 1.9  2005/01/27 09:23:53  delos
// Commentaires pour CVS en tete des fichiers
//

//

//

// copyleft :       Synchrotron SOLEIL

//                  L'Orme des Merisiers

//                  Saint-Aubin - BP 48

//                  91192 GIF-sur-YVETTE CEDEX

//

//-======================================================================
/// Store the current angle configuration according to the type of diffractometer.

#ifndef _ANGLECONFIG_H_
#define _ANGLECONFIG_H_


/// The base class to represent the different kinds of configurations whatever the diffractometer type is.
/**
 * \brief Store the current angle configuration according to the type of diffractometer.
 * 
 * This class will be derived depending of the geometry of the diffractometers.
 * this class is an "abstraite" class.
 */
class angleConfiguration
{
protected:
  angleConfiguration() {}

public:
  virtual void printOnScreen()=0;
  virtual void printStaticOnScreen()=0;
  virtual angleConfiguration* makeCopy() const =0;
};

/**
 * \brief A set of four angles to define the crystal and detector positions.
 * 
 * The 4C eulerian diffractometer is defined by a set of 3 angles omega, chi and phi to move the 
 * crystal and also a fourth angle to move the detector by 2theta. Angles are in radians.
 * Conventions are from William R. Busing and Henri A. Levy "Angle calculation 
 * for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
 * <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
 */
class eulerian_angleConfiguration4C : public angleConfiguration
{
protected:
  double m_omega; //!< The first angle in an eulerian 4-circle diffractometer.
  double m_chi; //!< The second angle in an eulerian 4-circle diffractometer.
  double m_phi; //!< The third angle in an eulerian 4-circle diffractometer.
  double m_2theta; //!< The detector angle in an eulerian 4-circle diffractometer.
  static double m_omegaInf; //!< The first angle lower bound.
  static double m_omegaSup; //!< The first angle upper bound.
  static double m_chiInf; //!< The second angle lower bound.
  static double m_chiSup; //!< The second angle upper bound.
  static double m_phiInf; //!< The third angle lower bound.
  static double m_phiSup; //!< The third angle upper bound.
  static double m_2thetaInf; //!< The detector angle lower bound.
  static double m_2thetaSup; //!< The detector angle upper bound.

public:

  /**
   * \brief constructor
   *
   * Empty constructor setting all the fields to zero.
   */
  eulerian_angleConfiguration4C();

  /**
   * \brief Constructor
   * @param omega The \f$\omega\f$ angle
   * @param chi The \f$\chi\f$ angle
   * @param phi The \f$\phi\f$ angle
   * @param theta The \f$\theta\f$ angle
   * 
   * Constructor with initialization of the angles parameters
   */
  eulerian_angleConfiguration4C(double omega, double chi, double phi, double theta);

  /**
   * \brief constructor
   * @param omega The \f$\omega\f$ angle
   * @param chi The \f$\chi\f$ angle
   * @param phi The \f$\phi\f$ angle
   * @param theta The \f$\theta\f$ angle
   * @param omega_inf The \f$\omega\f$ lower bound
   * @param omega_sup The \f$\omega\f$ upper bound
   * @param chi_inf The \f$\chi\f$ lower bound
   * @param chi_sup The \f$\chi\f$ upper bound
   * @param phi_inf The \f$\phi\f$ lower bound
   * @param phi_sup The \f$\phi\f$ upper bound
   * @param theta_inf The \f$\theta\f$ lower bound
   * @param theta_sup The \f$\theta\f$ upper bound
   * 
   * Constructor with an already made configuration and the angle intervals.
   */
  eulerian_angleConfiguration4C(
    double omega, double chi, double phi, double theta,
    double omega_inf, double omega_sup, double chi_inf, double chi_sup,
    double phi_inf, double phi_sup, double theta_inf, double theta_sup);

  /**
   * \brief This redefined function builds a copy of the class.
   * 
   * This redefined function builds a copy of the class.
   */
  virtual angleConfiguration* makeCopy() const;

  
  double  getOmega() const            {return m_omega;} //!< Get \f$\omega\f$ angle.
  double  getChi() const              {return m_chi;} //!< Get \f$\chi\f$ angle.
  double  getPhi() const              {return m_phi;} //!< Get \f$\phi\f$ angle.
  double  get2Theta() const           {return m_2theta;} //!< Get \f$\theta\f$ angle.
  void    setOmega(double omega)      {m_omega = omega;} //!< Set \f$\omega\f$ angle.
  void    setChi(double chi)          {m_chi = chi;} //!< Set \f$\chi\f$ angle.
  void    setPhi(double phi)          {m_phi = phi;} //!< Set \f$\phi\f$ angle.
  void    set2Theta(double two_theta) {m_2theta = two_theta;} //!< Set \f$\theta\f$ angle.

  static void setOmegaInf(double o)  {m_omegaInf = o;} //!< Set \f$\omega\f$ lower bound.
  static void setOmegaSup(double o)  {m_omegaSup = o;} //!< Set \f$\omega\f$ upper bound.
  static void setChiInf(double c)    {m_chiInf = c;} //!< Set \f$\chi\f$ lower bound.
  static void setChiSup(double c)    {m_chiSup = c;} //!< Set \f$\chi\f$ upper bound.
  static void setPhiInf(double p)    {m_phiInf = p;} //!< Set \f$\phi\f$ lower bound.
  static void setPhiSup(double p)    {m_phiSup = p;} //!< Set \f$\phi\f$ upper bound.
  static void set2ThetaInf(double t) {m_2thetaInf = t;} //!< Set \f$\theta\f$ lower bound.
  static void set2ThetaSup(double t) {m_2thetaSup = t;} //!< Set \f$\theta\f$ upper bound.

  virtual void printOnScreen();//!< Print the angle values in radians.
  virtual void printDegreesOnScreen();//!< Print the angle values in degrees.
  virtual void printStaticOnScreen();//!< Print only static fields.
};


/**
 * \brief A set of six angles to define the crystal and detector positions.
 *
 * The 6C eulerian diffractometer is defined by a set of 4 angles mu, eta, chi and phi to move the 
 * crystal and also nu and delta to move the detector. Angles are in radians. The angle omega
 * previously defined in a 4-circle is now called eta and the detector angle 2theta has been
 * renamed delta according to conventions from H. You "Angle calculations for a `4S+2D' six-circle
 * diffractometer" (1999) <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
 */
class eulerian_angleConfiguration6C : public eulerian_angleConfiguration4C
{
protected:
  double m_mu; //!< The new crystal angle in an eulerian 6-circle diffractometer.
  double m_nu; //!< The new detector angle in an eulerian 6-circle diffractometer.
  static double m_muInf; //!< \f$mu\f$ angle lower bound.
  static double m_muSup; //!< \f$mu\f$ angle upper bound.
  static double m_nuInf; //!< \f$nu\f$ angle lower bound.
  static double m_nuSup; //!< \f$nu\f$ angle upper bound.

public:

  /**
   * \brief Empty constructor setting all the fields to zero.
   *
   * Empty constructor setting all the fields to zero.
   */
  eulerian_angleConfiguration6C();

  /**
   * \brief Constructor with an already made configuration.
   * @param mu The \f$\mu\f$ angle
   * @param eta The \f$\eta\f$ angle
   * @param chi The \f$\chi\f$ angle
   * @param phi The \f$\phi\f$ angle
   * @param nu The \f$\nu\f$ angle
   * @param delta The \f$\delta\f$ angle
   *
   *  Constructor with an already made configuration.
   */
  eulerian_angleConfiguration6C(double mu,double eta,double chi,double phi,double nu,double delta);

  /**
   * \brief Constructor with an already made configuration and the angle intervals.
   * @param mu The \f$\mu\f$ angle
   * @param eta The \f$\eta\f$ angle
   * @param chi The \f$\chi\f$ angle
   * @param phi  The \f$\phi\f$ angle
   * @param nu The \f$\nu\f$ angle
   * @param delta The \f$\delta\f$ angle
   * @param mu_inf  The \f$\mu\f$ lower bound
   * @param mu_sup  The \f$\mu\f$ upper bound
   * @param eta_inf The \f$\eta\f$ lower bound
   * @param eta_sup The \f$\eta\f$ upper bound
   * @param chi_inf The \f$\chi\f$ lower bound
   * @param chi_sup The \f$\chi\f$ upper bound
   * @param phi_inf The \f$\phi\f$ lower bound
   * @param phi_sup The \f$\phi\f$ upper bound
   * @param nu_inf  The \f$\nu\f$ lower bound
   * @param nu_sup  The \f$\nu\f$ upper bound
   * @param delta_inf The \f$\delta\f$ lower bound
   * @param delta_sup The \f$\delta\f$ upper bound
   *
   *  Constructor with an already made configuration and the angle intervals.
   */
  eulerian_angleConfiguration6C(
    double mu, double eta, double chi, double phi, double nu, double delta,
    double mu_inf,double mu_sup,double eta_inf,double eta_sup,double chi_inf,double chi_sup,
    double phi_inf,double phi_sup,double nu_inf,double nu_sup,double delta_inf,double delta_sup);

  /// \brief This redefined function builds a copy of the class.
  virtual angleConfiguration* makeCopy() const;

  double getMu() const          {return m_mu;} //!< Get \f$\mu\f$ angle.
  double getNu() const          {return m_nu;} //!< Get \f$nu\f$ angle.
  void   setMu(double mu)       {m_mu = mu;} //!< Set \f$mu\f$ angle.
  void   setNu(double nu)       {m_nu = nu;} //!< Set \f$nu\f$ angle.
  double getEta() const         {return m_omega;} //!< Use the protected field \f$\omega\f$ as \f$eta\f$ (H. You conventions).
  void   setEta(double eta)     {m_omega = eta;} //!< Use the protected field \f$\omega\f$ as \f$eta\f$ (H. You conventions).
  double getDelta() const       {return m_2theta;} //!< Use the protected field \f$2\theta\f$ as \f$delta\f$ (H. You conventions).
  void   setDelta(double delta) {m_2theta = delta;} //!< Use the protected field \f$2\theta\f$ as \f$delta\f$ (H. You conventions).

  static void setMuInf(double m)    {m_muInf = m;} //!< Set \f$\mu\f$ lower bound.
  static void setMuSup(double m)    {m_muSup = m;} //!< Set \f$\mu\f$ upper bound.
  static void setNuInf(double n)    {m_nuInf = n;} //!< Set \f$\nu\f$ lower bound.
  static void setNuSup(double n)    {m_nuSup = n;} //!< Set \f$\nu\f$ upper bound.
  static void setEtaInf(double e)   {m_omegaInf = e;} //!< Use the protected field \f$\omega\f$ as \f$\eta\f$.
  static void setEtaSup(double e)   {m_omegaSup = e;} //!< Use the protected field \f$\omega\f$ as \f$\eta\f$.
  static void setDeltaInf(double t) {m_2thetaInf = t;} //!< Use the protected field \f$2\theta\f$ as \f$\delta\f$.
  static void setDeltaSup(double t) {m_2thetaSup = t;} //!< Use the protected field \f$2\theta\f$ as \f$\delta\f$.

  virtual void printOnScreen(); //!< Print the angle values in radians.
  virtual void printDegreesOnScreen(); //!< Print the angle values in degrees.
  virtual void printStaticOnScreen(); //!< Print only static fields.

  static int test_angle6C();

private:
  
  /**
   * \brief This function is private to make sure we can't use it at this level as H. You conventions are in use.
   * @return m_omega
   */
  double getOmega() const {return m_omega;}

  /**
   * \brief This function is private to make sure we can't use it at this level as H. You conventions are in use.
   * @param omega the \f$\omega\f$ angle
   */
  void setOmega(double omega) {m_omega = omega;}

  /**
   * \brief This function is private to make sure we can't use it at this level as H. You conventions are in use.
   * @param o The lower bound of \f$\omega\f$
   */
  static void setOmegaInf(double o) {m_omegaInf = o;}

  /**
   * \brief This function is private to make sure we can't use it at this level as H. You conventions are in use.
   * @param o The upper bound of \f$\omega\f$
   */
  static void setOmegaSup(double o) {m_omegaSup = o;}


};


/**
 * A space position in a 4C Kappa diffractometer is also defined by a set of of three angles omega, kappa and phi
 * but the geometry axes are different. The fourth angle to move the detector is 2theta. Angles are in radians.
 */
class kappa_angleConfiguration4C : public angleConfiguration
{
protected:
  double m_omega; //!<The \f$\omega\f$ angle
  double m_kappa; //!<The \f$\kappa\f$ angle
  double m_phi; //!<The \f$\phi\f$ angle
  double m_2theta; //!<The \f$2\theta\f$ angle
  static double m_omegaInf; //!< The \f$\omega\f$ angle lower bound.
  static double m_omegaSup; //!< The \f$\omega\f$ angle upper bound.
  static double m_kappaInf; //!< The \f$\kappa\f$ angle lower bound.
  static double m_kappaSup; //!< The \f$\kappa\f$ angle upper bound.
  static double m_phiInf; //!< The \f$\phi\f$ angle lower bound.
  static double m_phiSup; //!< The \f$\phi\f$ angle upper bound.
  static double m_2thetaInf; //!< The \f$2\theta\f$ angle lower bound.
  static double m_2thetaSup; //!< The \f$2\theta\f$ angle upper bound.

public:

  /**
   * \brief Empty constructor which sets everything to zero.
   *
   * Empty constructor which sets everything to zero.
   */
  kappa_angleConfiguration4C();

  /**
   * \brief Constructor with an already made Configuration.
   * @param omega The \f$\omega\f$ angle
   * @param kappa The \f$\kappa\f$ angle
   * @param phi The \f$\phi\f$ angle
   * @param two_theta The \f$2\theta\f$ angle
   *
   *  Constructor with an already made Configuration.
   */
  kappa_angleConfiguration4C(double omega, double kappa, double phi, double two_theta);

  /**
   * \brief Constructor with an already made configuration and the angle intervals.
   * @param o The \f$\omega\f$ angle
   * @param c The \f$\kappa\f$ angle
   * @param p The \f$\phi\f$ angle
   * @param t The \f$2\theta\f$ angle
   * @param oi The \f$\omega\f$ angle lower bound.
   * @param os The \f$\omega\f$ angle upper bound.
   * @param ci The \f$\kappa\f$ angle lower bound.
   * @param cs The \f$\kappa\f$ angle upper bound.
   * @param pi The \f$\phi\f$ angle lower bound.
   * @param ps The \f$\phi\f$ angle upper bound.
   * @param ti The \f$2\theta\f$ angle lower bound.
   * @param ts The \f$2\theta\f$ angle upper bound.
   *
   * Constructor with an already made configuration and the angle intervals.
   */
  kappa_angleConfiguration4C(
    double o, double c, double p, double t,
    double oi,double os,double ci,double cs,
    double pi,double ps,double ti,double ts);

  /**
   * \brief This redefined function builds a copy of the class.
   *
   *  This redefined function builds a copy of the class.
   */           
  angleConfiguration* makeCopy() const;
                
  double getOmega() const   {return m_omega;} //!< Get \f$\omega\f$ angle.
  double getKappa() const   {return m_kappa;}//!< Get \f$\kappa\f$ angle.
  double getPhi() const     {return m_phi;} //!< Get \f$\phi\f$ angle.
  double get2Theta() const  {return m_2theta;} //!< Get \f$2\theta\f$ angle.
  
  void setOmega(double omega)       {m_omega = omega;} //!< Set \f$\omega\f$ angle.
  void setKappa(double kappa)       {m_kappa = kappa;} //!< Set \f$\kappa\f$ angle.
  void setPhi(double phi)           {m_phi = phi;} //!< Set \f$\phi\f$ angle.
  void set2Theta(double two_theta)  {m_2theta = two_theta;} //!< Set \f$2\theta\f$ angle.

  static void setOmegaInf(double o)   {m_omegaInf = o;} //!< Set the \f$\omega\f$ angle lower bound.
  static void setOmegaSup(double o)   {m_omegaSup = o;} //!< Set the \f$\omega\f$ angle upper bound.
  static void setKappaInf(double k)   {m_kappaInf = k;} //!< Set the \f$\kappa\f$ angle lower bound.
  static void setKappaSup(double k)   {m_kappaSup = k;} //!< Set the \f$\kappa\f$ angle upper bound.
  static void setPhiInf(double p)     {m_phiInf = p;} //!< Set the \f$\phi\f$ angle lower bound.
  static void setPhiSup(double p)     {m_phiSup = p;} //!< Set the \f$\phi\f$ angle upper bound.
  static void set2ThetaInf(double t)  {m_2thetaInf = t;} //!< Set the \f$2\theta\f$ angle lower bound.
  static void set2ThetaSup(double t)  {m_2thetaSup = t;} //!< Set the \f$2\theta\f$ angle upper bound.

  void printOnScreen(); //!< print on stdout all the angle configuration.
  void printStaticOnScreen(); //!< print on stdout only the static fields.
};

#endif // _ANGLECONFIG_H_
