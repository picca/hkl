/// Store the current angle configuration according to the type of diffractometer.

#ifndef ANGLECONFIG
#define ANGLECONFIG


/// The base class to represent the different kinds of configurations whatever the  diffractometer type is.
class angleConfiguration
{
protected:
  angleConfiguration()
  {}

public:
  virtual void printOnScreen()=0;
  virtual void printStaticOnScreen()=0;
  virtual angleConfiguration* makeCopy() const =0;
};

/// The 4C eulerian diffractometer is defined by a set of 3 angles omega, chi and phi to move the 
/// crystal and also a fourth angle to move the detector by 2theta. Angles are in radians.
/// Conventions are from William R. Busing and Henri A. Levy "Angle calculation 
/// for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
/// <A HREF="http://journals.iucr.org/index.html"> Acta Cryst.</A>, <B>22</B>, 457-464.
/// \brief A set of four angles to define the crystal and detector positions.
class eulerian_angleConfiguration4C : public angleConfiguration
{
protected:
  /// The first angle in an eulerian 4-circle diffractometer.
  double m_omega;
  /// The second angle in an eulerian 4-circle diffractometer.
  double m_chi;
  /// The third angle in an eulerian 4-circle diffractometer.
  double m_phi;
  /// The detector angle in an eulerian 4-circle diffractometer.
  double m_2theta;
  /// The first angle lower bound.
  static double m_omegaInf;
  /// The first angle upper bound.
  static double m_omegaSup;
  /// The second angle lower bound.
  static double m_chiInf;
  /// The second angle upper bound.
  static double m_chiSup;
  /// The third angle lower bound.
  static double m_phiInf;
  /// The third angle upper bound.
  static double m_phiSup;
  /// The detector angle lower bound.
  static double m_2thetaInf;
  /// The detector angle upper bound.
  static double m_2thetaSup;

public:

  /// \brief Empty constructor setting all the fields to zero.
  eulerian_angleConfiguration4C();

  /// \brief Constructor with an already made configuration.
  eulerian_angleConfiguration4C(double omega, double chi, double phi, double theta);

  /// \brief Constructor with an already made configuration and the angle intervals.
  eulerian_angleConfiguration4C(
    double omega, double chi, double phi, double theta,
    double omega_inf, double omega_sup, double chi_inf, double chi_sup,
    double phi_inf, double phi_sup, double theta_inf, double theta_sup);

  /// \brief This redefined function builds a copy of the class.
  virtual angleConfiguration* makeCopy() const;

  /// Get omega angle.
  double getOmega() const            {return m_omega;}
  /// Get chi angle.
  double getChi() const              {return m_chi;}
  /// Get phi angle.
  double getPhi() const              {return m_phi;}
  /// Get theta angle.
  double get2Theta() const           {return m_2theta;}
  /// Set omega angle.
  void setOmega(double omega)        {m_omega = omega;}
  /// Set chi angle.
  void setChi(double chi)            {m_chi = chi;}
  /// Set phi angle.
  void setPhi(double phi)            {m_phi = phi;}
  /// Set theta angle.
  void set2Theta(double two_theta)   {m_2theta = two_theta;}

  /// Set omega lower bound.
  static void setOmegaInf(double o)  {m_omegaInf = o;}
  /// Set omega upper bound.
  static void setOmegaSup(double o)  {m_omegaSup = o;}
  /// Set chi lower bound.
  static void setChiInf(double c)    {m_chiInf = c;}
  /// Set chi upper bound.
  static void setChiSup(double c)    {m_chiSup = c;}
  /// Set phi lower bound.
  static void setPhiInf(double p)    {m_phiInf = p;}
  /// Set phi upper bound.
  static void setPhiSup(double p)    {m_phiSup = p;}
  /// Set theta lower bound.
  static void set2ThetaInf(double t) {m_2thetaInf = t;}
  /// Set theta upper bound.
  static void set2ThetaSup(double t) {m_2thetaSup = t;}

  /// \brief Print the angle values in radians.
  virtual void printOnScreen();
  /// \brief Print the angle values in degrees.
  virtual void printDegreesOnScreen();
  /// \brief Print only static fields.
  virtual void printStaticOnScreen();
};

/// The 6C eulerian diffractometer is defined by a set of 4 angles mu, eta, chi and phi to move the 
/// crystal and also nu and delta to move the detector. Angles are in radians. The angle omega
/// previously defined in a 4-circle is now called eta and the detector angle 2theta has been
/// renamed delta according to conventions from H. You "Angle calculations for a `4S+2D' six-circle
/// diffractometer" (1999) <A HREF="http://journals.iucr.org/index.html"> J. Appl. Cryst.</A>, <B>32</B>, 614-623.
/// \brief A set of six angles to define the crystal and detector positions.
class eulerian_angleConfiguration6C : public eulerian_angleConfiguration4C
{
protected:
  /// The new crystal  angle in an eulerian 6-circle diffractometer.
  double m_mu;
  /// The new detector angle in an eulerian 6-circle diffractometer.
  double m_nu;
  /// mu angle lower bound.
  static double m_muInf;
  /// mu angle upper bound.
  static double m_muSup;
  /// nu angle lower bound.
  static double m_nuInf;
  /// nu angle upper bound.
  static double m_nuSup;

public:

  /// \brief Empty constructor setting all the fields to zero.
  eulerian_angleConfiguration6C();

  /// \brief Constructor with an already made configuration.
  eulerian_angleConfiguration6C(double mu,double eta,double chi,double phi,double nu,double delta);

  /// \brief Constructor with an already made configuration and the angle intervals.
  eulerian_angleConfiguration6C(
    double mu, double eta, double chi, double phi, double nu, double delta,
    double mu_inf,double mu_sup,double eta_inf,double eta_sup,double chi_inf,double chi_sup,
    double phi_inf,double phi_sup,double nu_inf,double nu_sup,double delta_inf,double delta_sup);

  /// \brief This redefined function builds a copy of the class.
  virtual angleConfiguration* makeCopy() const;

  /// Get mu angle.
  double getMu() const {return m_mu;}
  /// Get nu angle.
  double getNu() const {return m_nu;}
  /// Set mu angle.
  void   setMu(double mu) {m_mu = mu;}
  /// Set nu angle.
  void   setNu(double nu) {m_nu = nu;}
  /// Use the protected field omega as eta (H. You conventions).
  double getEta() const {return m_omega;}
  /// Use the protected field omega as eta (H. You conventions).
  void   setEta(double eta) {m_omega = eta;}
  /// Use the protected field 2theta as delta (H. You conventions).
  double getDelta() const {return m_2theta;}
  /// Use the protected field 2theta as delta (H. You conventions).
  void   setDelta(double delta) {m_2theta = delta;}

  /// Set mu lower bound.
  static void setMuInf(double m)
  {m_muInf = m;}
  /// Set mu upper bound.
  static void setMuSup(double m)
  {m_muSup = m;}
  /// Set nu lower bound.
  static void setNuInf(double n)
  {m_nuInf = n;}
  /// Set nu upper bound.
  static void setNuSup(double n)
  {m_nuSup = n;}
  /// Use the protected field omega as eta.
  static void setEtaInf(double e)
  {m_omegaInf = e;}
  /// Use the protected field omega as eta.
  static void setEtaSup(double e)
  {m_omegaSup = e;}
  /// Use the protected field 2theta as delta.
  static void setDeltaInf(double t)
  {m_2thetaInf = t;}
  /// Use the protected field 2theta as delta.
  static void setDeltaSup(double t)
  {m_2thetaSup = t;}

  /// \brief Print the angle values in radians.
  virtual void printOnScreen();
  /// \brief Print the angle values in degrees.
  virtual void printDegreesOnScreen();
  /// \brief Print only static fields.
  virtual void printStaticOnScreen();

  static int test_angle6C();

private:
  /// \brief This function is private to make sure we can't use it at this level as H. You conventions are in use.
  static void setOmegaInf(double o)
  {m_omegaInf = o;}

  /// \brief This function is private to make sure we can't use it at this level as H. You conventions are in use.
  static void setOmegaSup(double o)
  {m_omegaSup = o;}

  /// \brief This function is private to make sure we can't use it at this level as H. You conventions are in use.
  double getOmega() const
  {return m_omega;}

  /// \brief This function is private to make sure we can't use it at this level as H. You conventions are in use.
  void setOmega(double omega)
  {m_omega = omega;}

};

/// A space position in a 4C Kappa diffractometer is also defined by a set of of three angles omega, kappa and phi
/// but the geometry axes are different. The fourth angle to move the detector is 2theta. Angles are in radians.
class kappa_angleConfiguration4C : public angleConfiguration
{
protected:
  /// The four angles.
  double m_omega;
  double m_kappa;
  double m_phi;
  double m_2theta;
  /// The intervals associated to the angles.
  static double m_omegaInf;
  static double m_omegaSup;
  static double m_kappaInf;
  static double m_kappaSup;
  static double m_phiInf;
  static double m_phiSup;
  static double m_2thetaInf;
  static double m_2thetaSup;

public:

  /// Empty constructor which sets everything to zero.
  kappa_angleConfiguration4C();

  /// Constructor with an already made Configuration.
  kappa_angleConfiguration4C(double,double,double,double);

  /// Constructor with an already made configuration and
  /// the angle intervals.
  kappa_angleConfiguration4C(
    double o, double c, double p, double t,
    double oi,double os,double ci,double cs,
    double pi,double ps,double ti,double ts);

  /// This redefined function builds a copy of the class.
  angleConfiguration* makeCopy() const;

  double getOmega() const {return m_omega;}
  double getKappa() const {return m_kappa;}
  double getPhi() const {return m_phi;}
  double get2Theta() const {return m_2theta;}
  void setOmega(double omega) {m_omega = omega;}
  void setKappa(double kappa) {m_kappa = kappa;}
  void setPhi(double phi) {m_phi = phi;}
  void set2Theta(double two_theta)
  {m_2theta = two_theta;}

  static void setOmegaInf(double o)
  {m_omegaInf = o;}
  static void setOmegaSup(double o)
  {m_omegaSup = o;}
  static void setKappaInf(double k)
  {m_kappaInf = k;}
  static void setKappaSup(double k)
  {m_kappaSup = k;}
  static void setPhiInf(double p)
  {m_phiInf = p;}
  static void setPhiSup(double p)
  {m_phiSup = p;}
  static void set2ThetaInf(double t)
  {m_2thetaInf = t;}
  static void set2ThetaSup(double t)
  {m_2thetaSup = t;}

  void printOnScreen();
  /// Print only static fields.
  void printStaticOnScreen();
};

#endif
