// Store the current angle configuration according
// to the type of diffractometer.

#ifndef ANGLECONFIG
#define ANGLECONFIG


// The base class to represent the different 
// kinds of cinfigurations whatever the 
// diffractometer type is.
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

// The 4C eulerian diffractometer is defined by a
// set of 3 angles omega, chi and phi to move the 
// cristal and also a fourth angle to move the
// detector 2theta. Angles are in radians.
class eulerian_angleConfiguration4C : public angleConfiguration
{
protected:
  // The four angles.
  double m_omega;
  double m_chi;
  double m_phi;
  double m_2theta;
  // The intervals associated to the angles.
  static double m_omegaInf;
  static double m_omegaSup;
  static double m_chiInf;
  static double m_chiSup;
  static double m_phiInf;
  static double m_phiSup;
  static double m_2thetaInf;
  static double m_2thetaSup;

public:

  // Empty constructor which sets everything to zero.
  eulerian_angleConfiguration4C();

  // Constructor with an already made configuration.
  eulerian_angleConfiguration4C(double,double,double,double);

  // Constructor with an already made Configuration and
  // the angle intervals.
  eulerian_angleConfiguration4C(
    double o,double c ,double p ,double t,
    double oi,double os,double ci,double cs,
    double pi,double ps,double ti,double ts);

  // This redefined function builds a copy of the class.
  angleConfiguration* makeCopy() const;

  double getOmega() const {return m_omega;}
  double getChi() const {return m_chi;}
  double getPhi() const {return m_phi;}
  double get2Theta() const {return m_2theta;}
  void setOmega(double omega) {m_omega = omega;}
  void setChi(double chi) {m_chi = chi;}
  void setPhi(double phi) {m_phi = phi;}
  void set2Theta(double two_theta)
  {m_2theta = two_theta;}

  static void setOmegaInf(double o)
  {m_omegaInf = o;}
  static void setOmegaSup(double o)
  {m_omegaSup = o;}
  static void setChiInf(double c)
  {m_chiInf = c;}
  static void setChiSup(double c)
  {m_chiSup = c;}
  static void setPhiInf(double p)
  {m_phiInf = p;}
  static void setPhiSup(double p)
  {m_phiSup = p;}
  static void set2ThetaInf(double t)
  {m_2thetaInf = t;}
  static void set2ThetaSup(double t)
  {m_2thetaSup = t;}

  void printOnScreen();
  void printDegreesOnScreen();
  // Print only static fields.
  void printStaticOnScreen();
};

// A space position in a 4C Kappa diffractometer is
// also defined by a set of of three angles omega,
// kappa and phi but the geometry axes are different.
// The fourth angle to move the detector is 2theta.
// Angles are in radians.
class kappa_angleConfiguration4C : public angleConfiguration
{
protected:
  // The four angles.
  double m_omega;
  double m_kappa;
  double m_phi;
  double m_2theta;
  // The intervals associated to the angles.
  static double m_omegaInf;
  static double m_omegaSup;
  static double m_kappaInf;
  static double m_kappaSup;
  static double m_phiInf;
  static double m_phiSup;
  static double m_2thetaInf;
  static double m_2thetaSup;

public:

  // Empty constructor which sets everything to zero.
  kappa_angleConfiguration4C();

  // Constructor with an already made Configuration.
  kappa_angleConfiguration4C(double,double,double,double);

  // Constructor with an already made configuration and
  // the angle intervals.
  kappa_angleConfiguration4C(
    double o, double c, double p, double t,
    double oi,double os,double ci,double cs,
    double pi,double ps,double ti,double ts);

  // This redefined function builds a copy of the class.
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
  // Print only static fields.
  void printStaticOnScreen();
};

#endif
