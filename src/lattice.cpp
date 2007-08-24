
#include "lattice.h"

namespace hkl {

/**
 * @brief The default constructor.
 */
Lattice::Lattice() 
{
  // Bouml preserved body begin 00028082
      _a = new FitParameter("a", "The a parameter of the crystal",
                            0., 1.54, 1000,
                            true, constant::math::epsilon);
      _b = new FitParameter("b", "The b parameter of the crystal",
                            0., 1.54, 1000,
                            true, constant::math::epsilon);
      _c = new FitParameter("c", "The c parameter of the crystal",
                            0., 1.54, 1000,
                            true, constant::math::epsilon);
      _alpha = new FitParameter("alpha", "The alpha parameter of the crystal",
                                0. * constant::math::degToRad, 90. * constant::math::degToRad, 180. * constant::math::degToRad,
                                true, constant::math::epsilon);
      _beta = new FitParameter("beta", "The beta parameter of the crystal",
                               0. * constant::math::degToRad, 90. * constant::math::degToRad, 180. * constant::math::degToRad,
                               true, constant::math::epsilon);
      _gamma = new FitParameter("gamma", "The gamma parameter of the cell",
                                0. * constant::math::degToRad, 90. * constant::math::degToRad, 180. * constant::math::degToRad,
                                true, constant::math::epsilon);
      
      // set a old values different than current values to force _B computation.
      _old_a = 0;
      _old_b = 0;
      _old_c = 0;
      _old_alpha = 0;
      _old_beta = 0;
      _old_gamma = 0;
      _computeB();
  // Bouml preserved body end 00028082
}

/**
 * @brief Another constructor.
 * @param a the a parameter of the Lattice
 * @param b the b parameter of the Lattice
 * @param c the c parameter of the Lattice
 * @param alpha the alpha parameter of the Lattice
 * @param beta the beta parameter of the Lattice
 * @param gamma the gamma parameter of the Lattice
 */
Lattice::Lattice(const hkl::Value & a, const hkl::Value & b, const hkl::Value & c, const hkl::Value & alpha, const hkl::Value & beta, const hkl::Value & gamma) 
{
  // Bouml preserved body begin 00028102
      _a = new FitParameter("a", "The a parameter of the crystal",
                            0., a, 1000,
                            true, constant::math::epsilon);
      _b = new FitParameter("b", "The b parameter of the crystal",
                            0., b, 1000,
                            true, constant::math::epsilon);
      _c = new FitParameter("c", "The c parameter of the crystal",
                            0., c, 1000,
                            true, constant::math::epsilon);
      _alpha = new FitParameter("alpha", "The alpha parameter of the crystal",
                                0. * constant::math::degToRad, alpha, 180. * constant::math::degToRad,
                                true, constant::math::epsilon);
      _beta = new FitParameter("beta", "The beta parameter of the crystal",
                               0. * constant::math::degToRad, beta, 180. * constant::math::degToRad,
                               true, constant::math::epsilon);
      _gamma = new FitParameter("gamma", "The gamma parameter of the cell",
                                0. * constant::math::degToRad, gamma, 180. * constant::math::degToRad,
                                true, constant::math::epsilon);
      
      // set a old values different than current values to force _B computation.
      _old_a = 0;
      _old_b = 0;
      _old_c = 0;
      _old_alpha = 0;
      _old_beta = 0;
      _old_gamma = 0;
      _computeB();
  // Bouml preserved body end 00028102
}

/**
 * @brief The copy constructor.
 * @param source The Lattice to copy.
 */
Lattice::Lattice(const hkl::Lattice & source) 
{
  // Bouml preserved body begin 00028282
      _a = new FitParameter(*(source._a));
      _b = new FitParameter(*(source._b));
      _c = new FitParameter(*(source._c));
      _alpha = new FitParameter(*(source._alpha));
      _beta = new FitParameter(*(source._beta));
      _gamma = new FitParameter(*(source._gamma));
      
      // update the old value to compute the B matrix
      _old_a = source._old_a;
      _old_b = source._old_b;
      _old_c = source._old_c;
      _old_alpha = source._old_alpha;
      _old_beta = source._old_beta;
      _old_gamma = source._old_gamma;
      
      _B = source._B;
  // Bouml preserved body end 00028282
}

/**
 * @brief The default destructor.
 */
Lattice::~Lattice() 
{
  // Bouml preserved body begin 00028202
      delete _a;
      delete _b;
      delete _c;
      delete _alpha;
      delete _beta;
      delete _gamma;
  // Bouml preserved body end 00028202
}

/**
 * @brief Get the a FitParameter of the Lattice.
 * @return A reference on the a FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
hkl::FitParameter & Lattice::a() 
{
  // Bouml preserved body begin 00028302
      return *_a;
  // Bouml preserved body end 00028302
}

/**
 * @brief Get the b FitParameter of the Lattice.
 * @return A reference on the b FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
hkl::FitParameter & Lattice::b() 
{
  // Bouml preserved body begin 00028382
      return *_b;
  // Bouml preserved body end 00028382
}

/**
 * @brief Get the c FitParameter of the Lattice.
 * @return A reference on the c FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
hkl::FitParameter & Lattice::c() 
{
  // Bouml preserved body begin 00028402
      return *_c;
  // Bouml preserved body end 00028402
}

/**
 * @brief Get the alpha FitParameter of the Lattice.
 * @return A reference on the alpha FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
hkl::FitParameter & Lattice::alpha() 
{
  // Bouml preserved body begin 00028482
      return *_alpha;
  // Bouml preserved body end 00028482
}

/**
 * @brief Get the beta FitParameter of the Lattice.
 * @return A reference on the beta FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
hkl::FitParameter & Lattice::beta() 
{
  // Bouml preserved body begin 00028502
      return *_beta;
  // Bouml preserved body end 00028502
}

/**
 * @brief Get the gamma FitParameter of the Lattice.
 * @return A reference on the gamma FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
hkl::FitParameter & Lattice::gamma() 
{
  // Bouml preserved body begin 00028582
      return *_gamma;
  // Bouml preserved body end 00028582
}

/**
 * @brief Get the a FitParameter of the Lattice.
 * @return A reference on the a FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
const hkl::FitParameter & Lattice::a() const 
{
  // Bouml preserved body begin 00028602
      return *_a;
  // Bouml preserved body end 00028602
}

/**
 * @brief Get the b FitParameter of the Lattice.
 * @return A reference on the b FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
const hkl::FitParameter & Lattice::b() const 
{
  // Bouml preserved body begin 00028682
      return *_b;
  // Bouml preserved body end 00028682
}

/**
 * @brief Get the c FitParameter of the Lattice.
 * @return A reference on the c FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
const hkl::FitParameter & Lattice::c() const 
{
  // Bouml preserved body begin 00028702
      return *_c;
  // Bouml preserved body end 00028702
}

/**
 * @brief Get the alpha FitParameter of the Lattice.
 * @return A reference on the alpha FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
const hkl::FitParameter & Lattice::alpha() const 
{
  // Bouml preserved body begin 00028782
      return *_alpha;
  // Bouml preserved body end 00028782
}

/**
 * @brief Get the beta FitParameter of the Lattice.
 * @return A reference on the beta FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
const hkl::FitParameter & Lattice::beta() const 
{
  // Bouml preserved body begin 00028802
      return *_beta;
  // Bouml preserved body end 00028802
}

/**
 * @brief Get the gamma FitParameter of the Lattice.
 * @return A reference on the gamma FitParameter.
 * @todo return fitparameter * instead of fitParameter &.
 */
const hkl::FitParameter & Lattice::gamma() const 
{
  // Bouml preserved body begin 00028882
      return *_gamma;
  // Bouml preserved body end 00028882
}

const hkl::smatrix & Lattice::get_B() const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00028902
      bool status = _computeB();
      if (status)
        return _B;
      else
        HKLEXCEPTION("can not compute B", "Check the lattice parameters");
  // Bouml preserved body end 00028902
}

const hkl::smatrix & Lattice::get_B(bool & status) const 
{
  // Bouml preserved body begin 00028982
      status = _computeB();
      return _B;
  // Bouml preserved body end 00028982
}

/**
 * @brief Compute the reciprocal Lattice.
 * @return The reciprocal Lattice.
 * @throw HKLException if the reciprocal Lattice can not be compute.
 * @todo See for the consign assignation.
 */
hkl::Lattice Lattice::reciprocal() const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00028A02
      double a_star, b_star, c_star;
      double alpha_star, beta_star, gamma_star;
      
      _compute_reciprocal(a_star, b_star, c_star, alpha_star, beta_star, gamma_star);
     
      hkl::Lattice lattice(a_star, b_star, c_star, alpha_star, beta_star, gamma_star);
      lattice._a->set_consign(_a->get_consign());
      lattice._b->set_consign(_b->get_consign());
      lattice._c->set_consign(_c->get_consign());
      lattice._alpha->set_consign(_alpha->get_consign());
      lattice._beta->set_consign(_beta->get_consign());
      lattice._gamma->set_consign(_gamma->get_consign());

      return lattice;
  // Bouml preserved body end 00028A02
}

/**
 * @brief Randomize the Lattice.
 */
void Lattice::randomize() 
{
  // Bouml preserved body begin 00028A82
      svector a, b, c;
      svector axe;
      
      // La valeur des angles alpha, beta et gamma ne sont pas indépendant.
      // Il faut donc gérer les différents cas.
      
      _a->randomize();
      _b->randomize();
      _c->randomize();
      unsigned int angles_to_randomize = _alpha->get_flagFit() + _beta->get_flagFit() + _gamma->get_flagFit();
      
      switch (angles_to_randomize)
        {
        case 0:
          break;
        case 1:
          if (_alpha->get_flagFit()) // alpha
            {
              a.set(1, 0, 0);
              b = a.rotatedAroundVector(axe.randomize(a), _gamma->get_current().get_value());
              c = a.rotatedAroundVector(axe.randomize(a), _beta->get_current().get_value());
              _alpha->set_current(b.angle(c));
            }
          else if (_beta->get_flagFit())
            { // beta
              a.set(1, 0, 0);
              b = a.rotatedAroundVector(axe.randomize(a), _gamma->get_current().get_value());
              c = b.rotatedAroundVector(axe.randomize(b), _alpha->get_current().get_value());
              _beta->set_current(a.angle(c));
            }
          else
            { // gamma
              a.set(1, 0, 0);
              c = a.rotatedAroundVector(axe.randomize(a), _beta->get_current().get_value());
              b = c.rotatedAroundVector(axe.randomize(c), _alpha->get_current().get_value());
              _gamma->set_current(a.angle(b));
            }
          break;
        case 2:
          if (_alpha->get_flagFit())
            {
              if (_beta->get_flagFit()) // alpha + beta
                {
                  a.set(1, 0, 0);
                  b = a.rotatedAroundVector(axe.randomize(a), _gamma->get_current().get_value());
                  c.randomize(a, b);
                  _alpha->set_current(b.angle(c));
                  _beta->set_current(a.angle(c));
                }
              else
                { // alpha + gamma
                  a.set(1, 0, 0);
                  c = a.rotatedAroundVector(axe.randomize(a), _beta->get_current().get_value());
                  b.randomize(a, c);
                  _alpha->set_current(b.angle(c));
                  _gamma->set_current(a.angle(b));
                }
            }
          else
            { // beta + gamma
              b.set(1, 0, 0);
              c = b.rotatedAroundVector(axe.randomize(b), _alpha->get_current().get_value());
              a.randomize(b, c);
              _beta->set_current(a.angle(c));
              _gamma->set_current(a.angle(b));
            }
          break;
        case 3:
          a.randomize();
          b.randomize(a);
          c.randomize(a, b);
          _alpha->set_current(b.angle(c));
          _beta->set_current(a.angle(c));
          _gamma->set_current(a.angle(b));
          break;
        }
      // no exception the lattice is always valid.
      _computeB();
  // Bouml preserved body end 00028A82
}

/**
 * \brief Are two Lattice equals ?
 * \param lattice the hkl::Lattice to compare with.
 * \return true if both are equals flase otherwise.
 */
bool Lattice::operator==(const hkl::Lattice & lattice) const 
{
  // Bouml preserved body begin 00028B02
      return *_a == *(lattice._a)
          && *_b == *(lattice._b)
          && *_c == *(lattice._c)
          && *_alpha == *(lattice._alpha)
          && *_beta == *(lattice._beta)
          && *_gamma == *(lattice._gamma);
  // Bouml preserved body end 00028B02
}

/**
 * @brief print the Lattice into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & Lattice::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00028B82
      flux << *_a << std::endl
           << *_b << std::endl
           << *_c << std::endl
           << *_alpha << std::endl
           << *_beta << std::endl
           << *_gamma << std::endl;
      
      flux << _B;
      
      return flux;
  // Bouml preserved body end 00028B82
}

/**
 * @brief print on a stream the content of the Lattice
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Lattice::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00028C02
      _a->toStream(flux);
      _b->toStream(flux);
      _c->toStream(flux);
      _alpha->toStream(flux);
      _beta->toStream(flux);
      _gamma->toStream(flux);
      
      _B.toStream(flux);
      
      return flux;
  // Bouml preserved body end 00028C02
}

/**
 * @brief restore the content of the Lattice from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Lattice::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00028C82
      _a->fromStream(flux);
      _b->fromStream(flux);
      _c->fromStream(flux);
      _alpha->fromStream(flux);
      _beta->fromStream(flux);
      _gamma->fromStream(flux);
      
      _B.fromStream(flux);
      
      _old_a = 0;
      _old_b = 0;
      _old_c = 0;
      _old_alpha = 0;
      _old_beta = 0;
      _old_gamma = 0;
      
      return flux;
  // Bouml preserved body end 00028C82
}

/**
 * @brief compute the B matrix from the fitParameters.
 * @return true if the calculus is valid.
 */
bool Lattice::_computeB() const 
{
  // Bouml preserved body begin 00028D02
      double a = _a->get_current().get_value();
      double b = _b->get_current().get_value();
      double c = _c->get_current().get_value();
      double alpha = _alpha->get_current().get_value();
      double beta = _beta->get_current().get_value();
      double gamma = _gamma->get_current().get_value();
      
      if ((a != _old_a)
          ||(b != _old_b)
          ||(c != _old_c)
          ||(alpha != _old_alpha)
          ||(beta != _old_beta)
          ||(gamma != _old_gamma))
        {
          double cos_alpha = cos(alpha);
          double cos_beta = cos(beta);
          double cos_gamma = cos(gamma);
          double D = 1 - cos_alpha*cos_alpha - cos_beta*cos_beta - cos_gamma*cos_gamma + 2*cos_alpha*cos_beta*cos_gamma;
      
          if (D > 0.)
            D = sqrt(D);
          else
            return false;
      
          double sin_alpha = sin(alpha);
          double sin_beta = sin(beta);
          double sin_gamma = sin(gamma);
      
          // optimization (18*, 3+)
          double a_star = constant::physic::tau * sin_alpha / (a * D);
      
          double b_star_sin_gamma_star = constant::physic::tau / (b * sin_alpha);
          double b_star_cos_gamma_star = b_star_sin_gamma_star / D * (cos_alpha*cos_beta - cos_gamma);
      
          double tmp = constant::physic::tau / (c * sin_alpha);
          double c_star_cos_beta_star = tmp / D * (cos_gamma*cos_alpha - cos_beta);
          double c_star_sin_beta_star_cos_alpha_star = tmp / (sin_beta * sin_gamma) * (cos_beta*cos_gamma - cos_alpha);
          // end of optimization
      
          _B.set( a_star, b_star_cos_gamma_star,                c_star_cos_beta_star,
                  0.    , b_star_sin_gamma_star, c_star_sin_beta_star_cos_alpha_star,
                  0.    ,                       0.,        constant::physic::tau / c);
      
          _old_a = a;
          _old_b = b;
          _old_c = c;
          _old_alpha = alpha;
          _old_beta = beta;
          _old_gamma = gamma;
        }
      return true;
  // Bouml preserved body end 00028D02
}

/**
 * @brief compute the reciprocal parameters of the Lattice.
 * @param[out] a_star the a_star value.
 * @param[out] b_star the b_star value.
 * @param[out] c_star the c_star value.
 * @param[out] alpha_star the alpha_star value.
 * @param[out] beta_star the beta_star value.
 * @param[out] gamma_star the gamma_star value.
 * @throw HKLException if the reciprocal calculus is not possible.
 */
void Lattice::_compute_reciprocal(double & a_star, double & b_star, double & c_star, double & alpha_star, double & beta_star, double & gamma_star) const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00028D82
      double a = _a->get_current().get_value();
      double b = _b->get_current().get_value();
      double c = _c->get_current().get_value();
      double alpha = _alpha->get_current().get_value();
      double beta = _beta->get_current().get_value();
      double gamma = _gamma->get_current().get_value();
      
      
      // D = 1 - cos(alpha)*cos(alpha) - cos(beta)*cos(beta) - cos(gamma)*cos(gamma) + 2*cos(alpha)*cos(beta)*cos(gamma);
      double cos_alpha = cos(alpha);
      double cos_beta = cos(beta);
      double cos_gamma = cos(gamma);
      double D = 1 - cos_alpha*cos_alpha - cos_beta*cos_beta - cos_gamma*cos_gamma + 2*cos_alpha*cos_beta*cos_gamma;
      
      if (D > 0.)
        D = sqrt(D);
      else
        HKLEXCEPTION("Incorrect lattice parameters", "Please check lattice parameters");
      
      //
      double sin_alpha = sin(alpha);
      double sin_beta = sin(beta);
      double sin_gamma = sin(gamma);
      
      double sin_beta_sin_gamma = sin_beta*sin_gamma;
      double sin_gamma_sin_alpha = sin_gamma*sin_alpha;
      double sin_alpha_sin_beta = sin_alpha*sin_beta;
      
      double cos_beta1 = (cos_beta*cos_gamma - cos_alpha) / sin_beta_sin_gamma;
      double cos_beta2 = (cos_gamma*cos_alpha - cos_beta) / sin_gamma_sin_alpha;
      double cos_beta3 = (cos_alpha*cos_beta - cos_gamma) / sin_alpha_sin_beta;
      double sin_beta1 = D / sin_beta_sin_gamma;
      double sin_beta2 = D / sin_gamma_sin_alpha;
      double sin_beta3 = D / sin_alpha_sin_beta;
      
      a_star = constant::physic::tau * sin_alpha / (a * D);
      b_star = constant::physic::tau * sin_beta / (b * D);
      c_star = constant::physic::tau * sin_gamma / (c * D);
      
      alpha_star = atan2(sin_beta1, cos_beta1);
      beta_star = atan2(sin_beta2, cos_beta2);
      gamma_star = atan2(sin_beta3, cos_beta3);
  // Bouml preserved body end 00028D82
}


} // namespace hkl
