#include <limits>
#include <iostream>

#include "lattice.h"

namespace hkl
  {

  Lattice::Lattice(void)
  {
    _a = new FitParameter("a", "The a parameter of the crystal",
                          //0., 1.54, std::numeric_limits<double>::max(),
                          0., 1.54, 1000,
                          true, constant::math::epsilon);
    _b = new FitParameter("b", "The b parameter of the crystal",
                          //0., 1.54, std::numeric_limits<double>::max(),
                          0., 1.54, 1000,
                          true, constant::math::epsilon);
    _c = new FitParameter("c", "The c parameter of the crystal",
                          //0., 1.54, std::numeric_limits<double>::max(),
                          0., 1.54, 1000,
                          true, constant::math::epsilon);
    _alpha = new FitParameter("alpha", "The alpha parameter of the crystal",
                              0. * constant::math::degToRad, 90. * constant::math::degToRad, 180. * constant::math::degToRad,
                              true, constant::math::epsilon);
    _beta = new FitParameter("beta", "The beta parameter of the crystal",
                             0. * constant::math::degToRad, 90. * constant::math::degToRad, 180. * constant::math::degToRad,
                             true, constant::math::epsilon_1);
    _gamma = new FitParameter("gamma", "The gamma parameter of the cell",
                              0. * constant::math::degToRad, 90. * constant::math::degToRad, 180. * constant::math::degToRad,
                              true, constant::math::epsilon_1);

    // set a old values different than current values to force _B computation.
    _old_a = 0;
    _old_b = 0;
    _old_c = 0;
    _old_alpha = 0;
    _old_beta = 0;
    _old_gamma = 0;
    _computeB();
  }

  Lattice::Lattice(Value const & a, Value const & b, Value const & c,
                   Value const & alpha, Value const & beta, Value const & gamma)
  {
    _a = new FitParameter("a", "The a parameter of the crystal",
                          //0., 1.54, std::numeric_limits<double>::max(),
                          0., a, 1000,
                          true, constant::math::epsilon);
    _b = new FitParameter("b", "The b parameter of the crystal",
                          //0., 1.54, std::numeric_limits<double>::max(),
                          0., b, 1000,
                          true, constant::math::epsilon);
    _c = new FitParameter("c", "The c parameter of the crystal",
                          //0., 1.54, std::numeric_limits<double>::max(),
                          0., c, 1000,
                          true, constant::math::epsilon);
    _alpha = new FitParameter("alpha", "The alpha parameter of the crystal",
                              0. * constant::math::degToRad, alpha, 180. * constant::math::degToRad,
                              true, constant::math::epsilon);
    _beta = new FitParameter("beta", "The beta parameter of the crystal",
                             0. * constant::math::degToRad, beta, 180. * constant::math::degToRad,
                             true, constant::math::epsilon_1);
    _gamma = new FitParameter("gamma", "The gamma parameter of the cell",
                              0. * constant::math::degToRad, gamma, 180. * constant::math::degToRad,
                              true, constant::math::epsilon_1);

    // set a old values different than current values to force _B computation.
    _old_a = 0;
    _old_b = 0;
    _old_c = 0;
    _old_alpha = 0;
    _old_beta = 0;
    _old_gamma = 0;
    _computeB();
  }

  Lattice::Lattice(Lattice const & lattice)
  {
    _a = new FitParameter(*(lattice._a));
    _b = new FitParameter(*(lattice._b));
    _c = new FitParameter(*(lattice._c));
    _alpha = new FitParameter(*(lattice._alpha));
    _beta = new FitParameter(*(lattice._beta));
    _gamma = new FitParameter(*(lattice._gamma));

    // update the old value to compute the B matrix
    _old_a = lattice._old_a;
    _old_b = lattice._old_b;
    _old_c = lattice._old_c;
    _old_alpha = lattice._old_alpha;
    _old_beta = lattice._old_beta;
    _old_gamma = lattice._old_gamma;

    _B = lattice._B;
  }

  Lattice::~Lattice(void)
  {
    delete _a;
    delete _b;
    delete _c;
    delete _alpha;
    delete _beta;
    delete _gamma;
  }

  smatrix &
  Lattice::get_B(bool & status)
  {
    status = _computeB();
    return _B;
  }

  Lattice const
  Lattice::reciprocal(void) const throw (HKLException)
  {
    double a_star, b_star, c_star;
    double alpha_star, beta_star, gamma_star;

    _compute_reciprocal(a_star, b_star, c_star, alpha_star, beta_star, gamma_star);

    return Lattice(a_star, b_star, c_star, alpha_star, beta_star, gamma_star);
  }

  void
  Lattice::randomize(void)
  {
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
  }

  /**
   * @brief overload of the == operator for the Lattice
   * @param lattice The Lattice we want to compare.
   */
  bool
  Lattice::operator == (Lattice const & lattice) const
    {
      return *_a == *(lattice._a)
             && *_b == *(lattice._b)
             && *_c == *(lattice._c)
             && *_alpha == *(lattice._alpha)
             && *_beta == *(lattice._beta)
             && *_gamma == *(lattice._gamma)
             && _B == lattice._B;
    }

  ostream &
  Lattice::printToStream(ostream & flux) const
    {
      _a->printToStream(flux);
      _b->printToStream(flux);
      _c->printToStream(flux);
      _alpha->printToStream(flux);
      _beta->printToStream(flux);
      _gamma->printToStream(flux);

      return flux;
    }

  ostream &
  Lattice::toStream(ostream & flux) const
    {
      _a->toStream(flux);
      _b->toStream(flux);
      _c->toStream(flux);
      _alpha->toStream(flux);
      _beta->toStream(flux);
      _gamma->toStream(flux);

      _B.toStream(flux);

      return flux;
    }

  istream &
  Lattice::fromStream(istream & flux)
  {
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
  }


  bool
  Lattice::_computeB(void)
  {
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
  }

  void
  Lattice::_compute_reciprocal(double & a_star, double & b_star, double & c_star,
                               double & alpha_star, double & beta_star, double & gamma_star) const throw (HKLException)
  {
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
  }

} // namespace hkl
