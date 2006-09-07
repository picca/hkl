#include <iostream>

#include "lattice.h"

using namespace std;

namespace hkl {

    Lattice::Lattice(void)
      {
        _a = new FitParameter("a", "The a parameter of the crystal",
                              0., 0., 10.,
                              true, constant::math::epsilon);
        _b = new FitParameter("b", "The b parameter of the crystal",
                              0., 0., 10.,
                              true, constant::math::epsilon);
        _c = new FitParameter("c", "The c parameter of the crystal",
                              0., 0., 10.,
                              true, constant::math::epsilon);
        _alpha = new FitParameter("alpha", "The alpha parameter of the crystal",
                                  0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad,
                                  true, constant::math::epsilon);
        _beta = new FitParameter("beta", "The beta parameter of the crystal",
                                 0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad,
                                 true, constant::math::epsilon_1);
        _gamma = new FitParameter("gamma", "The gamma parameter of the cell",
                                  0. * constant::math::degToRad, 0. * constant::math::degToRad, 90. * constant::math::degToRad,
                                  true, constant::math::epsilon_1);

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

    smatrix const &
    Lattice::get_B(void)
      {
        _computeB();
        return _B;
      }

    Lattice const
    Lattice::reciprocal(void) const
      {
        Lattice reciprocal;

        double a = _a->get_current().get_value();
        double b = _b->get_current().get_value();
        double c = _c->get_current().get_value();
        double alpha = _alpha->get_current().get_value();
        double beta = _beta->get_current().get_value();
        double gamma = _gamma->get_current().get_value();

        double D = sqrt( 1 
                        - cos(alpha)*cos(alpha) 
                        - cos(beta)*cos(beta)
                        - cos(gamma)*cos(gamma)
                        + 2*cos(alpha)*cos(beta)*cos(gamma));

        double cos_beta1 = (cos(beta)*cos(gamma) - cos(alpha)) / (sin(beta)*sin(gamma));
        double cos_beta2 = (cos(gamma)*cos(alpha) - cos(beta)) / (sin(gamma)*sin(alpha));
        double cos_beta3 = (cos(alpha)*cos(beta) - cos(gamma)) / (sin(alpha)*sin(beta));
        double sin_beta1 = D / (sin(beta) * sin(gamma));
        double sin_beta2 = D / (sin(gamma) * sin(alpha));
        double sin_beta3 = D / (sin(alpha) * sin(beta));

        reciprocal._a->set_current(constant::physic::tau * sin(alpha) / (a * D));
        reciprocal._b->set_current(constant::physic::tau * sin(beta) / (b * D));
        reciprocal._c->set_current(constant::physic::tau * sin(gamma) / (c * D));

        reciprocal._alpha->set_current(atan2(sin_beta1, cos_beta1));
        reciprocal._beta->set_current(atan2(sin_beta2, cos_beta2));
        reciprocal._gamma->set_current(atan2(sin_beta3, cos_beta3));

        return reciprocal;
      }

    void
    Lattice::randomize(void)
      {
        svector a, b, c;
        svector axe;

        // La valeur des angles alpha, beta et gamma ne sont pas indépendant.
        // Il faut donc gérer les différents cas.

        unsigned int angles_to_fit = _alpha->get_flagFit() + _beta->get_flagFit() + _gamma->get_flagFit();

        switch (angles_to_fit)
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
        && _B == lattice._B
        && _old_a == lattice._old_a
        && _old_b == lattice._old_b
        && _old_c == lattice._old_c
        && _old_alpha == lattice._old_alpha
        && _old_beta == lattice._old_beta
        && _old_gamma == lattice._old_gamma;
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

        return flux;
      }


    void
    Lattice::_computeB(void)
      {
        static double _old_a = 0;
        static double _old_b = 0;
        static double _old_c = 0;
        static double _old_alpha = 0;
        static double _old_beta = 0;
        static double _old_gamma = 0;

        if ((_a->get_current().get_value() != _old_a)
            ||(_b->get_current().get_value() != _old_b)
            ||(_c->get_current().get_value() != _old_c)
            ||(_alpha->get_current().get_value() != _old_alpha)
            ||(_beta->get_current().get_value() != _old_beta)
            ||(_gamma->get_current().get_value() != _old_gamma))
          {

            Lattice tmp = reciprocal();

            double a_star = tmp._a->get_current().get_value();
            double b_star = tmp._b->get_current().get_value();
            double c_star = tmp._c->get_current().get_value();
            double alpha_star = tmp._alpha->get_current().get_value();
            double beta_star = tmp._beta->get_current().get_value();
            double gamma_star = tmp._gamma->get_current().get_value();

            double c = _c->get_current().get_value();

            _B.set( a_star, b_star * cos(gamma_star),                   c_star * cos(beta_star),
                    0.    , b_star * sin(gamma_star), c_star * sin(beta_star) * cos(alpha_star),
                    0.    ,                       0.,                 constant::physic::tau / c);

            _old_a = _a->get_current().get_value();
            _old_b = _b->get_current().get_value();
            _old_c = _c->get_current().get_value();
            _old_alpha = _alpha->get_current().get_value();
            _old_beta = _beta->get_current().get_value();
            _old_gamma = _gamma->get_current().get_value();
          }
      }

} // namespace hkl
