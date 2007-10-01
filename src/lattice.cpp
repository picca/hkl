#include "config.h"
#include "svector.h"
#include "lattice.h"

extern struct hkl_svector hkl_svector_X;

namespace hkl
  {

  /**
   * @brief The default constructor.
   */
  Lattice::Lattice()
  {
    _a = new FitParameter("a", "The a parameter of the crystal",
                          0., 1.54, 1000,
                          true, HKL_EPSILON);
    _b = new FitParameter("b", "The b parameter of the crystal",
                          0., 1.54, 1000,
                          true, HKL_EPSILON);
    _c = new FitParameter("c", "The c parameter of the crystal",
                          0., 1.54, 1000,
                          true, HKL_EPSILON);
    _alpha = new FitParameter("alpha", "The alpha parameter of the crystal",
                              0. * HKL_DEGTORAD, 90. * HKL_DEGTORAD, 180. * HKL_DEGTORAD,
                              true, HKL_EPSILON);
    _beta = new FitParameter("beta", "The beta parameter of the crystal",
                             0. * HKL_DEGTORAD, 90. * HKL_DEGTORAD, 180. * HKL_DEGTORAD,
                             true, HKL_EPSILON);
    _gamma = new FitParameter("gamma", "The gamma parameter of the cell",
                              0. * HKL_DEGTORAD, 90. * HKL_DEGTORAD, 180. * HKL_DEGTORAD,
                              true, HKL_EPSILON);

    // set a old values different than current values to force _B computation.
    _old_a = 0;
    _old_b = 0;
    _old_c = 0;
    _old_alpha = 0;
    _old_beta = 0;
    _old_gamma = 0;
    _computeB();
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
    _a = new FitParameter("a", "The a parameter of the crystal",
                          0., a, 1000,
                          true, HKL_EPSILON);
    _b = new FitParameter("b", "The b parameter of the crystal",
                          0., b, 1000,
                          true, HKL_EPSILON);
    _c = new FitParameter("c", "The c parameter of the crystal",
                          0., c, 1000,
                          true, HKL_EPSILON);
    _alpha = new FitParameter("alpha", "The alpha parameter of the crystal",
                              0. * HKL_DEGTORAD, alpha, 180. * HKL_DEGTORAD,
                              true, HKL_EPSILON);
    _beta = new FitParameter("beta", "The beta parameter of the crystal",
                             0. * HKL_DEGTORAD, beta, 180. * HKL_DEGTORAD,
                             true, HKL_EPSILON);
    _gamma = new FitParameter("gamma", "The gamma parameter of the cell",
                              0. * HKL_DEGTORAD, gamma, 180. * HKL_DEGTORAD,
                              true, HKL_EPSILON);

    // set a old values different than current values to force _B computation.
    _old_a = 0;
    _old_b = 0;
    _old_c = 0;
    _old_alpha = 0;
    _old_beta = 0;
    _old_gamma = 0;
    _computeB();
  }

  /**
   * @brief The copy constructor.
   * @param source The Lattice to copy.
   */
  Lattice::Lattice(const hkl::Lattice & source)
  {
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
  }

  /**
   * @brief The default destructor.
   */
  Lattice::~Lattice()
  {
    delete _a;
    delete _b;
    delete _c;
    delete _alpha;
    delete _beta;
    delete _gamma;
  }

  /**
   * @brief Get the a FitParameter of the Lattice.
   * @return A reference on the a FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  hkl::FitParameter & Lattice::a()
  {
    return *_a;
  }

  /**
   * @brief Get the b FitParameter of the Lattice.
   * @return A reference on the b FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  hkl::FitParameter & Lattice::b()
  {
    return *_b;
  }

  /**
   * @brief Get the c FitParameter of the Lattice.
   * @return A reference on the c FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  hkl::FitParameter & Lattice::c()
  {
    return *_c;
  }

  /**
   * @brief Get the alpha FitParameter of the Lattice.
   * @return A reference on the alpha FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  hkl::FitParameter & Lattice::alpha()
  {
    return *_alpha;
  }

  /**
   * @brief Get the beta FitParameter of the Lattice.
   * @return A reference on the beta FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  hkl::FitParameter & Lattice::beta()
  {
    return *_beta;
  }

  /**
   * @brief Get the gamma FitParameter of the Lattice.
   * @return A reference on the gamma FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  hkl::FitParameter & Lattice::gamma()
  {
    return *_gamma;
  }

  /**
   * @brief Get the a FitParameter of the Lattice.
   * @return A reference on the a FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  const hkl::FitParameter & Lattice::a() const
    {
      return *_a;
    }

  /**
   * @brief Get the b FitParameter of the Lattice.
   * @return A reference on the b FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  const hkl::FitParameter & Lattice::b() const
    {
      return *_b;
    }

  /**
   * @brief Get the c FitParameter of the Lattice.
   * @return A reference on the c FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  const hkl::FitParameter & Lattice::c() const
    {
      return *_c;
    }

  /**
   * @brief Get the alpha FitParameter of the Lattice.
   * @return A reference on the alpha FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  const hkl::FitParameter & Lattice::alpha() const
    {
      return *_alpha;
    }

  /**
   * @brief Get the beta FitParameter of the Lattice.
   * @return A reference on the beta FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  const hkl::FitParameter & Lattice::beta() const
    {
      return *_beta;
    }

  /**
   * @brief Get the gamma FitParameter of the Lattice.
   * @return A reference on the gamma FitParameter.
   * @todo return fitparameter * instead of fitParameter &.
   */
  const hkl::FitParameter & Lattice::gamma() const
    {
      return *_gamma;
    }

  hkl_smatrix const * Lattice::get_B() const throw(hkl::HKLException)
  {
    bool status = _computeB();
    if (status)
      return &_B;
    else
      HKLEXCEPTION("can not compute B", "Check the lattice parameters");
  }

  hkl_smatrix const * Lattice::get_B(bool & status) const
    {
      status = _computeB();
      return &_B;
    }

  /**
   * @brief Compute the reciprocal Lattice.
   * @return The reciprocal Lattice.
   * @throw HKLException if the reciprocal Lattice can not be compute.
   * @todo See for the consign assignation.
   */
  hkl::Lattice Lattice::reciprocal() const throw(hkl::HKLException)
  {
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
  }

  /**
   * @brief Randomize the Lattice.
   */
  void Lattice::randomize()
  {
    static hkl_svector svector_x = {{1, 0, 0}};
    hkl_svector a, b, c;
    hkl_svector axe;

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
            a = b = c = svector_x;

            // randomize b
            ::hkl_svector_randomize_svector(&axe, &a);
            ::hkl_svector_rotated_around_vector(&b, &axe, _gamma->get_current().get_value());

            // randomize c
            ::hkl_svector_randomize_svector(&axe, &a);
            ::hkl_svector_rotated_around_vector(&c, &axe, _beta->get_current().get_value());

            //compute the alpha angle.
            _alpha->set_current(::hkl_svector_angle(&b, &c));
          }
        else if (_beta->get_flagFit())
          {
            // beta
            a = b = svector_x;

            // randomize b
            ::hkl_svector_randomize_svector(&axe, &a);
            ::hkl_svector_rotated_around_vector(&b, &axe, _gamma->get_current().get_value());

            // randomize c
            c = b;
            ::hkl_svector_randomize_svector(&axe, &b);
            ::hkl_svector_rotated_around_vector(&c, &axe, _alpha->get_current().get_value());

            //compute beta
            _beta->set_current(::hkl_svector_angle(&a, &c));
          }
        else
          {
            // gamma
            a = c = svector_x;

            // randomize c
            ::hkl_svector_randomize_svector(&axe, &a);
            ::hkl_svector_rotated_around_vector(&c, &axe, _beta->get_current().get_value());

            // randomize b
            b = c;
            ::hkl_svector_randomize_svector(&axe, &c);
            ::hkl_svector_rotated_around_vector(&b, &axe, _alpha->get_current().get_value());

            //compute beta
            _gamma->set_current(::hkl_svector_angle(&a, &b));
          }
        break;
      case 2:
        if (_alpha->get_flagFit())
          {
            if (_beta->get_flagFit()) // alpha + beta
              {
                a = b = svector_x;

                // randomize b
                ::hkl_svector_randomize_svector(&axe, &a);
                ::hkl_svector_rotated_around_vector(&b, &axe, _gamma->get_current().get_value());

                //randomize c
                ::hkl_svector_randomize_svector_svector(&c, &a, &b);

                _alpha->set_current(::hkl_svector_angle(&b, &c));
                _beta->set_current(::hkl_svector_angle(&a, &c));
              }
            else
              {
                // alpha + gamma
                a = c = svector_x;

                // randomize c
                ::hkl_svector_randomize_svector(&axe, &a);
                ::hkl_svector_rotated_around_vector(&c, &axe, _beta->get_current().get_value());

                //randomize c
                ::hkl_svector_randomize_svector_svector(&b, &a, &c);

                _alpha->set_current(::hkl_svector_angle(&b, &c));
                _gamma->set_current(::hkl_svector_angle(&a, &b));
              }
          }
        else
          {
            // beta + gamma
            b = c = svector_x;

            // randomize c
            ::hkl_svector_randomize_svector(&axe, &b);
            ::hkl_svector_rotated_around_vector(&c, &axe, _alpha->get_current().get_value());

            //randomize c
            ::hkl_svector_randomize_svector_svector(&a, &b, &c);

            _beta->set_current(::hkl_svector_angle(&a, &c));
            _gamma->set_current(::hkl_svector_angle(&a, &b));
          }
        break;
      case 3:
        ::hkl_svector_randomize(&a);
        ::hkl_svector_randomize_svector(&b, &a);
        ::hkl_svector_randomize_svector_svector(&c, &b, &a);

        _alpha->set_current(::hkl_svector_angle(&b, &c));
        _beta->set_current(::hkl_svector_angle(&a, &c));
        _gamma->set_current(::hkl_svector_angle(&a, &b));
        break;
      }
    // no exception the lattice is always valid.
    _computeB();
  }

  /**
   * \brief Are two Lattice equals ?
   * \param lattice the hkl::Lattice to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool Lattice::operator==(const hkl::Lattice & lattice) const
    {
      return *_a == *(lattice._a)
             && *_b == *(lattice._b)
             && *_c == *(lattice._c)
             && *_alpha == *(lattice._alpha)
             && *_beta == *(lattice._beta)
             && *_gamma == *(lattice._gamma);
    }

  /**
   * @brief print the Lattice into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Lattice::printToStream(std::ostream & flux) const
    {
      flux << *_a << std::endl
      << *_b << std::endl
      << *_c << std::endl
      << *_alpha << std::endl
      << *_beta << std::endl
      << *_gamma << std::endl;

      return flux;
    }

  /**
   * @brief compute the B matrix from the fitParameters.
   * @return true if the calculus is valid.
   */
  bool Lattice::_computeB() const
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
          double a_star = HKL_TAU * sin_alpha / (a * D);

          double b_star_sin_gamma_star = HKL_TAU / (b * sin_alpha);
          double b_star_cos_gamma_star = b_star_sin_gamma_star / D * (cos_alpha*cos_beta - cos_gamma);

          double tmp = HKL_TAU / (c * sin_alpha);
          double c_star_cos_beta_star = tmp / D * (cos_gamma*cos_alpha - cos_beta);
          double c_star_sin_beta_star_cos_alpha_star = tmp / (sin_beta * sin_gamma) * (cos_beta*cos_gamma - cos_alpha);
          // end of optimization

          _B.data[0][0] = a_star;
          _B.data[0][1] = b_star_cos_gamma_star;
          _B.data[0][2] = c_star_cos_beta_star;

          _B.data[1][0] = 0;
          _B.data[1][1] = b_star_sin_gamma_star;
          _B.data[1][2] = c_star_sin_beta_star_cos_alpha_star;

          _B.data[2][0] = 0;
          _B.data[2][1] = 0;
          _B.data[2][2] = HKL_TAU / c;

          _old_a = a;
          _old_b = b;
          _old_c = c;
          _old_alpha = alpha;
          _old_beta = beta;
          _old_gamma = gamma;
        }
      return true;
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

    a_star = HKL_TAU * sin_alpha / (a * D);
    b_star = HKL_TAU * sin_beta / (b * D);
    c_star = HKL_TAU * sin_gamma / (c * D);

    alpha_star = atan2(sin_beta1, cos_beta1);
    beta_star = atan2(sin_beta2, cos_beta2);
    gamma_star = atan2(sin_beta3, cos_beta3);
  }


} // namespace hkl
