#include "crystal.h"

namespace hkl {

    Crystal::Crystal(void) :
      FitParameterList(),
      Object()
    {
      add(FitParameter("a", 0., 1., 10., true, constant::math::epsilon_1));
      add(FitParameter("b", 0., 1., 10., true, constant::math::epsilon_1));
      add(FitParameter("c", 0., 1., 10., true, constant::math::epsilon_1));
      add(FitParameter("alpha", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("beta", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("gamma", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 90. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("euler_x", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("euler_y", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("euler_z", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));

      m_U = smatrix(1., 0., 0.,
                    0., 1., 0.,
                    0., 0., 1.);
    }

    Crystal::Crystal(MyString const & name) :
      FitParameterList(),
      Object(name)
    {
      add(FitParameter("a", 0., 1., 10., true, constant::math::epsilon_1));
      add(FitParameter("b", 0., 1., 10., true, constant::math::epsilon_1));
      add(FitParameter("c", 0., 1., 10., true, constant::math::epsilon_1));
      add(FitParameter("alpha", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("beta", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 120. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("gamma", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 90. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("euler_x", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("euler_y", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));
      add(FitParameter("euler_z", 0. * constant::math::degToRad, 0. * constant::math::degToRad, 180. * constant::math::degToRad, true, constant::math::epsilon_1));

      m_U = smatrix(1., 0., 0.,
                    0., 1., 0.,
                    0., 0., 1.);
    }

    Crystal::Crystal(Crystal const & crystal) :
      FitParameterList(crystal),
      Object(crystal),
      m_B(crystal.m_B),
      m_U(crystal.m_U),
      m_reflectionList(crystal.m_reflectionList)
    {}

    bool
    Crystal::operator ==(Crystal const & crystal) const
      {
        return FitParameterList::operator==(crystal)
        && Object::operator==(crystal)
        && m_B == crystal.m_B
        && m_U == crystal.m_U
        && m_reflectionList == crystal.m_reflectionList;
      }

    void
    Crystal::getLattice(double * a, double * b, double * c,
                        double * alpha, double * beta, double * gamma) const
      {
        FitParameterList::const_iterator iter = begin();
        FitParameterList::const_iterator last = end();

        *a = (*this)["a"].get_value();
        *b = (*this)["b"].get_value();
        *c = (*this)["c"].get_value();
        *alpha = (*this)["alpha"].get_value();
        *beta = (*this)["beta"].get_value();
        *gamma = (*this)["gamma"].get_value();
      }

    void
    Crystal::getReciprocalLattice(double * a_star, double * b_star, double * c_star,
                                  double * alpha_star, double * beta_star, double * gamma_star) const
      {
        double a, b, c, alpha, beta, gamma;
        getLattice(&a, &b, &c, &alpha, &beta, &gamma);

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

        *a_star = constant::physic::tau * sin(alpha) / (a * D);
        *b_star = constant::physic::tau * sin(beta) / (b * D);
        *c_star = constant::physic::tau * sin(gamma) / (c * D);

        *alpha_star = atan2(sin_beta1, cos_beta1);
        *beta_star = atan2(sin_beta2, cos_beta2);
        *gamma_star = atan2(sin_beta3, cos_beta3);
      }

    void
    Crystal::setLattice(double const & a, double const & b, double const & c,
                        double const & alpha, double const & beta, double const & gamma)
      {
        (*this)["a"].set_value(a);
        (*this)["b"].set_value(b);
        (*this)["c"].set_value(c);
        (*this)["alpha"].set_value(alpha);
        (*this)["beta"].set_value(beta);
        (*this)["gamma"].set_value(gamma);
        _computeB();
      }

    unsigned int
    Crystal::addReflection(Reflection const & reflection) throw (HKLException)
      {
        // test the validity of the reflection
        if (fabs(reflection.get_geometry().get_source().get_waveLength()) < constant::math::epsilon_0)
            HKLEXCEPTION("The waveLength is equal to zero.",
                         "The source is not properly configure");

        // If the reflection already exist put the flag to false
        if (reflection.get_flag())
          {
            ReflectionList::iterator iter(m_reflectionList.begin());
            ReflectionList::iterator end(m_reflectionList.end());
            while(iter != end)
              {
                if (fabs(reflection.get_h() - iter->get_h()) < constant::math::epsilon_0
                    && fabs(reflection.get_k() - iter->get_k()) < constant::math::epsilon_0
                    && fabs(reflection.get_l() - iter->get_l()) < constant::math::epsilon_0)
                  {
                    m_reflectionList.push_back(reflection);
                    m_reflectionList.back().set_flag(false);
                    return m_reflectionList.size();
                  }
                ++iter;
              }
          } 
        m_reflectionList.push_back(reflection);
        return m_reflectionList.size();
      }

    void
    Crystal::delReflection(unsigned int const & index ) throw (HKLException)
      {
        unsigned int nb_reflection = m_reflectionList.size();

        if (index >= nb_reflection)
          {
            ostringstream reason;
            ostringstream description;

            reason << "The reflection number " << index << " is out of range";
            description << " you ask for the reflection " << index 
            << " deletion, but the cristal: " << get_name() << " containe only "
            << nb_reflection << " reflections";

            HKLEXCEPTION(reason.str(), description.str());
          }
        else
          {
            vector<Reflection>::iterator iter = m_reflectionList.begin();
            for(unsigned int i=0;i<index;i++)
                ++iter;
            m_reflectionList.erase(iter);
          }
      }

    void
    Crystal::setReflection(unsigned int const & index,
                           Reflection const & reflection) throw (HKLException)
      {
        unsigned int nb_reflection = m_reflectionList.size();

        if (index >= nb_reflection)
          {
            ostringstream reason;
            ostringstream description;

            reason << "The reflection number " << index << " is out of range";
            description << " you ask for the modification of the " << index 
            << "th reflection, but the cristal: " << get_name() << " containe only "
            << nb_reflection << " reflections";

            HKLEXCEPTION(reason.str(), description.str());
          }

        // If the reflection already exist put the flag to false      
        if (reflection.get_flag())
          {
            ReflectionList::iterator iter(m_reflectionList.begin());
            ReflectionList::iterator end(m_reflectionList.end());
            while(iter != end)
              {
                if (fabs(reflection.get_h() - iter->get_h()) < constant::math::epsilon_0
                    && fabs(reflection.get_k() - iter->get_k()) < constant::math::epsilon_0
                    && fabs(reflection.get_l() - iter->get_l()) < constant::math::epsilon_0)
                  {
                    m_reflectionList[index] = reflection;
                    m_reflectionList[index].set_flag(false);
                    return;
                  }
                ++iter;
              }
          } 
        m_reflectionList[index] = reflection;
      }

    Reflection &
    Crystal::getReflection(unsigned int const & index) throw (HKLException)
      {
        unsigned int nb_reflection = m_reflectionList.size();

        if (index >= nb_reflection)
          {
            ostringstream reason;
            ostringstream description;

            reason << "The reflection number " << index << " is out of range";
            description << " you ask for the reflection " << index 
            << " deletion, but the cristal: " << get_name() << " containe only "
            << nb_reflection << " reflections";

            HKLEXCEPTION(reason.str(), description.str());
          }
        else
            return m_reflectionList[index];
      }

    Reflection const &
    Crystal::getReflection(unsigned int const & index) const throw (HKLException)
      {
        unsigned int nb_reflection = m_reflectionList.size();

        if (index >= nb_reflection)
          {
            ostringstream reason;
            ostringstream description;

            reason << "The reflection number " << index << " is out of range";
            description << " you ask for the reflection " << index 
            << " deletion, but the cristal: " << get_name() << " containe only "
            << nb_reflection << " reflections";

            HKLEXCEPTION(reason.str(), description.str());
          }
        else
            return m_reflectionList[index];
      }

    bool
    Crystal::isEnoughReflections(unsigned int nb_reflections) const
      {
        unsigned int nb_usable_reflections = 0;
        ReflectionList::const_iterator iter = m_reflectionList.begin();
        ReflectionList::const_iterator iter2 = m_reflectionList.begin();
        ReflectionList::const_iterator end = m_reflectionList.end();

        while(iter < end && nb_usable_reflections < nb_reflections)
          {
            if (iter->get_flag())
              {
                if (nb_usable_reflections == 0)
                    nb_usable_reflections = 1;
                iter2 = iter;
                ++iter2;
                while(iter2 < end && nb_usable_reflections < nb_reflections)
                  {
                    if (iter2->get_flag())
                      {
                        if (!iter->isColinear(*iter2))
                            nb_usable_reflections++;
                      }
                    ++iter2;
                  }
              }
            ++iter;
          }
        if (nb_usable_reflections == nb_reflections)
            return true;
        else
            return false;
      }

    void
    Crystal::computeU(void) throw (HKLException)
      {
        if (!isEnoughReflections(2))
            HKLEXCEPTION("Not enought reflections (at least 2)",
                         "Please add reflections.");
        else
          {
            ReflectionList::iterator iter = m_reflectionList.begin();
            iter = _getNextReflectionIteratorForCalculation(iter);
            svector h1c = m_B * iter->getHKL();
            svector u1phi = iter->get_hkl_phi();

            ++iter;
            iter = _getNextReflectionIteratorForCalculation(iter);
            svector h2c = m_B * iter->getHKL();
            svector u2phi = iter->get_hkl_phi();

            // Compute matrix Tc from h1c and h2c.
            smatrix Tc = h1c.axisSystem(h2c).transpose();

            // Compute Tphi.
            smatrix Tphi = u1phi.axisSystem(u2phi);

            // Compute U from equation (27).
            m_U = Tphi;
            m_U *= Tc;
          }
      }

    double
    Crystal::fitness(void) throw (HKLException)
      {
        unsigned int nb_reflections = 0;
        double fitness = 0.;
        svector hkl_phi, hkl_phi_c;

        if (!isEnoughReflections(1))
            HKLEXCEPTION("Not enought reflections",
                         "Please add reflections.");
        else
          {
            _computeB();
            _computeU();
            ReflectionList::const_iterator iter = m_reflectionList.begin();
            ReflectionList::const_iterator end = m_reflectionList.end();
            while(iter != end)
              {
                if (iter->get_flag())
                  {
                    hkl_phi = iter->get_hkl_phi();
                    hkl_phi_c = m_U * m_B * iter->getHKL();
                    hkl_phi -= hkl_phi_c;
                    fitness += hkl_phi[0]*hkl_phi[0] + hkl_phi[1]*hkl_phi[1] + hkl_phi[2]*hkl_phi[2];
                    nb_reflections++;
                  }
                ++iter;
              }
            fitness /= 3*nb_reflections;

            return fitness;
          }
      }

    void
    Crystal::randomize(void)
      {
        FitParameterList::randomize();
        svector a, b, c;
        svector axe;

        // La valeur des angles alpha, beta et gamma ne sont pas indépendant.
        // Il faut donc gérer les différents cas.

        FitParameter & Alpha = (*this)["alpha"];
        FitParameter & Beta = (*this)["beta"];
        FitParameter & Gamma = (*this)["gamma"];

        unsigned int angles_to_fit = Alpha.get_flagFit() + Beta.get_flagFit() + Gamma.get_flagFit();

        switch (angles_to_fit)
          {
          case 0:
            break;
          case 1:
            if (Alpha.get_flagFit()) // alpha
              {
                a.set(1, 0, 0);
                b = a.rotatedAroundVector(axe.randomize(a), Gamma.get_value());
                c = a.rotatedAroundVector(axe.randomize(a), Beta.get_value());
                Alpha.set_value(b.angle(c));
              }
            else if (Beta.get_flagFit())
              { // beta
                a.set(1, 0, 0);
                b = a.rotatedAroundVector(axe.randomize(a), Gamma.get_value());
                c = b.rotatedAroundVector(axe.randomize(b), Alpha.get_value());
                Beta.set_value(a.angle(c));
              }
            else
              { // gamma
                a.set(1, 0, 0);
                c = a.rotatedAroundVector(axe.randomize(a), Beta.get_value());
                b = c.rotatedAroundVector(axe.randomize(c), Alpha.get_value());
                Gamma.set_value(a.angle(b));
              }
            break;
          case 2:
            if (Alpha.get_flagFit())
              {
                if (Beta.get_flagFit()) // alpha + beta
                  {
                    a.set(1, 0, 0);
                    b = a.rotatedAroundVector(axe.randomize(a), Gamma.get_value());
                    c.randomize(a, b);
                    Alpha.set_value(b.angle(c));
                    Beta.set_value(a.angle(c));
                  }
                else
                  { // alpha + gamma
                    a.set(1, 0, 0);
                    c = a.rotatedAroundVector(axe.randomize(a), Beta.get_value());
                    b.randomize(a, c);
                    Alpha.set_value(b.angle(c));
                    Gamma.set_value(a.angle(b));
                  }
              } 
            else
              { // beta + gamma
                b.set(1, 0, 0);
                c = b.rotatedAroundVector(axe.randomize(b), Alpha.get_value());
                a.randomize(b, c);
                Beta.set_value(a.angle(c));
                Gamma.set_value(a.angle(b));
              }
            break;
          case 3:
            a.randomize();
            b.randomize(a);
            c.randomize(a, b);
            Alpha.set_value(b.angle(c));
            Beta.set_value(a.angle(c));
            Gamma.set_value(a.angle(b));
            break;
          }
        _computeB();
        _computeU();
      }

    // William R. Busing and Henri A. Levy "Angle calculation 
    // for 3- and 4- Circle X-ray and Neutron Diffractometer"
    // (1967) Acta Cryst., 22, 457-464.
    // Compute the matrix B from equation (3) page 458.
    void
    Crystal::_computeB(void)
      {
        double a_star, b_star, c_star, alpha_star, beta_star, gamma_star;
        getReciprocalLattice(&a_star, &b_star, &c_star, &alpha_star, &beta_star, &gamma_star);

        double c = (*this)["c"].get_value();

        m_B.set( a_star, b_star * cos(gamma_star),                   c_star * cos(beta_star),
                 0., b_star * sin(gamma_star), c_star * sin(beta_star) * cos(alpha_star),
                 0.,                       0.,                 constant::physic::tau / c);
      }

    void
    Crystal::_computeU(void)
      {
        double euler_x = (*this)["euler_x"].get_value();
        double euler_y = (*this)["euler_y"].get_value();
        double euler_z = (*this)["euler_z"].get_value();

        set_U(smatrix(euler_x, euler_y, euler_z));
      }

    ReflectionList::iterator &
    Crystal::_getNextReflectionIteratorForCalculation(ReflectionList::iterator & from) throw (HKLException)
      {
        ReflectionList::iterator end = m_reflectionList.end();

        while( from < end)
          {
            if (from->get_flag())
                return from;
            ++from;
          }
        HKLEXCEPTION("No more reflection.",
                     "Please add reflections.");
      }

    ostream &
    Crystal::printToStream(ostream & flux) const
      { 
        unsigned int i;
        unsigned int j;

        // Parameters
        flux << "\"" << get_name() << "\"" << endl;
        flux.width(9); flux << "  Parameters:";
        flux.width(9); flux << "value";
        flux.width(9); flux << "min";
        flux.width(9); flux << "max";
        flux << endl;
        for(i=0;i<3;i++)
          {
            FitParameter const & p = (*this).at(i);
            flux.precision(3);
            flux.width(9); flux << p.get_name() << "(" << p.get_flagFit() << "):";
            flux.width(9); flux << p.get_value();
            flux.width(9); flux << p.get_min();
            flux.width(9); flux << p.get_max();
            flux << endl;
          }
        for(i=3;i<6;i++)
          {
            FitParameter const & p = (*this).at(i);
            flux.precision(3);
            flux.width(9); flux << p.get_name() << "(" << p.get_flagFit() << "):";
            flux.width(9); flux << p.get_value()*constant::math::radToDeg;
            flux.width(9); flux << p.get_min()*constant::math::radToDeg;
            flux.width(9); flux << p.get_max()*constant::math::radToDeg;
            flux << endl;
          }
        flux << endl << "  UB:" << endl;
        smatrix UB = get_U() * get_B();
        flux.precision(3);
        for(i=0;i<3;i++)
          {
            flux << "  ";
            for(j=0;j<3;j++)
              {
                flux.width(9);
                flux << UB.get(i,j);
              }
            flux << endl;
          }

        //Reflections
        if (m_reflectionList.size())
          {
            flux << endl << "  Reflections:" << endl
            << "  n";
            flux.width(9); flux << "h";
            flux.width(9); flux << "k";
            flux.width(9); flux << "l";
            flux << "  ";
            vector<MyString> axesNames = m_reflectionList[0].get_geometry().getAxesNames();
            unsigned int n = axesNames.size();
            for(i=0;i<n;i++)
              {
                flux.width(9);
                flux << axesNames[i];
              }
            flux << "  ";
            flux.width(9); flux << "lambda";
            flux << endl;
            ReflectionList::const_iterator iter = m_reflectionList.begin();
            ReflectionList::const_iterator end = m_reflectionList.end();
            n = 1;
            while(iter != end)
              {
                flux << "  " << n << *iter << endl;
                ++iter;
                ++n;
              }
          }
        else
            flux << endl << "  No reflection" << endl;
        return flux;
      }

    ostream &
    Crystal::toStream(ostream & flux) const
      {
        Object::toStream(flux);
        FitParameterList::toStream(flux);
        m_B.toStream(flux);
        m_U.toStream(flux);

        unsigned int nb_reflections = m_reflectionList.size();
        flux << " " << nb_reflections << endl;
        vector<Reflection>::const_iterator iter = m_reflectionList.begin();
        vector<Reflection>::const_iterator end = m_reflectionList.end();
        while(iter != end)
          {
            iter->toStream(flux);
            ++iter;
          }

        return flux;
      }

    istream &
    Crystal::fromStream(istream & flux)
      {
        Object::fromStream(flux);
        FitParameterList::fromStream(flux);
        m_B.fromStream(flux);
        m_U.fromStream(flux);

        unsigned int nb_reflections;
        unsigned int i;
        flux >> nb_reflections;
        for(i=0;i<nb_reflections;i++)
          {
            m_reflectionList.push_back(Reflection());
            m_reflectionList[i].fromStream(flux);
          }
        //_computeB();
        return flux;
      }

} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::Crystal const & crystal)
{ 
    return crystal.printToStream(flux);
}
