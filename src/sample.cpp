#include "sample.h"

using namespace std;

namespace hkl {

    Sample::Sample(Geometry & geometry, MyString const & name) :
      FitParameterList(),
      Object(name),
      _geometry(geometry)
    {
      _parameters.push_back(&_lattice.a());
      _parameters.push_back(&_lattice.b());
      _parameters.push_back(&_lattice.c());
      _parameters.push_back(&_lattice.alpha());
      _parameters.push_back(&_lattice.beta());
      _parameters.push_back(&_lattice.gamma());
    }

    Sample::Sample(Sample const & sample) :
      FitParameterList(sample),
      Object(sample),
      _geometry(sample._geometry),
      _lattice(sample._lattice),
      _reflections(sample._reflections)
    {}

    /**
     * @brief Return true or false if the crystal contain at least nb_reflections independant reflections.
     * @param nb_reflections the minimim number of independant reflections.
     * @return true if crystal contain at least nb_reflections independant reflections. false otherwise.
     *
     * We comptabilize Colinear reflections as one unique reflection available for computation.
     * (ex (1,0,0) == (2,0,0)).
     */
    /*
    template<class T>
    bool
    Crystal<T>::isEnoughReflections(unsigned int nb_reflections) const
      {
        unsigned int nb_usable_reflections = 0;
        typename vector<Reflection<T> >::const_iterator iter = m_reflectionList.begin();
        typename vector<Reflection<T> >::const_iterator iter2 = m_reflectionList.begin();
        typename vector<Reflection<T> >::const_iterator end = m_reflectionList.end();

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
                    if (iter2->get_flag() && !iter->isColinear(*iter2))
                        nb_usable_reflections++;
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
*/
    /**
     * @brief Compute the orientation matrix from two basic non-parallel reflections.
     *
     * Compute the orientation matrix from two basic non-parallel reflections.
     */
    /*
    void
    Crystal<T>::computeU(void) throw (HKLException)
      {
        if (!isEnoughReflections(2))
          {
            ostringstream reason;
            reason << "Can not compute the U matrix of the Crystal \"" << get_name() << "\" with less than 2 active reflections";
            HKLEXCEPTION(reason.str(),
                         "Please set at least 2 active reflections.");
          }
        else
          {
            typename vector<Reflection<T> >::iterator iter = m_reflectionList.begin();
            iter = _getNextReflectionIteratorForCalculation(iter);
            svector h1c = m_B * iter->getHKL();
            svector u1phi = iter->get_hkl_phi();
            if (u1phi == svector())
              {
                ostringstream reason;
                ostringstream description;
                reason << "Can not compute U with this " << h1c << " reflection.";
                if (iter->get_geometry().get_source().getKi() == svector())
                    description << "The Ki vector is null check the source parameters.";
                else
                    description << "You are looking to the direct beam, find a diffraction peak first ;)";
                HKLEXCEPTION(reason.str(), description.str());
              }

            ++iter;
            iter = _getNextReflectionIteratorForCalculation(iter);
            svector h2c = m_B * iter->getHKL();
            svector u2phi = iter->get_hkl_phi();
            if (u2phi == svector())
              {
                ostringstream reason;
                ostringstream description;
                reason << "Can not compute U with this " << h2c << " reflection.";
                if (iter->get_geometry().get_source().getKi() == svector())
                    description << "The Ki vector is null check the source parameters.";
                else
                    description << "You are looking to the direct beam, find a diffraction peak first ;)";
                HKLEXCEPTION(reason.str(), description.str());
              }

            // Compute matrix Tc from h1c and h2c.
            smatrix Tc = h1c.axisSystem(h2c).transpose();

            // Compute Tphi.
            smatrix Tphi = u1phi.axisSystem(u2phi);

            // Compute U from equation (27).
            m_U = Tphi;
            m_U *= Tc;
          }
      }
*/
    /**
     * @brief Compute the leastSquare of the crystal.
     * @return the variance.
     */
    /*
    template<class T>
    double
    Crystal<T>::fitness(void) throw (HKLException)
      {
        unsigned int nb_reflections = 0;
        double fitness = 0.;
        svector hkl_phi, hkl_phi_c;

        if (!isEnoughReflections(1))
          {
            ostringstream reason;
            reason << "Can not compute the fitness of the Crystal \"" << get_name() << "\" with less than 1 active reflection.";
            HKLEXCEPTION(reason.str(),
                         "Please set at least 1 active reflections.");
          }
        else
          {
            _computeB();
            _computeU();
            typename vector<Reflection<T> >::const_iterator iter = m_reflectionList.begin();
            typename vector<Reflection<T> >::const_iterator end = m_reflectionList.end();
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
*/

    /**
     * @brief Randomize the crystal
     */
    /*
    template<class T>
    void
    Crystal<T>::randomize(void)
      {
        FitParameterList::randomize();
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
        _computeU();
      }
      */

    /**
     * @brief overload of the == operator for the cristal class
     * @param C The crystal we want to compare.
     */
    bool
    Sample::operator == (Sample const & sample) const
      {
        return FitParameterList::operator==(sample)
        && Object::operator==(sample)
        && _lattice == sample._lattice
        && _reflections == sample._reflections;
      }

    /**
     * @brief Print the state of the current crystal on a ostream.
     * @param flux the ostream to write into.
     * @return the flux modified.
     */
    ostream &
    Sample::printToStream(ostream & flux) const
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
            FitParameter const & p = *_parameters[i];
            flux.precision(3);
            flux.width(9); flux << p.get_name() << "(" << p.get_flagFit() << "):";
            flux.width(9); flux << p.get_current();
            flux.width(9); flux << p.get_min();
            flux.width(9); flux << p.get_max();
            flux << endl;
          }
        for(i=3;i<6;i++)
          {
            FitParameter const & p = *_parameters[i];
            flux.precision(3);
            flux.width(9); flux << p.get_name() << "(" << p.get_flagFit() << "):";
            flux.width(9); flux << p.get_current()*constant::math::radToDeg;
            flux.width(9); flux << p.get_min()*constant::math::radToDeg;
            flux.width(9); flux << p.get_max()*constant::math::radToDeg;
            flux << endl;
          }
        flux << _reflections;
        /*
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
          */

        //Reflections
        /*
        if (_reflections->size())
          {
            flux << endl << "  Reflections:" << endl
            << "  n";
            flux.width(9); flux << "h";
            flux.width(9); flux << "k";
            flux.width(9); flux << "l";
            flux << "  ";
            vector<MyString> axesNames = (*_reflections)[0].get_geometry().getAxesNames();
            unsigned int n = axesNames.size();
            for(i=0;i<n;i++)
              {
                flux.width(9);
                flux << axesNames[i];
              }
            flux << "  ";
            flux.width(9); flux << "lambda";
            flux << endl;
            vector<Reflection *>::const_iterator iter = _reflections->begin();
            vector<Reflection *>::const_iterator end = _reflections->end();
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
            */
        return flux;
      }

    ostream &
    Sample::toStream(ostream & flux) const
      {
        Object::toStream(flux);
        FitParameterList::toStream(flux);
        _lattice.toStream(flux);
        _reflections->toStream(flux);

        return flux;
      }

    istream &
    Sample::fromStream(istream & flux)
      {
        Object::fromStream(flux);
        FitParameterList::fromStream(flux);
        _lattice.fromStream(flux);
        _reflections->fromStream(flux);

        return flux;
      }


    /*!
     * \brief The main function to compute the U matrix.
     *
     * This method compute U using the 3 eulerian angles.
     * in fact euler_x, euler_y and euler_z parameters.
     */
    /*
    template<class T>
    void
    Crystal<T>::_computeU(void)
      {
        double euler_x = (*this)["euler_x"].get_value();
        double euler_y = (*this)["euler_y"].get_value();
        double euler_z = (*this)["euler_z"].get_value();

        set_U(smatrix(euler_x, euler_y, euler_z));
      }
*/

    /**
     * @brief Return the index of the next usable reflection for calculation
     * @param from The iterator of the reflection from which the search start.
     * @return The iterator of the next reflection valid for calculus.
     */
    /*
    template<class T>
    typename vector<Reflection<T> >::iterator &
    Crystal<T>::_getNextReflectionIteratorForCalculation(typename vector<Reflection<T> >::iterator & from) throw (HKLException)
      {
        typename vector<Reflection<T> >::iterator end = m_reflectionList.end();

        while(from < end)
          {
            if (from->get_flag())
                return from;
            ++from;
          }
        HKLEXCEPTION("No more reflection.",
                     "Please add reflections.");
      }
*/
} // namespace hkl
