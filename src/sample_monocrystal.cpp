#include "sample_monocrystal.h"
#include "reflection_monocrystal.h"

using namespace std;

namespace hkl {
    namespace sample {

        MonoCrystal::MonoCrystal(Geometry & geometry, MyString const & name) :
          Sample(geometry, name)
        {
          // add the U parameters
          _U.set(0, 0, 0);
          _euler_x = new FitParameter("euler_x", "The X composant of the orientation matrix.",
                                      0., 0., constant::math::pi, true, constant::math::epsilon);
          _euler_y = new FitParameter("euler_y", "The Y composant of the orientation matrix.",
                                      0, 0, constant::math::pi, true, constant::math::epsilon);
          _euler_z = new FitParameter("euler_z", "The Z composant of the orientation matrix.",
                                      0, 0, constant::math::pi, true, constant::math::epsilon);
          _parameters.push_back(_euler_x);
          _parameters.push_back(_euler_y);
          _parameters.push_back(_euler_z);


          // create the reflectionList
          _reflections = new ReflectionList(_geometry, REFLECTION_MONOCRYSTAL);

        }

        MonoCrystal::MonoCrystal(MonoCrystal const & sample) :
          Sample(sample),
          _U(sample._U)
        {
          _euler_x = new FitParameter(*sample._euler_x);
          _euler_y = new FitParameter(*sample._euler_y);
          _euler_z = new FitParameter(*sample._euler_z);
          _parameters.push_back(_euler_x);
          _parameters.push_back(_euler_y);
          _parameters.push_back(_euler_z);

          _reflections = new ReflectionList(*sample._reflections);
        }

        MonoCrystal::~MonoCrystal(void)
          {
            delete _euler_x;
            delete _euler_y;
            delete _euler_z;

            delete _reflections;
          }

        Sample *
        MonoCrystal::clone(void) const
          {
            return new MonoCrystal(*this);
          }

        /**
         * @brief Compute the orientation matrix from two non colinear reflections.
         *
         * @param index1 The index of the first reflection.
         * @param index2 The index of the second reflection.
         */
        void
        MonoCrystal::computeU(unsigned int index1, unsigned int index2) throw (HKLException)
          {
            //! @todo add a test for the index.
            unsigned int nb_reflections = _reflections->size();
            unsigned int max = index1 > index2 ? index1 : index2;
            if (max >= nb_reflections)
              {
                ostringstream reason;
                if (nb_reflections)
                  reason << "Cannot find the reflection indexed " << max << ", maximum index is : " << nb_reflections - 1;
                else
                  reason << "Cannot find a reflection in an empty ReflectionList";
                HKLEXCEPTION(reason.str(), "Please set 2 correct index.");
              }
            else
              {

                Reflection & r1 = (*_reflections)[index1];
                Reflection & r2 = (*_reflections)[index2];

                if (!r1.isColinear(r2))
                  {
                    svector h1c = _lattice.get_B() * r1.get_hkl();
                    svector u1phi = r1.get_hkl_phi();

                    svector h2c = _lattice.get_B() * r2.get_hkl();
                    svector u2phi = r2.get_hkl_phi();

                    // Compute matrix Tc from h1c and h2c.
                    smatrix Tc = h1c.axisSystem(h2c).transpose();

                    // Compute Tphi.
                    smatrix Tphi = u1phi.axisSystem(u2phi);

                    // Compute U from equation (27).
                    _U = Tphi;
                    _U *= Tc;
                  }
                else
                  {
                    ostringstream reason;
                    reason << "reflection 1 : " << r1.get_hkl() << " and \nreflection2 : " << r2.get_hkl() <<  " are colinear.";
                    HKLEXCEPTION(reason.str(), "Choose two non-colinear reflection");
                  }
              }
          }

        /**
         * @brief Compute the leastSquare of the crystal.
         * @return the variance.
         */
        double
        MonoCrystal::fitness(void) throw (HKLException)
          {
            unsigned int nb_reflections = 0;
            double fitness = 0.;
            svector hkl_phi, hkl_phi_c;

            if ( _reflections->size_indep() < 1)
              {
                ostringstream reason;
                reason << "Can not compute the fitness of the Crystal \"" << get_name() << "\" with less than 1 active reflection.";
                HKLEXCEPTION(reason.str(),
                             "Please set at least 1 active reflections.");
              }
            else
              {
                //_computeB();
                //_computeU();
                vector<Reflection *>::const_iterator iter = _reflections->begin();
                vector<Reflection *>::const_iterator end = _reflections->end();
                while(iter != end)
                  {
                    if ((*iter)->flag())
                      {
                        Reflection & reflection = **iter;
                        hkl_phi = reflection.get_hkl_phi();
                        hkl_phi_c = _U * _lattice.get_B() * reflection.get_hkl();
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

        /**
         * @brief Randomize the crystal
         */
        void
        MonoCrystal::randomize(void)
          {
            _lattice.randomize();
            _euler_x->randomize();
            _euler_y->randomize();
            _euler_z->randomize();
            _U.set(_euler_x->get_current().get_value(),
                   _euler_y->get_current().get_value(),
                   _euler_z->get_current().get_value());
          }

        /**
         * @brief overload of the == operator for the cristal class
         * @param C The crystal we want to compare.
         */
        bool
        MonoCrystal::operator == (MonoCrystal const & sample) const
          {
            return Sample::operator==(sample)
            && _U == sample._U;
          }

        ostream &
        MonoCrystal::toStream(ostream & flux) const
          {
            Sample::toStream(flux);
            _euler_x->toStream(flux);
            _euler_y->toStream(flux);
            _euler_z->toStream(flux);
            _U.toStream(flux);

            return flux;
          }

        istream &
        MonoCrystal::fromStream(istream & flux)
          {
            Sample::fromStream(flux);
            _euler_x->fromStream(flux);
            _euler_y->fromStream(flux);
            _euler_z->fromStream(flux);
            _U.fromStream(flux);

            return flux;
          }

    } // namespace sample
} // namespace hkl