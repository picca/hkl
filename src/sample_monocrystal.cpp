#include "sample_monocrystal.h"
#include "fitparameter.h"
#include "geometry.h"
#include "reflectionlist.h"

extern struct hkl_smatrix hkl_smatrix_I;

namespace hkl
  {

  namespace sample
    {

    /**
     * @brief The default constructor.
     * @param geometry the geometry use to fill reflections.
     * @param name The name of the sample.
     */

    MonoCrystal::MonoCrystal(hkl::Geometry & geometry, const std::string & name) :
        Sample(geometry, name)
    {
      // add the U parameters
      _U = hkl_smatrix_I;

      _euler_x = new FitParameter("euler_x", "The X composant of the orientation matrix.",
                                  0., 0., M_PI, true, HKL_EPSILON);
      _euler_y = new FitParameter("euler_y", "The Y composant of the orientation matrix.",
                                  0, 0, M_PI, true, HKL_EPSILON);
      _euler_z = new FitParameter("euler_z", "The Z composant of the orientation matrix.",
                                  0, 0, M_PI, true, HKL_EPSILON);
      _parameters.push_back(_euler_x);
      _parameters.push_back(_euler_y);
      _parameters.push_back(_euler_z);


      // create the reflectionList
      _reflections = new ReflectionList(_geometry, REFLECTION_MONOCRYSTAL);
    }

    /**
     * @brief The default destructor.
     */

    MonoCrystal::~MonoCrystal()
    {
      delete _euler_x;
      delete _euler_y;
      delete _euler_z;

      delete _reflections;
    }

    /**
     * @brief The copy constructor.
     * @param sample The sample to copy from.
     */

    MonoCrystal::MonoCrystal(const hkl::sample::MonoCrystal & source) :
        Sample(source),
        _U(source._U)
    {
      _euler_x = new FitParameter(*source._euler_x);
      _euler_y = new FitParameter(*source._euler_y);
      _euler_z = new FitParameter(*source._euler_z);
      _parameters.push_back(_euler_x);
      _parameters.push_back(_euler_y);
      _parameters.push_back(_euler_z);

      _reflections = new ReflectionList(*source._reflections);
    }

    /**
     * @brief Clone the current Sample.
     * @return A pointer on the cloned sample.
     */

    hkl::Sample * MonoCrystal::clone() const
      {
        return new MonoCrystal(*this);
      }

    /**
     * @brief Get the UB matrix of the Sample.
     * @return The UB matrix.
     */

    void MonoCrystal::get_UB(hkl_smatrix * UB)
    {
      bool status;
      hkl_smatrix const * B = _lattice.get_B(status);

      *UB = _U;
      ::hkl_smatrix_times_smatrix(UB, B);
    }

    /**
     * @brief Get the type of the Sample.
     *
     * @return The Sample type.
     *
     * this method is use during the toStream and fromStream process.
     */

    hkl::SampleType MonoCrystal::get_type()
    {
      return SAMPLE_MONOCRYSTAL;
    }

    /**
     * @brief Compute the orientation matrix from two non colinear reflections.
     *
     * @param index1 The index of the first reflection.
     * @param index2 The index of the second reflection.
     */

    void MonoCrystal::computeU(unsigned int index1, unsigned int index2) throw(hkl::HKLException)
    {
      unsigned int nb_reflections = _reflections->size();
      unsigned int max = index1 > index2 ? index1 : index2;
      if (max >= nb_reflections)
        {
          std::ostringstream reason;
          if (nb_reflections)
            reason << "Cannot find the reflection indexed " << max << ", maximum index is : " << nb_reflections - 1;
          else
            reason << "Cannot find a reflection in an empty ReflectionList";
          HKLEXCEPTION(reason.str(), "Please set 2 correct index.");
        }
      else
        {

          Reflection const * r1 = (*_reflections)[index1];
          Reflection const * r2 = (*_reflections)[index2];

          if (!r1->isColinear(*r2))
            {
              /*
              bool status;
              hkl_svector h1c = _lattice.get_B(status) * r1->get_hkl();
              hkl_svector const * u1phi = r1->get_hkl_phi();

              hkl_svector h2c = _lattice.get_B(status) * r2->get_hkl();
              hkl_svector const * u2phi = r2->get_hkl_phi();

              // Compute matrix Tc from h1c and h2c.
              hkl_smatrix Tc = h1c.axisSystem(h2c).transpose();

              // Compute Tphi.
              hkl_smatrix Tphi = u1phi.axisSystem(u2phi);

              // Compute U from equation (27).
              _U = Tphi;
              _U *= Tc;
              */
              hkl_svector h1c;
              hkl_svector h2c;
              hkl_smatrix const * B;
              bool status;

              // compute h1c in the phi coordinates using B and r1 hkl coordinates.
              h1c = *(r1->get_hkl());
              B = _lattice.get_B(status);
              hkl_smatrix_times_svector(B, &h1c);

              // compute h2c in the phi coordinates using B and r2 hkl coordinates.
              h2c = *(r2->get_hkl());
              B = _lattice.get_B(status);
              hkl_smatrix_times_svector(B, &h2c);

              // Compute matrix Tc from h1c and h2c.
              hkl_smatrix Tc;
              hkl_smatrix_from_two_svector(&Tc, &h1c, &h2c);
              hkl_smatrix_transpose(&Tc);

              //now use only the Axis values to compute the same hphi vectors.
              hkl_svector const * u1phi = r1->get_hkl_phi();
              hkl_svector const * u2phi = r2->get_hkl_phi();

              hkl_smatrix Tphi;
              hkl_smatrix_from_two_svector(&Tphi, u1phi, u2phi);

              // compute U
              _U = Tphi;
              hkl_smatrix_times_smatrix(&_U, &Tc);
            }
          else
            {
              std::ostringstream reason;
              reason << "reflection 1 : " << r1->get_hkl() << " and \nreflection2 : " << r2->get_hkl() <<  " are colinear.";
              HKLEXCEPTION(reason.str(), "Choose two non-colinear reflection");
            }
        }
    }

    bool MonoCrystal::ready_to_fit() const
      {
        if ( _reflections->size_indep() < 1)
          return false;
        else
          return true;
      }

    double MonoCrystal::fitness() throw(hkl::HKLException)
    {
      if (ready_to_fit())
        {
          double f;
          bool ok = fitness(f);
          if (ok)
            return f;
          else
            HKLEXCEPTION("Cannot compute the fitness of this crystal", "check the lattice parameters");
        }
      else
        {
          std::ostringstream reason;
          reason << "Can not compute the fitness of the Crystal \"" << get_name() << "\" with less than 1 active reflection.";
          HKLEXCEPTION(reason.str(),
                       "Please set at least 1 active reflections.");
        }
    }

    bool MonoCrystal::fitness(double & fitness)
    {
      unsigned int nb_reflections = 0;
      fitness = 0.;
      hkl_svector hkl_phi_c;
      hkl_smatrix UB;
      hkl_smatrix const * B;
      bool status;

      // compute UB = _U * B
      B = _lattice.get_B(status);
      if (!status)
        return status;

      UB = _U;
      hkl_smatrix_times_smatrix(&UB, B);

      std::vector<Reflection *>::const_iterator iter = _reflections->begin();
      std::vector<Reflection *>::const_iterator end = _reflections->end();
      while (iter != end)
        {
          if ((*iter)->flag())
            {
              /*
              Reflection & reflection = **iter;
              svector const & hkl_phi = reflection.get_hkl_phi();
              hkl_phi_c = reflection.get_hkl();
              hkl_phi_c *= UB;
              hkl_phi_c -= hkl_phi;
              hkl_phi_c *= hkl_phi_c;
              fitness += hkl_phi_c.sum();
              nb_reflections++;
              */
              hkl::Reflection & reflection = **iter;
              hkl_svector const * hkl_phi = reflection.get_hkl_phi();
              hkl_phi_c = *(reflection.get_hkl());
              hkl_smatrix_times_svector(&UB, &hkl_phi_c);
              hkl_svector_minus_svector(&hkl_phi_c, hkl_phi);
              hkl_svector_times_svector(&hkl_phi_c, &hkl_phi_c);
              fitness += hkl_phi_c.data[0] + hkl_phi_c.data[1] + hkl_phi_c.data[2];
              nb_reflections++;
            }
          ++iter;
        }
      fitness /= 3 * nb_reflections;

      return status;
    }

    /**
     * @brief Randomize the crystal
     */

    void MonoCrystal::randomize()
    {
      _lattice.randomize();
      _euler_x->randomize();
      _euler_y->randomize();
      _euler_z->randomize();

      double const & euler_x = _euler_x->get_current().get_value();
      double const & euler_y = _euler_y->get_current().get_value();
      double const & euler_z = _euler_z->get_current().get_value();

      hkl_smatrix_from_euler(&_U, euler_x, euler_y, euler_z);
    }

    void MonoCrystal::update()
    {
      double const & euler_x = _euler_x->get_current().get_value();
      double const & euler_y = _euler_y->get_current().get_value();
      double const & euler_z = _euler_z->get_current().get_value();

      hkl_smatrix_from_euler(&_U, euler_x, euler_y, euler_z);
    }

    /**
     * \brief Are two MonoCrystal equals ?
     * \param sample the hkl::sample::MonoCrystal to compare with.
     * \return true if both are equals flase otherwise.
     */
    bool MonoCrystal::operator==(const hkl::sample::MonoCrystal & sample) const
      {
        return Sample::operator==(sample)
               && ::hkl_smatrix_cmp(&_U, &(sample._U));
      }

    /**
     * @brief print on a stream the content of the MonoCrystal
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    std::ostream & MonoCrystal::toStream(std::ostream & flux) const
      {
        Sample::toStream(flux);
        _euler_x->toStream(flux);
        _euler_y->toStream(flux);
        _euler_z->toStream(flux);

        return flux;
      }

    /**
     * @brief restore the content of the MonoCrystal from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    std::istream & MonoCrystal::fromStream(std::istream & flux)
    {
      Sample::fromStream(flux);
      _euler_x->fromStream(flux);
      _euler_y->fromStream(flux);
      _euler_z->fromStream(flux);

      return flux;
    }


  } // namespace hkl::sample

} // namespace hkl
