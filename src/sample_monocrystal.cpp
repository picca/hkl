
#include "sample_monocrystal.h"
#include "fitparameter.h"
#include "geometry.h"

#include "reflectionlist.h"
namespace hkl {

namespace sample {

/**
 * @brief The default constructor.
 * @param geometry the geometry use to fill reflections.
 * @param name The name of the sample.
 */

MonoCrystal::MonoCrystal(hkl::Geometry & geometry, const std::string & name) :
  Sample(geometry, name) 
{
  // Bouml preserved body begin 0002E402
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
  // Bouml preserved body end 0002E402
}

/**
 * @brief The default destructor.
 */

MonoCrystal::~MonoCrystal() 
{
  // Bouml preserved body begin 0002E482
      delete _euler_x;
      delete _euler_y;
      delete _euler_z;
      
      delete _reflections;
  // Bouml preserved body end 0002E482
}

/**
 * @brief The copy constructor.
 * @param sample The sample to copy from.
 */

MonoCrystal::MonoCrystal(const hkl::sample::MonoCrystal & source) :
  Sample(source),
  _U(source._U) 
{
  // Bouml preserved body begin 0002E502
      _euler_x = new FitParameter(*source._euler_x);
      _euler_y = new FitParameter(*source._euler_y);
      _euler_z = new FitParameter(*source._euler_z);
      _parameters.push_back(_euler_x);
      _parameters.push_back(_euler_y);
      _parameters.push_back(_euler_z);
      
      _reflections = new ReflectionList(*source._reflections);
  // Bouml preserved body end 0002E502
}

/**
 * @brief Clone the current Sample.
 * @return A pointer on the cloned sample.
 */

hkl::Sample * MonoCrystal::clone() const 
{
  // Bouml preserved body begin 0002E582
      return new MonoCrystal(*this);
  // Bouml preserved body end 0002E582
}

/**
 * @brief Get the UB matrix of the Sample.
 * @return The UB matrix.
 */

hkl::smatrix MonoCrystal::get_UB() 
{
  // Bouml preserved body begin 0002E702
      bool status;
      return _U * _lattice.get_B(status);
  // Bouml preserved body end 0002E702
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
  // Bouml preserved body begin 0002E782
      return SAMPLE_MONOCRYSTAL;
  // Bouml preserved body end 0002E782
}

/**
 * @brief Compute the orientation matrix from two non colinear reflections.
 *
 * @param index1 The index of the first reflection.
 * @param index2 The index of the second reflection.
 */

void MonoCrystal::computeU(unsigned int index1, unsigned int index2) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002E602
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
      
          Reflection const * r1 = (*_reflections)[index1];
          Reflection const * r2 = (*_reflections)[index2];
      
          if (!r1->isColinear(*r2))
            {
              bool status;
              svector h1c = _lattice.get_B(status) * r1->get_hkl();
              svector const & u1phi = r1->get_hkl_phi();
      
              svector h2c = _lattice.get_B(status) * r2->get_hkl();
              svector const & u2phi = r2->get_hkl_phi();
      
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
              reason << "reflection 1 : " << r1->get_hkl() << " and \nreflection2 : " << r2->get_hkl() <<  " are colinear.";
              HKLEXCEPTION(reason.str(), "Choose two non-colinear reflection");
            }
        }
  // Bouml preserved body end 0002E602
}

bool MonoCrystal::ready_to_fit() const 
{
  // Bouml preserved body begin 0002E902
      if ( _reflections->size_indep() < 1)
        return false;
      else
        return true;
  // Bouml preserved body end 0002E902
}

double MonoCrystal::fitness() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002EA02
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
          ostringstream reason;
          reason << "Can not compute the fitness of the Crystal \"" << get_name() << "\" with less than 1 active reflection.";
          HKLEXCEPTION(reason.str(),
                       "Please set at least 1 active reflections.");
        }
  // Bouml preserved body end 0002EA02
}

bool MonoCrystal::fitness(double & fitness) 
{
  // Bouml preserved body begin 0002EA82
      unsigned int nb_reflections = 0;
      fitness = 0.;
      svector hkl_phi_c;
      bool status;
      
      // compute UB = _U * B
      smatrix UB(_U);
      UB *= _lattice.get_B(status);
      if (!status)
        return status;
      
      vector<Reflection *>::const_iterator iter = _reflections->begin();
      vector<Reflection *>::const_iterator end = _reflections->end();
      while(iter != end)
        {
          if ((*iter)->flag())
            {
              Reflection & reflection = **iter;
              svector const & hkl_phi = reflection.get_hkl_phi();
              hkl_phi_c = reflection.get_hkl();
              hkl_phi_c *= UB;
              hkl_phi_c -= hkl_phi;
              hkl_phi_c *= hkl_phi_c;
              fitness += hkl_phi_c.sum();
              nb_reflections++;
            }
          ++iter;
        }
      fitness /= 3 * nb_reflections;
      
      return status;
  // Bouml preserved body end 0002EA82
}

/**
 * @brief Randomize the crystal
 */

void MonoCrystal::randomize() 
{
  // Bouml preserved body begin 0002EB02
      _lattice.randomize();
      _euler_x->randomize();
      _euler_y->randomize();
      _euler_z->randomize();
      _U.set(_euler_x->get_current().get_value(),
             _euler_y->get_current().get_value(),
             _euler_z->get_current().get_value());
  // Bouml preserved body end 0002EB02
}

void MonoCrystal::update() 
{
  // Bouml preserved body begin 0002EB82
      _U.set(_euler_x->get_current().get_value(),
             _euler_y->get_current().get_value(),
             _euler_z->get_current().get_value());
  // Bouml preserved body end 0002EB82
}

/**
 * \brief Are two MonoCrystal equals ?
 * \param sample the hkl::sample::MonoCrystal to compare with.
 * \return true if both are equals flase otherwise.
 */
bool MonoCrystal::operator==(const hkl::sample::MonoCrystal & sample) const 
{
  // Bouml preserved body begin 0002EC02
      return Sample::operator==(sample)
             && _U == sample._U;
  // Bouml preserved body end 0002EC02
}

/**
 * @brief print on a stream the content of the MonoCrystal
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & MonoCrystal::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 0002EC82
      Sample::toStream(flux);
      _euler_x->toStream(flux);
      _euler_y->toStream(flux);
      _euler_z->toStream(flux);
      _U.toStream(flux);
      
      return flux;
  // Bouml preserved body end 0002EC82
}

/**
 * @brief restore the content of the MonoCrystal from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & MonoCrystal::fromStream(istream & flux) 
{
  // Bouml preserved body begin 0002ED02
      Sample::fromStream(flux);
      _euler_x->fromStream(flux);
      _euler_y->fromStream(flux);
      _euler_z->fromStream(flux);
      _U.fromStream(flux);
      
      return flux;
  // Bouml preserved body end 0002ED02
}


} // namespace hkl::sample

} // namespace hkl
