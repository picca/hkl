
#include "affinement.h"
#include "fitparameterlist.h"

#include <iomanip>
#include "constant.h"
namespace hkl {

/**
 * @brief the default constructor protected because the class is abstrait
 * @param name The name of the Affinement.
 * @param description The description of the Affinement.
 */

Affinement::Affinement(const std::string & name, const std::string & description) :
  Object(name, description),
  _nb_max_iterations(2500),
  _nb_iterations(0),
  _fitness(0)
{
  // Bouml preserved body begin 00036002
  // Bouml preserved body end 00036002
}

Affinement::~Affinement() 
{
  // Bouml preserved body begin 00036082
  // Bouml preserved body end 00036082
}

void Affinement::set_nb_max_iterations(const unsigned int value) 
{
  _nb_max_iterations = value;
}

void Affinement::set_nb_iterations(const unsigned int value) 
{
  _nb_iterations = value;
}

void Affinement::set_fitness(const double value) 
{
  _fitness = value;
}

/**
 * @brief print the Affinement into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & Affinement::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00036482
      flux << "Affinement : " << Object::get_name() << std::endl;
      flux << Object::get_description() << std::endl;
      flux << "  max iterations : " << _nb_max_iterations << std::endl;
      flux << "      iterations : " << _nb_iterations << std::endl;
      flux << "         fitness : " << _fitness << std::endl;
      
      return flux;
  // Bouml preserved body end 00036482
}

/**
 * @brief print on a stream the content of the Affinement
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Affinement::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00036502
      Object::toStream(flux);
      flux << " " << _nb_max_iterations
           << " " << _nb_iterations
           << std::setprecision(constant::math::precision)
           << " " << _fitness << std::endl;
      
      return flux;
  // Bouml preserved body end 00036502
}

/**
 * @brief restore the content of the Affinement from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Affinement::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00036582
      Object::fromStream(flux);
      flux >> _nb_max_iterations
           >> _nb_iterations
           >> std::setprecision(constant::math::precision)
           >> _fitness;
      
      return flux;
  // Bouml preserved body end 00036582
}


} // namespace hkl
