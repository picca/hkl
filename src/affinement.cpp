#include <iomanip>
#include "affinement.h"
#include "fitparameterlist.h"

namespace hkl
  {

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
  }

  Affinement::~Affinement()
  {
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
      flux << "Affinement : " << Object::get_name() << std::endl;
      flux << Object::get_description() << std::endl;
      flux << "  max iterations : " << _nb_max_iterations << std::endl;
      flux << "      iterations : " << _nb_iterations << std::endl;
      flux << "         fitness : " << _fitness << std::endl;

      return flux;
    }

  /**
   * @brief print on a stream the content of the Affinement
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & Affinement::toStream(std::ostream & flux) const
    {
      Object::toStream(flux);
      flux << " " << _nb_max_iterations
      << " " << _nb_iterations
      << std::setprecision(7)
      << " " << _fitness << std::endl;

      return flux;
    }

  /**
   * @brief restore the content of the Affinement from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & Affinement::fromStream(std::istream & flux)
  {
    Object::fromStream(flux);
    flux >> _nb_max_iterations
    >> _nb_iterations
    >> std::setprecision(7)
    >> _fitness;

    return flux;
  }


} // namespace hkl
