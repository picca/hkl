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

} // namespace hkl
