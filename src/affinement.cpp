#include "affinement.h"
 
namespace hkl {

  Affinement::Affinement(std::string name) :
    Object(name),
    m_nb_max_iteration(10000),
    m_nb_iteration(10000),
    m_fitness(0)
  {}

  Affinement::Affinement(Affinement & affinement) :
    Object(affinement),
    m_nb_max_iteration(affinement.m_nb_max_iteration),
    m_nb_iteration(affinement.m_nb_iteration),
    m_fitness(affinement.m_fitness)
  {}

  Affinement::~Affinement()
  {}

} // namespace hkl
