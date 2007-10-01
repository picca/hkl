#ifndef _PSEUDOAXEENGINELIST_H
#define _PSEUDOAXEENGINELIST_H


#include <vector>
#include "pseudoaxelist.h"
#include <ostream>
#include <istream>

namespace hkl
  {
  class PseudoAxeEngine;
}

namespace hkl
  {

  class PseudoAxeEngineList
    {
    protected:
      std::vector<hkl::PseudoAxeEngine *> _pseudoAxeEngines;

      hkl::PseudoAxeList _pseudoAxes;


    public:
      hkl::PseudoAxeList & pseudoAxes();

      void push_back(hkl::PseudoAxeEngine * pseudoAxeEngine);

      void clear();

      /**
       * \brief Are two PseudoAxeEngineList equals ?
       * \param pseudoAxeEngineList the PseudoAxeEngineList to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const PseudoAxeEngineList & pseudoAxeEngineList) const;

      /**
       * @brief print the PseudoAxeEngineList into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

    };

} // namespace hkl

inline std::ostream &
operator <<(std::ostream & flux, hkl::PseudoAxeEngineList const & pseudoAxeEngineList)
{
  return pseudoAxeEngineList.printToStream(flux);
}

#endif
