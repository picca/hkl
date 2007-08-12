#ifndef _PSEUDOAXEENGINELIST_H
#define _PSEUDOAXEENGINELIST_H


#include <vector>
#include "pseudoaxelist.h"
#include <iostream>
using namespace std;

namespace hkl { class PseudoAxeEngine; } 

namespace hkl {

class PseudoAxeEngineList {
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
    ostream & printToStream(ostream & flux) const;

    /**
     * @brief print on a stream the content of the PseudoAxeEngineList
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the PseudoAxeEngineList from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    istream & fromStream(istream & flux);

};

} // namespace hkl

inline std::ostream &
operator <<(std::ostream & flux, hkl::PseudoAxeEngineList const & pseudoAxeEngineList)
{
  return pseudoAxeEngineList.printToStream(flux);
}

#endif
