#ifndef _PSEUDOAXELIST_H
#define _PSEUDOAXELIST_H


#include <vector>
#include <vector>

#include "HKLException.h"
#include <string>

#include <iostream>
using namespace std;

namespace hkl { class PseudoAxe; } 

namespace hkl {

class PseudoAxeList {
  protected:
    std::vector<hkl::PseudoAxe *> _pseudoAxes;


  public:
    typedef vector<hkl::PseudoAxe*>::iterator iterator;

    typedef vector<hkl::PseudoAxe*>::const_iterator const_iterator;

    void push_back(hkl::PseudoAxe * pseudoAxe) throw(hkl::HKLException);

    PseudoAxeList::iterator begin();

    PseudoAxeList::iterator end();

    PseudoAxeList::const_iterator begin() const;

    PseudoAxeList::const_iterator end() const;

    /**
     * @brief Get all the names of the PseudoAxes in the PseudoAxeList
     */
    vector<string> get_names() const;

    /**
     * @brief Get an element of the PseudoAxeList.
     * @param name The name of the PseudoAxe to find.
     * @return A pointer on the PseudoAxe or NULL if the pseudoAxe is not present in the PseudoAxeList
     */
    hkl::PseudoAxe * operator[](const std::string & name);

    /**
     * @brief Get the size of the PseudoAxeList.
     * @return the number of element in the PseudoAxeList.
     */
    unsigned int size() const;

    void clear();

    void set_write_from_read();

    /*!
     * \brief print the PseudoAxeList into a flux
     * \param flux The stream to print into.
     */
    ostream & printToStream(ostream & flux) const;

};

} // namespace hkl
/*!
 * @brief Overload of the << operator for the %PseudoAxeList class
 * @param flux
 * @param pseudoAxeList
 * @return the modified flux.
 */
inline ostream &
operator<<(ostream & flux, hkl::PseudoAxeList const & pseudoAxeList)
{
  return pseudoAxeList.printToStream(flux);
}
#endif
