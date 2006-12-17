#ifndef _PSEUDOAXELIST_H_
#define _PSEUDOAXELIST_H_

#include "pseudoaxe.h"

using namespace std;

namespace hkl
{

class PseudoAxeList
{
public:
    /**
     * @brief The default destructor
     */
    ~PseudoAxeList(void);

    /**
     * @brief Add a pseudoAxe to the PseudoAxeList.
     * @throw HKLException if the pseudoAxe is already present in the PseudoAxeList.
     */
    void add(PseudoAxe * pseudoAxe) throw (HKLException);
    
    /**
     * @brief Erase a pseudoAxe from the PseudoAxeList.
     * @param pos An iterator on the PseudoAxe to erase.
     * @throw HKLException if the iterator is not a valid iterator.
     */
    void erase(vector<PseudoAxe *>::iterator pos) throw (HKLException);
    
    /**
     * @brief Clear the PseudoAxeList
     *
     * remove all pseudoAxes from the PseudoAxeList and release the Memory with a delete on each PseudoAxe.
     */
    void clear(void);
    
    /**
     * @brief Get the size of the PseudoAxeList
     * @return the number of PseudoAxe * in the PseudoAxeList.
     */
    unsigned int size(void) const;
    
    /**
     * @brief Get an iterator on the first element of PseudoAxeList.
     * @return The iterator.
     */
    vector<PseudoAxe *>::iterator begin(void);
    
    /**
     * @brief Get an iterator on the end of PseudoAxeList.
     * @return The iterator.
     */
    vector<PseudoAxe *>::iterator end(void);
    
    /**
     * @return the PseudoAxe * named
     * @param name The name of the PseudoAxe we are looking for in the PseudoAxeList.
     * @return The pseudoAxe.
     * @throw HKLException if the PseudoAxe is not present n the list.
     */
    PseudoAxe * operator[](MyString const & name) throw (HKLException);
    
    /**
     * @brief Are two PseudoAxeList equals ?
     * @param pseudoAxeList the PseudoAxeList to compare with.
     * @return True if both are equals, false otherwise.
     */
    bool operator==(PseudoAxeList const & pseudoAxeList) const;
    
    /**
     * @brief print the PseudoAxeList into a flux
     * @param flux The stream to print into.
     * @return The modified stream.
     */
    ostream & printToStream(ostream & flux) const;
    
    /**
     * @brief Save the PseudoAxeList into a stream.
     * @param flux the stream to save the PseudoAxeList into.
     * @return The stream with the PseudoAxeList.
     */
    ostream & toStream(ostream & flux) const;
    
    /**
     * @brief Restore an PseudoAxeList from a stream.
     * @param flux The stream containing the PseudoAxeList.
     * @return The modified stream.
     */
    istream & fromStream(istream & flux);

private:
    vector<PseudoAxe *> _pseudoAxes; //!< the vector containing the pseudoAxes.
};

} // namespace hkl

static ostream &
operator <<(ostream & flux, hkl::PseudoAxeList const & pseudoAxeList)
{
    return pseudoAxeList.printToStream(flux);
}

#endif // _PSEUDOAXELIST_H_
