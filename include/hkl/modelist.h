#ifndef _MODELIST_H_
#define _MODELIST_H_

#include "mode.h"

using namespace std;

namespace hkl
{

class ModeList
{
public:
    /**
     * @brief The default destructor
     */
    virtual ~ModeList(void);

    /**
     * @brief Add a mode to the ModeList.
     * @throw HKLException if the mode is already present in the ModeList.
     */
    void add(Mode * mode) throw (HKLException);

    /**
     * @brief Erase a mode from the ModeList.
     * @param pos An iterator on the Mode to erase.
     * @throw HKLException if the iterator is not a valid iterator.
     */
    void erase(vector<Mode *>::iterator pos) throw (HKLException);

    /**
     * @brief Clear the ModeList
     *
     * remove all modes from the ModeList and release the Memory with a delete on each Mode.
     */
    void clear(void);

    /**
     * @brief Get the size of the ModeList
     * @return the number of Mode * in the ModeList.
     */
    unsigned int size(void) const;

    /**
     * @brief Get an iterator on the first element of ModeList.
     * @return The iterator.
     */
    vector<Mode *>::iterator begin(void);

    /**
     * @brief Get an iterator on the end of ModeList.
     * @return The iterator.
     */
    vector<Mode *>::iterator end(void);

    /**
     * @return the Mode * named
     * @param name The name of the Mode we are looking for in the ModeList.
     * @return The mode.
     * @throw HKLException if the Mode is not present n the list.
     */
    Mode * operator[](MyString const & name) throw (HKLException);

    /**
     * @brief Are two ModeList equals ?
     * @param modeList the ModeList to compare with.
     * @return True if both are equals, false otherwise.
     */
    bool operator==(ModeList const & modeList) const;

    /**
     * @brief print the ModeList into a flux
     * @param flux The stream to print into.
     * @return The modified stream.
     */
    ostream & printToStream(ostream & flux) const;

    /**
     * @brief Save the ModeList into a stream.
     * @param flux the stream to save the ModeList into.
     * @return The stream with the ModeList.
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief Restore an ModeList from a stream.
     * @param flux The stream containing the ModeList.
     * @return The modified stream.
     */
    istream & fromStream(istream & flux);

private:
    vector<Mode *> _modes; //!< The vector containing the Mode.
};

} // namespace hkl

static ostream &
operator <<(ostream & flux, hkl::ModeList const & modeList)
{
    return modeList.printToStream(flux);
}

#endif // _MODELIST_H_
