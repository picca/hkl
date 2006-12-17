#ifndef _PARAMETERLIST_H_
#define _PARAMETERLIST_H_

#include "parameter.h"

using namespace std;

namespace hkl
{

/**
 * @brief A class design to describe a ParameterList for the simplex methode
 */

class ParameterList
{

public:

    /**
     * @brief The default destructor
     */
    virtual ~ParameterList(void);

    /**
     * @brief Are two ParameterList equals ?
     * @param parameterList the ParameterList to compare with.
     * @return True if both are equals, false otherwise.
     */
    bool operator==(ParameterList const & parameterList) const;

    /**
     * @brief print the ParameterList into a flux
     * @param flux The stream to print into.
     * @return The modified stream.
     */
    ostream & printToStream(ostream & flux) const;

    /**
     * @brief Save the ParameterList into a stream.
     * @param flux the stream to save the ParameterList into.
     * @return The stream with the ParameterList.
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief Restore an ParameterList from a stream.
     * @param flux The stream containing the ParameterList.
     * @return The modified stream.
     */
    istream & fromStream(istream & flux);

    /**
     * @brief Add a Parameter to the ParameterList.
     * @param parameter The parameter to add.
     * @throw HKLException if the parameter is already present in the ParameterList.
     */
    bool add(Parameter * parameter);

    /**
     * @brief Get the size of the ParameterList
     * @return the number of Parameter * in the ParameterList.
     */
    unsigned int size(void) const;

    /**
     * @return the Parameter * named
     * @param name The name of the Parameter we are looking for in the ParameterList.
     * @return The mode.
     * @throw HKLException if the Parameter is not present n the list.
     */
    Parameter * & operator[](MyString const & name) throw (HKLException);

    /**
     * @brief Get an iterator on the first element of ParameterList.
     * @return The iterator.
     */
    vector<Parameter *>::iterator begin(void)
    {
        return _parameters.begin();
    }

    /**
     * @brief Get an iterator on the end of ParameterList.
     * @return The iterator.
     */
    vector<Parameter *>::iterator end(void)
    {
        return _parameters.end();
    }

    /**
     * @brief Get a const_iterator on the first element of ParameterList.
     * @return The const_iterator.
     */
    vector<Parameter *>::const_iterator begin(void) const
    {
        return _parameters.begin();
    }

    /**
     * @brief Get a const_iterator on the end of ParameterList.
     * @return The const_iterator.
     */
    vector<Parameter *>::const_iterator end(void) const
    {
        return _parameters.end();
    }

protected:

    vector<Parameter *> _parameters; //!< the Parameter list.

};

} // namespace hkl

/*!
 * @brief Overload of the << operator for the %ParameterList class
 * @param flux
 * @param parameterList
 * @return the modified flux.
 */
inline ostream &
operator<<(ostream & flux, hkl::ParameterList const & parameterList)
{
    return parameterList.printToStream(flux);
}

#endif // _PARAMETERLIST_H_
