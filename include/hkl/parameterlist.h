#ifndef _PARAMETERLIST_H
#define _PARAMETERLIST_H


#include <vector>
#include <vector>

#include <string>

#include "HKLException.h"
#include <iostream>
using namespace std;

namespace hkl { class Parameter; } 

namespace hkl {

class ParameterList {
  protected:
    std::vector<hkl::Parameter *> _parameters;


  public:
    typedef vector<Parameter *>::iterator iterator;

    typedef vector<Parameter *>::const_iterator const_iterator;

    /**
     * @brief Add a hkl::Parameter to the ParameterList.
     * @param parameter The hkl::Parameter to add.
     */
    bool add(hkl::Parameter * parameter);

    /**
     * @brief Get the size of the ParameterList.
     * @return the number of element in the ParameterList.
     */
    unsigned int size() const;

    vector<string> get_names() const;

    /**
     * @return the std::string * named
     * @param name The name of the std::string we are looking for in the ParameterList.
     * @return A std::string pointer.
     * @throw HKLException if the std::string is not present n the ParameterList.
     */
    hkl::Parameter * operator[](const std::string & name) throw(hkl::HKLException);

    /**
     * @brief Get an iterator on the first element of the ParameterList.
     * @return The iterator.
     */
    iterator begin();

    /**
     * @brief Get an iterator on the end of the ParameterList.
     * @return The iterator.
     */
    iterator end();

    /**
     * @brief Get an const_iterator on the first element of the ParameterList.
     * @return The const_iterator.
     */
    const_iterator begin() const;

    /**
     * @brief Get an const_iterator on the end of the ParameterList.
     * @return The const_iterator.
     */
    const_iterator end() const;

    /*!
     * \brief Are two ParameterList equals ?
     * \param parameterList the ParameterList to compare with.
     */
    
    bool operator==(const ParameterList & parameterList) const;

    /*!
     * \brief print the ParameterList into a flux
     * \param flux The stream to print into.
     */
    ostream & printToStream(ostream & flux) const;

    /*!
     * \brief Save the ParameterList into a stream.
     * \param flux the stream to save the ParameterList into.
     * \return The stream with the ParameterList.
     */
    ostream & toStream(ostream & flux) const;

    /*!
     * \brief Restore a ParameterList from a stream.
     * \param flux The stream containing the ParameterList to restore.
     * @todo call update_observers or not ?
     */
    istream & fromStream(istream & flux);

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

#endif
