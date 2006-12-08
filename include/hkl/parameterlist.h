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

      virtual ~ParameterList(void);

      bool operator==(ParameterList const & parameterList) const;

      ostream & printToStream(ostream & flux) const;

      ostream & toStream(ostream & flux) const;

      istream & fromStream(istream & flux);

      bool add(Parameter * parameter);

      unsigned int size(void) const;

      Parameter * & operator[](MyString const & name) throw (HKLException);

      vector<Parameter *>::iterator begin(void)
      {
        return _parameters.begin();
      }

      vector<Parameter *>::iterator end(void)
      {
        return _parameters.end();
      }

      vector<Parameter *>::const_iterator begin(void) const
        {
          return _parameters.begin();
        }

      vector<Parameter *>::const_iterator end(void) const
        {
          return _parameters.end();
        }

    protected:

      vector<Parameter *> _parameters;

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
