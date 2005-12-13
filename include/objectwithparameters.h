#ifndef _OBJECT_WITH_PARAMETERS_H_
#define _OBJECT_WITH_PARAMETERS_H_

#include <string>
#include <vector>
#include <iostream>

#include "value.h"
#include "object.h"
#include "HKLException.h"

using namespace std;

namespace hkl {

  /**
   * This class defines a parameters for the fit.
   */
  class ObjectWithParameters : public Object
  {
    public:
  
      /**
       * @brief the default constructor
       */
      ObjectWithParameters(void); //!< Default constructor.
    
      ObjectWithParameters(ObjectWithParameters const & objectWithParameters); //!< Copy constructor.
  
     /**
       * @brief the default destructor
       */
      virtual ~ObjectWithParameters(void);
  
      /**
       * \brief get the #ValueList of the #ObjectWithParameters.
       * \return The #ValueList of the #ObjectWithParameters.
       */
      ValueList const & get_valueList(void) const {return m_valueList;}
     
      /**
       * @brief set the #ValueList of the #ObjectWithParameters.
       * @param valueList The #ValueList to set.
       */
      void set_valueList(ValueList const & valueList) {m_valueList = valueList;}
  
      /**
       * \brief Are two #ObjectWithParameters equals ?
       * \param objectWithParameters the #ObjectWithParameters to compare with
       */
      bool operator ==(ObjectWithParameters const & objectWithParameters) const;
         
      /**
       * \brief print the #ObjectWithParameters into a flux
       * \param flux The stream to print into.
       */
      ostream & printToStream(ostream & flux) const;
    
      /**
       * @brief get the names of all parameters in the #ObjectWithParameters
       * @return The names as a vector of strings.
       */
      vector<string> getParametersNames(void) const;
     
      /**
       * @brief get the value of the parameter named.
       * @param name The name of the pârameter.
       * @return the value of the parameter.
       */
      double const & getParameterValue(string const & name) const;
  
      /**
       * @brief set the value of a parameter.
       * @param name The name of the parameter.
       * @param value The value to set.
       */
      void setParameterValue(string const & name, double const & value);
     
      /**
       * @brief add a parameter to the #ObjectWithParameters
       * @param name The name of the new parameter
       */
      void addParameter(string const & name) throw (HKLException);

      /**
       * \brief Save the ObjectWithParameters into a stream.
       * \param flux the stream to save the ObjectWithParameters into.
       * \return The stream with the ObjectWithParameters.
       */
      ostream & toStream(ostream & flux) const;
    
      /**
       * \brief Restore an ObjectWithParameters from a stream.
       * \param flux The stream containing the ObjectWithParameters.
       */
      istream & fromStream(istream & flux);
      
    private:
  
      ValueList m_valueList; // values store in the object.
  };

} // namespace hkl

ostream & operator << (ostream & flux, hkl::ObjectWithParameters const & objectWithParameters);

#endif // _OBJECT_WITH_PARAMETERS_H_
