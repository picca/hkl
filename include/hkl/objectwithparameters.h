#ifndef _OBJECT_WITH_PARAMETERS_H_
#define _OBJECT_WITH_PARAMETERS_H_

#include <vector>
#include <iostream>

#include "value.h"
#include "object.h"
#include "mystring.h"
#include "HKLException.h"

using namespace std;

namespace hkl {

  /*!
   * \brief Class used to store a object with scalars parameters.
   */
  class ObjectWithParameters : public Object
  {
    public:
  
      /*!
       * \brief the default constructor
       */
      ObjectWithParameters(void);
    
      /*!
       * \brief Copy Constructor.
       * \param objectWithParameters The ObjectWithParameters to copy.
       */
      ObjectWithParameters(ObjectWithParameters const & objectWithParameters);
  
     /*!
       * \brief Default destructor.
       */
      virtual ~ObjectWithParameters(void);
  
      /*!
       * \brief get the ValueList of the ObjectWithParameters.
       * \return The ValueList of the ObjectWithParameters.
       */
      ValueList const & get_valueList(void) const {return m_valueList;}
     
      /*!
       * \brief set the ValueList of the ObjectWithParameters.
       * \param valueList The ValueList to set.
       */
      void set_valueList(ValueList const & valueList) {m_valueList = valueList;}
  
      /*!
       * \brief Are two ObjectWithParameters equals ?
       * \param objectWithParameters the ObjectWithParameters to compare with.
       * \return True if both are equals, false otherwise.
       */
      bool operator ==(ObjectWithParameters const & objectWithParameters) const;
         
      /*!
       * \brief print the ObjectWithParameters into a flux
       * \param flux The stream to print into.
       * \return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;
    
      /*!
       * \brief get the names of all parameters in the ObjectWithParameters.
       * \return The names as a vector of MyStrings.
       */
      vector<MyString> getParametersNames(void) const;
     
      /*!
       * \brief get the value of a named parameter.
       * \param name The name of the parameter.
       * \return The value of the parameter.
       */
      double const & getParameterValue(MyString const & name) const;
  
      /*!
       * \brief set the value of a parameter.
       * \param name The name of the parameter.
       * \param value The value to set.
       */
      void setParameterValue(MyString const & name, double const & value);
     
      /*!
       * \brief add a parameter to the ObjectWithParameters
       * \param name The name of the new parameter
       */
      void addParameter(MyString const & name) throw (HKLException);

      /*!
       * \brief Save the ObjectWithParameters into a stream.
       * \param flux the stream to save the ObjectWithParameters into.
       * \return The stream with the ObjectWithParameters.
       */
      ostream & toStream(ostream & flux) const;
    
      /*!
       * \brief Restore an ObjectWithParameters from a stream.
       * \param flux The stream containing the ObjectWithParameters.
       * \return The modified stream.
       */
      istream & fromStream(istream & flux);
      
    private:
  
      ValueList m_valueList; //!< values store in the object.
  };

} // namespace hkl

ostream & operator << (ostream & flux, hkl::ObjectWithParameters const & objectWithParameters);

#endif // _OBJECT_WITH_PARAMETERS_H_
