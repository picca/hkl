#ifndef _OBJECT_H
#define _OBJECT_H

#include <string>
#include <iostream>

#include "HKLException.h"

using namespace std;

namespace hkl {

  /**
   * This class defines a parameters for the fit.
   */
  class Object
  {
  public:

    /**
     * @brief the default constructor
     */
    Object(void);
  
    /**
     * @brief Another constructor
     * @param name The name of the Object
     */
    Object(string const & name) throw (HKLException);
      
    /**
     * @brief Another constructor
     * @param name The name of the Object
     * @param description The description of the Object
     */
    Object(string const & name, string const & description) throw (HKLException);

    /**
     * @brief the copy contructor
     */
    Object(Object const & object);

    /**
     * @brief the default destructor
     */
    virtual ~Object(void);

    /**
     * \brief get the Name of the Object.
     * \return a string with the name of the Object.
     */
    string const & get_name(void) const {return m_name;}
   
    /**
     * \brief get the description of the Object.
     * \return a string with the description of the Object.
     */
    string const & get_description(void) const {return m_description;}
    
    /**
     * \brief set the name of the Object.
     */
    void set_name(string const & name) throw (HKLException);

    /**
     * \brief set the description of the Object.
     */
    void set_description(string const & description) throw (HKLException);

    /**
     * \brief Are two Object equals ?
     * \param O the Object to compare with
     */
    bool operator ==(Object const & object) const;
       
    /**
     * \brief print the Object into a flux
     * \param flux The stream to print into.
     */
    ostream & printToStream(ostream & flux) const;
     
    /**
     * \brief Save the Object into a stream.
     * \param flux the stream to save the Object into.
     * \return The stream with the Object.
     */
    ostream & toStream(ostream & flux) const throw (HKLException);
  
    /**
     * \brief Restore a Object from a stream.
     * \param flux The stream containing the Object.
     */
    istream & fromStream(istream & flux);
    
  private:

    string m_name; //!< Name of the object.
    string m_description; //!< Description of the object.
  };

} // namespace hkl

ostream& operator << (ostream& flux, hkl::Object const & object);

#endif // _OBJECT_H
