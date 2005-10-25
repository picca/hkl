#ifndef _OBJECT_H
#define _OBJECT_H

#include <string>
#include <iostream>

#include "HKLException.h"

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
     * @param name The name of the #Object
     */
    Object(std::string const & name);
      
    /**
     * @brief Another constructor
     * @param name The name of the #Object
     * @param description The description of the #Object
     */
    Object(std::string const & name, std::string const & description);

    /**
     * @brief the copy contructor
     */
    Object(Object const & object);

    /**
     * @brief the default destructor
     */
    virtual ~Object(void);

    /**
     * \brief get the Name of the #Object.
     * \return a string with the name of the #Object.
     */
    std::string const & get_name(void) const {return m_name;}
   
    /**
     * \brief get the description of the #Object.
     * \return a string with the description of the #Object.
     */
    std::string const & get_description(void) const {return m_description;}
    
    /**
     * \brief set the name of the #Object.
     */
    void set_name(std::string const & name) {m_name = name;}

    /**
     * \brief set the description of the #Object.
     */
    void set_description(std::string const & description) {m_description = description;}

    /**
     * \brief Are two #Object equals ?
     * \param O the #Object to compare with
     */
    bool operator ==(Object const & object) const;
       
    /**
     * \brief print the #Object into a flux
     * \param flux The stream to print into.
     */
    std::ostream & printToStream(std::ostream & flux) const;
    
  private:

    std::string m_name; //!< Name of the object.
    std::string m_description; //!< Description of the object.
  };

} // namespace hkl

std::ostream& operator << (std::ostream& flux, hkl::Object const & object);

#endif // _OBJECT_H
