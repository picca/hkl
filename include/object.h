#ifndef _OBJECT_H
#define _OBJECT_H

#include <string>
#include <iostream>

#include "HKLException.h"

/**
 * This class defines a parameters for the fit.
 */
class Object
{
public:

  Object(void);
 
  /**
   * @brief the default constructor
   */
  Object(std::string name);

  /**
   * @brief the copy contructor
   */
  Object(Object const & object);

  /**
   * @brief the default destructor
   */
  virtual ~Object(void);

  /**
   * \brief get the Name of the affinement method
   * \return a string with the name of the affinement method.
   */
  std::string const & get_name(void) const {return m_name;}
  
  /**
   * \brief set the name of the object.
   */
  void set_name(std::string const & name) {m_name = name;}

  /**
   * \brief Are two #Object equals ?
   * \param O the #Object to compare with
   */
  bool operator ==(Object const & O) const;
     
  /**
   * \brief print the #Object into a flux
   * \param flux The stream to print into.
   */
  std::ostream & printToStream(std::ostream & flux) const;
  
private:

  std::string m_name;
};

std::ostream& operator << (std::ostream& flux, Object const & object);

#endif // _OBJECT_H
