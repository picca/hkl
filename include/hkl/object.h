#ifndef _OBJECT_H
#define _OBJECT_H

#include <iostream>

#include "mystring.h"
#include "HKLException.h"

using namespace std;

namespace hkl {

  /*!
   * \brief This class defines The base of most hkl classes.
   * it provide a name and a description for the Object.
   */
  class Object
  {
  public:

    /*!
     * \brief the default constructor
     */
    Object(void);
  
    /*!
     * \brief Another constructor
     * \param name The name of the Object
     * \throw HKLException if the name is empty.
     */
    Object(MyString const & name) throw (HKLException);
      
    /*!
     * \brief Another constructor
     * \param name The name of the Object
     * \param description The description of the Object
     * \throw HKLException if the name or the description is empty.
     */
    Object(MyString const & name, MyString const & description) throw (HKLException);

    /*!
     * \brief Copy contructor.
     * \param object The Object to copy from.
     */
    Object(Object const & object);

    /*!
     * \brief Default destructor
     */
    virtual ~Object(void);

    /*!
     * \brief Get the Name of the Object.
     * \return a MyString with the name of the Object.
     */
    MyString const & get_name(void) const {return m_name;}
   
    /*!
     * \brief Get the description of the Object.
     * \return a MyString with the description of the Object.
     */
    MyString const & get_description(void) const {return m_description;}
    
    /*!
     * \brief set the name of the Object.
     * \param name The name of the Object to set.
     * \throw HKLException if the name is empty.
     */
    void set_name(MyString const & name) throw (HKLException);

    /*!
     * \brief set the description of the Object.
     * \param description The description of the Object to set.
     * \throw HKLException if the descriotion is empty.
     */
    void set_description(MyString const & description) throw (HKLException);

    /*!
     * \brief Are two Object equals ?
     * \param object the Object to compare with.
     * \return true if both are equals flase otherwise.
     */
    bool operator ==(Object const & object) const;
       
    /*!
     * \brief print the Object into a flux
     * \param flux The stream to print into.
     * \return The modified flux.
     */
    ostream & printToStream(ostream & flux) const;
     
    /*!
     * \brief Save the Object into a stream.
     * \param flux the stream to save the Object into.
     * \return The modified stream.
     */
    ostream & toStream(ostream & flux) const throw (HKLException);
  
    /*!
     * \brief Restore a Object from a stream.
     * \param flux The stream containing the Object.
     * \return The modified flux.
     */
    istream & fromStream(istream & flux);
    
  private:

    MyString m_name; //!< Name of the object.
    MyString m_description; //!< Description of the object.
  };

} // namespace hkl

ostream& operator << (ostream& flux, hkl::Object const & object);

#endif // _OBJECT_H
