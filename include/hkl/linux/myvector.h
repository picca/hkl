#ifndef _MYVECTOR_H_
#define _MYVECTOR_H_

#include <vector>
#include <sstream>
#include <iostream>

#include "mystring.h"
#include "HKLException.h"

using namespace std;

namespace hkl {

  /*!
   * \brief Template use to store objects derived from Object.
   * \todo replace with an unordered_map.
   */
  template<class T>
  class MyVector : public vector<T>
  {
    public:
      MyVector(void);
      MyVector(MyVector const & myVector);
      virtual ~MyVector(void);
      T & operator[] (MyString const & name) throw (HKLException);
      T const & operator[] (MyString const & name) const throw (HKLException);
      bool operator== (MyVector const & myVector) const;
      bool add(T const & object) throw (HKLException);
      bool remove(MyString const & name) throw (HKLException);
      vector<MyString> getNames(void) const;
      ostream & printToStream(ostream & flux) const;
      ostream & toStream(ostream & flux) const;
      istream & fromStream(istream & flux);
  };

} // namespace hkl

/*!
 * \brief Overload of the << operator for the MyVector class
 * \param flux The flux to write into.
 * \param myVector The MyVector to stream.
 * \return The modified flux.
 */
template<class T>
ostream & operator<<(ostream & flux, hkl::MyVector<T> const & myVector)
{
  myVector.printToStream(flux);
  return flux;
} 

namespace hkl {

  /*!
   * \brief Default constructor
   */
  template<class T>
  MyVector<T>::MyVector(void)
  {}

  /*!
   * \brief Copy constructor
   * \param myVector the MyVector to copy from.
   */
  template<class T>
  MyVector<T>::MyVector(MyVector const & myVector) 
    : vector<T>(myVector)
  {}

  /*!
   * \brief Default destructor
   */
  template<class T>
  MyVector<T>::~MyVector(void)
  {}

  /*!
   * \brief Return a reference on the Object named name.
   * \param name Name of the element to find in the MyVector.
   * \throw HKLException The Object is not store in the MyVector.
   * \return The reference of the Object.
   */
  template<class T>
  T & 
  MyVector<T>::operator[] (MyString const & name) throw (HKLException)
  {
    typename MyVector<T>::iterator iter = vector<T>::begin();
    typename MyVector<T>::iterator last = vector<T>::end();

    while (iter != last){
      if (iter->get_name() == name)
        return *iter;
      ++iter;
    }
    
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Objects available are: ";

    iter = vector<T>::begin();
    while (iter != last){
      description << iter->get_name() << " ";
      ++iter;
    }   
    location << "MyVector<T>::operator[]";
    throw HKLException(reason.str(),
                       description.str(),
                       location.str());
  }

  /*!
   * \brief Return a constant reference on the Object named name.
   * \param name Name of the element to find in the MyVector.
   * \throw HKLException The Object is not store in the MyVector.
   * \return The constant reference of the Object.
   */
  template<class T>
  T const & 
  MyVector<T>::operator[] (MyString const & name) const throw (HKLException)
  {
    typename MyVector<T>::const_iterator iter = vector<T>::begin();
    typename MyVector<T>::const_iterator last = vector<T>::end();

    while (iter != last){
      if (iter->get_name() == name)
        return *iter;
      ++iter;
    }
    
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";

    iter = vector<T>::begin();
    while (iter != last){
      description << iter->get_name() << " ";
      ++iter;
    }   
    location << "MyVector<T>::operator[] const";
    throw HKLException(reason.str(),
                       description.str(),
                       location.str());
  }

  /*!
   * \brief compare two Myvector
   * \param myVector The MyVector to compare with.
   * \return true if they are identical. false otherwise.
   */
  template<class T>
  bool 
  MyVector<T>::operator== (MyVector const & myVector) const
  {
    if (vector<T>::size() != myVector.size())
      return false;
    typename MyVector<T>::const_iterator iter = vector<T>::begin();
    typename MyVector<T>::const_iterator last = vector<T>::end();
    typename MyVector<T>::const_iterator iter2 = myVector.begin();
     
    while (iter != last){
      if (!(*iter == *iter2))
        return false;
      ++iter;
      ++iter2;
    }
    return true;
  }

  /*!
   * \brief add a parameter to the MyVector
   * \param object the Object to add.
   * \throw HKLException The object is already present in the MyVector.
   */
  template<class T>
  bool
  MyVector<T>::add(T const & object) throw (HKLException)
  {
    typename MyVector<T>::iterator iter = vector<T>::begin();
    typename MyVector<T>::iterator last = vector<T>::end();

    MyString const & name = object.get_name();
    while(iter != last)
    {
      if (iter->get_name() == name)
      {
        ostringstream reason;
        reason << "The object \"" << object.get_name() << "\" already exist";
        throw HKLException(reason.str(),
                           "Please change the name of the object",
                           "MyVector<T>::add(T const & object)");
      }
      ++iter;
    }
    push_back(object);
    return true;
  }

  /*!
   * \brief remove the object named name from the MyVector
   * \param name The name of the object.
   * \throw HKLException The object names name is no present int he MyVector.
   * \return True if object removed.
   */
  template<class T>
  bool
  MyVector<T>::remove(MyString const & name) throw (HKLException)
  {
    typename MyVector<T>::iterator iter = vector<T>::begin();
    typename MyVector<T>::iterator last = vector<T>::end();

    while(iter != last)
    {
      if (iter->get_name() == name)
      {
        vector<T>::erase(iter);
        return true;
      }
      ++iter;
    }

    ostringstream reason;
    ostringstream description;

    reason << "The object \"" << name << "\" do not exist";
    description << "Removable Object are: ";
      
    iter = vector<T>::begin();
    while (iter != last){
      description << iter->get_name() << " ";
      ++iter;
    }   
    throw HKLException(reason.str(),
                       description.str(),
                       "MyVector<T>::remove(MyString const & name)");
  }
  
  /*!
   * \brief Get the name of all parameters in the MyVector
   * \return All the names as a vector of MyString.
   */
  template<class T>
  vector<MyString> 
  MyVector<T>::getNames (void) const
  {
    typename MyVector<T>::const_iterator iter = vector<T>::begin();
    typename MyVector<T>::const_iterator last = vector<T>::end();

    vector<MyString> result;

    while (iter != last){
      result.push_back(iter->get_name());
      ++iter;
    }
    return result;
  }

  /*!
   * \brief print in a human readable way the content of the MyVector.
   * \param flux the stream to modify.
   * \return the modified stream
   */
  template<class T>
  ostream & 
  MyVector<T>::printToStream(ostream  & flux) const
  {
    typename MyVector<T>::const_iterator iter = vector<T>::begin();
    typename MyVector<T>::const_iterator last = vector<T>::end();

    while (iter != last){
      flux << *iter;
      ++iter;
    }
    return flux;
  }

  /*!
   * \brief print on a stream the content of the MyVector.
   * \param flux the ostream to modify.
   * \return the modified ostream
   */
  template<class T>
  ostream & 
  MyVector<T>::toStream(ostream  & flux) const
  {
    typename MyVector<T>::const_iterator iter = vector<T>::begin();
    typename MyVector<T>::const_iterator last = vector<T>::end();

    flux << vector<T>::size() << " ";
    while (iter != last)
    {
      iter->toStream(flux);
      ++iter;
    }
    return flux;
  }

  /*!
   * \brief restore the content of the MyVector from an istream.
   * \param flux The istream.
   * \return The modified istream.
   */
  template<class T>
  istream &
  MyVector<T>::fromStream(istream  & flux)
  {
    unsigned int size;

    flux >> size;
    vector<T>::clear();
    for(unsigned int i=0;i<size;i++)
    {
      push_back(T());
      vector<T>::back().fromStream(flux);
    }
    return flux;
  }

  /* specialisation for pointer  */
/*
  template<class T>
  class MyVector<T*> : public vector<T*>
  {
    public:
      MyVector(void);
      MyVector(MyVector const & myVector);
      virtual ~MyVector(void);
      bool operator== (MyVector const & myVector) const;
      inline T * operator[](MyString const & name) throw (HKLException);
      inline T const * operator[](MyString const & name) const throw (HKLException);
      ostream & toStream(ostream & flux) const;
      istream & fromStream(istream & flux);
      void add(T * const object) throw (HKLException);
      vector<MyString> getNames(void) const;
  };

  template<class T>
  MyVector<T*>::MyVector(void)
  {}

  template<class T>
  MyVector<T*>::MyVector(MyVector const & myVector)
    : vector<T*>(myVector)
  {}
    
  template<class T>
  MyVector<T*>::~MyVector(void)
  {}

   
  template<class T>
  bool
  MyVector<T*>::operator== (MyVector const & myVector) const
  {
    // Pourquoi ne peut-on pas utiliser vector<T>::operator== ?
    if (vector<T*>::size() != myVector.size())
      return false;
    typename MyVector<T*>::const_iterator iter = vector<T*>::begin();
    typename MyVector<T*>::const_iterator last = vector<T*>::end();
    typename MyVector<T*>::const_iterator iter2 = myVector.begin();
     
    while (iter != last){
      if (!(*iter == *iter2))
        return false;
      ++iter;
      ++iter2;
    }
    return true;
  }

  template<class T>
  T * 
  MyVector<T*>::operator[] (MyString const & name) throw (HKLException)
  {
    typename MyVector<T*>::iterator iter = vector<T*>::begin();
    typename MyVector<T*>::iterator last = vector<T*>::end();

    while(iter != last){
      if ((*iter)->get_name() == name)
        return *iter;
      ++iter;
    }
    
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
    iter = vector<T*>::begin();
    while (iter != last){
      description << (*iter)->get_name() << " ";
      ++iter;
    }   
    location << "MyVector<T*>::operator[] specialize";
    throw HKLException(reason.str(),
                       description.str(),
                       location.str());
  }

  template<class T>
  T const * 
  MyVector<T*>::operator[] (MyString const & name) const throw (HKLException)
  {
    typename MyVector<T*>::const_iterator iter = vector<T*>::begin();
    typename MyVector<T*>::const_iterator last = vector<T*>::end();

    while(iter != last){
      if ((*iter)->get_name() == name)
        return *iter;
      ++iter;
    }
    
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
    iter = vector<T*>::begin();
    while (iter != last){
      description << (*iter)->get_name() << " ";
      ++iter;
    }   
    location << "MyVector<T*>::operator[] const specialize";
    throw HKLException(reason.str(),
                       description.str(),
                       location.str());
  }

  template<class T>
  void 
  MyVector<T*>::add(T * const object) throw (HKLException)
  {
    typename MyVector<T*>::const_iterator iter = vector<T*>::begin();
    typename MyVector<T*>::const_iterator last = vector<T*>::end();
    MyString const & name = object->get_name();
    while(iter != last){
      if ((*iter)->get_name() == name){
        ostringstream reason;
        reason << "The object \"" << name << "\" already exist";
        throw HKLException(reason.str(),
                           "Please change the name of the object",
                           "MyVector<T>::add(T const & object)");
      }
      ++iter;
    }
    push_back(object);
  }

  template<class T>
  vector<MyString> 
  MyVector<T*>::getNames (void) const
  {
    typename MyVector<T*>::const_iterator iter = vector<T*>::begin();
    typename MyVector<T*>::const_iterator last = vector<T*>::end();
    vector<MyString> result;

    while (iter != last){
      result.push_back((*iter)->get_name());
      ++iter;
    }
    return result;
  }

  template<class T>
  ostream & 
  MyVector<T*>::toStream(ostream  & flux) const
  {
    typename MyVector<T*>::const_iterator iter = vector<T*>::begin();
    typename MyVector<T*>::const_iterator last = vector<T*>::end();

    while (iter != last)
    {
      (*iter)->toStream(flux);
      ++iter;
    }
    return flux;
  }

  template<class T>
  istream &
  MyVector<T*>::fromStream(istream  & flux)
  {
    typename MyVector<T*>::iterator iter = vector<T*>::begin();
    typename MyVector<T*>::iterator last = vector<T*>::end();

    while (iter != last){
      (*iter)->fromStream(flux);
      ++iter;
    }
    return flux;
  }
*/
} // namespace hkl

#endif //_MYVECTOR_H_
