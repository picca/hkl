#ifndef _MYVECTOR_H_
#define _MYVECTOR_H_

#include "config.h"

#include <vector>
#include <sstream>
#include <iostream>

#include "mystring.h"
#include "HKLException.h"

using namespace std;

namespace hkl {

template<class T>
class MyVector : public vector<T>
{
  public:
    MyVector(void);
    MyVector(MyVector const & myVector);
    virtual ~MyVector(void);
    bool operator== (MyVector const & myVector) const;
    T & operator[] (MyString const & name) throw (HKLException);
    T const & operator[] (MyString const & name) const throw (HKLException);
    ostream & printToStream(ostream & flux) const;
    ostream & toStream(ostream & flux) const;
    istream & fromStream(istream & flux);
    void add(T const & object) throw (HKLException);
    vector<MyString> getNames(void) const;
};

} // namespace hkl

/**
 * @brief Overload of the << operator for the #MyVector class
 * @param flux The flux to write into.
 * @param myVector The #MyVector to stream.
 * @return The modified flux.
 */
template<class T>
ostream & operator<<(ostream & flux, hkl::MyVector<T> const & myVector)
{
  myVector.printToStream(flux);
  return flux;
} 

namespace hkl {
	
/**
 * @brief Default constructor
 */
template<class T>
MyVector<T>::MyVector(void)
{}

/**
 * @brief Copy constructor
 */
template<class T>
MyVector<T>::MyVector(MyVector const & myVector) 
  : vector<T>(myVector)
{}

/**
 * @brief Default destructor
 */
template<class T>
MyVector<T>::~MyVector(void)
{}

/**
 * @brief compare two #Myvector
 * @param myvector The #MyVector to compare with.
 * @return true if they are identical. fals otherwise.
 */
template<class T>
bool
MyVector<T>::operator== (MyVector const & myVector) const
{
  // Pourquoi ne peut-on pas utiliser vector<T>::operator== ?
  if (size() != myVector.size())
    return false;
  typename MyVector<T>::const_iterator iter = begin();
  typename MyVector<T>::const_iterator last = end();
  typename MyVector<T>::const_iterator iter2 = myVector.begin();
   
  while (iter != last){
    if (!(*iter == *iter2))
      return false;
    ++iter;
    ++iter2;
  }
  return true;
}

/**
 * @brief [] overload
 * @param name the name of the element to access
 * @return the element with the right name
 */
template<class T>
T & 
MyVector<T>::operator[] (MyString const & name) throw (HKLException)
{
  typename MyVector<T>::iterator iter = begin();
  typename MyVector<T>::iterator last = end();

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

  iter = begin();
  while (iter != last){
    description << iter->get_name() << " ";
    ++iter;
  }   
  location << "MyVector<T>::operator[]";
  throw HKLException(reason.str(),
                     description.str(),
                     location.str());
}

/**
 * @brief [] overload
 * @param name the name of the element to access
 * @return the element with the right name
 */
template<class T>
T const &
MyVector<T>::operator[] (MyString const & name) const throw (HKLException)
{
  typename MyVector<T>::const_iterator iter = begin();
  typename MyVector<T>::const_iterator last = end();

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

  iter = begin();
  while (iter != last){
    description << iter->get_name() << " ";
    ++iter;
  }   
  location << "MyVector<T>::operator[] const";
  throw HKLException(reason.str(),
                     description.str(),
                     location.str());
}

/**
 * @brief print on a stream the content of the #MyVector
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
template<class T>
ostream &
MyVector<T>::printToStream(ostream  & flux) const
{
  typename MyVector<T>::const_iterator iter = begin();
  typename MyVector<T>::const_iterator last = end();

  while (iter != last){
    flux << *iter;
    ++iter;
  }
  return flux;
}
/**
 * @brief print on a stream the content of the MyVector
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
template<class T>
ostream & 
MyVector<T>::toStream(ostream  & flux) const
{
  typename MyVector<T>::const_iterator iter = begin();
  typename MyVector<T>::const_iterator last = end();

  flux << vector<T>::size() << " ";
  while (iter != last)
  {
    iter->toStream(flux);
    ++iter;
  }
  return flux;
}

/**
 * @brief restore the content of the MyVector from an istream.
 * @param flux the istream.
 * @return the modified istream.
 */
template<class T>
istream &
MyVector<T>::fromStream(istream  & flux)
{
  unsigned int size;

  flux >> size;
  clear();
  for(unsigned int i=0;i<size;i++)
  {
    push_back(T());
    back().fromStream(flux);
  }
  return flux;
}

/**
 * @brief add a parameter to the MyVector
 * @param object the Object to add
 */
template<class T>
void
MyVector<T>::add(T const & object) throw (HKLException)
{
  typename MyVector<T>::iterator iter = begin();
  typename MyVector<T>::iterator last = end();

  MyString const & name = object.get_name();
  while(iter != last){
    if (iter->get_name() == name){
      ostringstream reason;
      reason << "The object \"" << object.get_name() << "\" already exist";
      throw HKLException(reason.str(),
                         "Please change the name of the object",
                         "MyVector<T>::add(T const & object)");
    }
    ++iter;
  }
  push_back(object);
}

/**
 * @brief Get the name of all parameters in the #MyVector
 * @return All the names as a vector of MyString.
 */
template<class T>
vector<MyString> 
MyVector<T>::getNames (void) const
{
  typename MyVector<T>::const_iterator iter = begin();
  typename MyVector<T>::const_iterator last = end();

  vector<MyString> result;

  while (iter != last){
    result.push_back(iter->get_name());
    ++iter;
  }
  return result;
}

template<class T>
class MyStarVector: public vector<T>
{
  public:
    MyStarVector(void);
    MyStarVector(MyStarVector const & myStarVector);
    virtual ~MyStarVector(void);
    inline T & operator[](MyString const & name) throw (HKLException);
    inline T const & operator[](MyString const & name) const throw (HKLException);
    ostream & printToStream(ostream & flux) const;
    ostream & toStream(ostream & flux) const;
    istream & fromStream(istream & flux);
    void add(T const & object) throw (HKLException);
    vector<MyString> getNames(void) const;
};

} //namespace hkl

/**
 * @brief Overload of the << operator for the MyStarVector class
 * @param flux The flux to write into.
 * @param myVector The MyStarVector to stream.
 * @return The modified flux.
 */
template<class T>
ostream &
operator<<(ostream & flux, hkl::MyStarVector<T> const & myVector)
{
  myStarVector.printToStream(flux);
  return flux;
} 

namespace hkl {
	
template<class T>
MyStarVector<T>::MyStarVector(void)
{}

template<class T>
MyStarVector<T>::MyStarVector(MyStarVector const & myStarVector) 
  : vector<T>(myStarVector)
{}

template<class T>
MyStarVector<T>::~MyStarVector(void)
{}

template<class T>
T &
MyStarVector<T>::operator[] (MyString const & name) throw (HKLException)
{
  typename MyStarVector<T>::iterator iter = begin();
  typename MyStarVector<T>::iterator last = end();
  
  while (iter != last){
    if ((*iter)->get_name() == name)
      return *iter;
    ++iter;
  }
  
  ostringstream reason;
  ostringstream description;
  ostringstream location;
  reason << "The Object \"" << name << "\" does not exist";
  description << "Object available are: ";
  iter = begin();
  while (iter != last){
    description << (*iter)->get_name() << " ";
    ++iter;
  }   
  location << "MyStarVector<T>::operator[]";
  throw HKLException(reason.str(),
                     description.str(),
                     location.str());
}

template<class T>
T const &
MyStarVector<T>::operator[] (MyString const & name) const throw (HKLException)
{
  typename MyStarVector<T>::const_iterator iter = begin();
  typename MyStarVector<T>::const_iterator last = end();
  
  while (iter != last){
    if ((*iter)->get_name() == name)
      return *iter;
    ++iter;
  }
  
  ostringstream reason;
  ostringstream description;
  ostringstream location;
  reason << "The Object \"" << name << "\" does not exist";
  description << "Object available are: ";
  iter = begin();
  while (iter != last){
    description << (*iter)->get_name() << " ";
    ++iter;
  }   
  location << "MyStarVector<T>::operator[] const";
  throw HKLException(reason.str(),
                     description.str(),
                     location.str());
}

template<class T>
ostream &
MyStarVector<T>::printToStream(ostream  & flux) const
{
  typename MyStarVector<T>::const_iterator iter = begin();
  typename MyStarVector<T>::const_iterator last = end();

  while (iter != last){
    flux << **iter;
    ++iter;
  }
  return flux;
}

template<class T>
ostream & 
MyStarVector<T>::toStream(ostream  & flux) const
{
  typename MyStarVector<T>::const_iterator iter = begin();
  typename MyStarVector<T>::const_iterator last = end();

  while (iter != last)
  {
    (*iter)->toStream(flux);
    ++iter;
  }
  return flux;
}

template<class T>
istream &
MyStarVector<T>::fromStream(istream  & flux)
{
  typename MyStarVector<T>::iterator iter = begin();
  typename MyStarVector<T>::iterator last = end();

  while (iter != last){
    (*iter)->fromStream(flux);
    ++iter;
  }
  return flux;
}

template<class T>
void
MyStarVector<T>::add(T const & object) throw (HKLException)
{
  typename MyStarVector<T>::iterator iter = begin();
  typename MyStarVector<T>::iterator last = end();

  MyString const & name = object->get_name();
  while(iter != last){
    if ((*iter)->get_name() == name){
      ostringstream reason;
      reason << "The object \"" << object->get_name() << "\" already exist";
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
MyStarVector<T>::getNames (void) const
{
  typename MyStarVector<T>::const_iterator iter = begin();
  typename MyStarVector<T>::const_iterator last = end();
  vector<MyString> result;

  while (iter != last){
    result.push_back((*iter)->get_name());
    ++iter;
  }
  return result;
}

} // namespace hkl
#endif //_MYVECTOR_H_
