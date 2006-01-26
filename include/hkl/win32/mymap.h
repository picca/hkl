#ifndef _MYMAP_H_
#define _MYMAP_H_

#include "config.h"

#include <map>
#include <vector>
#include <iostream>
#include <sstream>

#include "mystring.h"
#include "HKLException.h"

using namespace std;

namespace hkl {

template<class T>
class MyMap : public map<MyString, T>
{
  public:
    MyMap(void);
    MyMap(MyMap const & myMap);
    T & operator[] (MyString const & name) throw (HKLException);
    T const & operator[] (MyString const & name) const throw (HKLException);
    bool add(T const & object) throw (HKLException);
    bool remove(MyString const & name) throw (HKLException);
    vector<MyString> getNames(void) const;
    ostream & printToStream(ostream & flux) const;
    ostream & toStream(ostream & flux) const;
    istream & fromStream(istream & flux);
};

} // namespace hkl

/**
 * @brief Overload of the << operator for the MyStarMap class
 * @param flux The flux to write into.
 * @param myVector The MyVector to stream.
 * @return The modified flux.
 */
template<class T>
ostream & operator<<(ostream & flux, hkl::MyMap<T> const & myMap)
{
  myMap.printToStream(flux);
  return flux;
}

namespace hkl {
	
template<class T>
MyMap<T>::MyMap(void)
{}

template<class T>
MyMap<T>::MyMap(MyMap const & mymap) 
  : map<MyString, T>(mymap)
{}

template<class T>
T & 
MyMap<T>::operator[] (MyString const & name) throw (HKLException)
{
  typename MyMap<T>::iterator iter = find(name);
  typename MyMap<T>::iterator last = end();
  if (iter == last)
  {
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
    
    iter = begin();
    
    while (iter != last)
    {
      description << iter->first << " ";
      ++iter;
    }   
    location << "MyMap<T>::operator[]";
    throw HKLException(reason.str(),
        description.str(),
        location.str());
  }
  return iter->second;
}

template<class T>
T const &
MyMap<T>::operator[] (MyString const & name) const throw (HKLException)
{

  typename MyMap<T>::const_iterator iter = find(name);
  typename MyMap<T>::const_iterator last = end();
  if (iter == last)
  {
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
    iter = begin();
    while (iter != last)
    {
      description << iter->first << " ";
      ++iter;
    }   
    location << "MyMap<T>::operator[]";
    throw HKLException(reason.str(),
        description.str(),
        location.str());
  }
  return iter->second;
}

template<class T>
bool
MyMap<T>::add(T const & object) throw (HKLException)
{
  typename MyMap<T>::iterator iter;
  typename MyMap<T>::iterator last = end();
  pair<MyMap<T>::iterator, bool> is_insert = insert(MyMap<T>::value_type(object.get_name(), object));

  if (!is_insert.second){
    ostringstream reason;
    reason << "The object \"" << object.get_name() << "\" already exist";
    throw HKLException(reason.str(),
        "Please change the name of the object",
        "MyMap<T>::add(T const & object)");
  } else
    return true;
}

template<class T>
bool
MyMap<T>::remove(MyString const & name) throw (HKLException)
{
  unsigned int n = erase(name);
  if (n == 0){
    typename MyMap<T>::iterator iter = begin();
    typename MyMap<T>::iterator last = end();
    ostringstream reason;
    ostringstream description;

    reason << "The object \"" << name << "\" do not exist";
    description << "Removable Object are: ";
    iter = begin();
    while (iter != last){
      description << iter->first << " ";
      ++iter;
    }   
    throw HKLException(reason.str(),
        description.str(),
        "MyMap<T>::remove(string const & name)");
  } else
    return true;
}

template<class T>
vector<MyString>
MyMap<T>::getNames(void) const
{
  typename MyMap<T>::const_iterator iter = begin();
  typename MyMap<T>::const_iterator last = end();
  vector<MyString> crystalNames;

  while (iter != last){
    crystalNames.push_back(iter->first);
    ++iter;
  }
  return crystalNames;
}

template<class T>
ostream &
MyMap<T>::printToStream(ostream & flux) const
{
  typename MyMap<T>::const_iterator iter = begin();
  typename MyMap<T>::const_iterator last = end();
  while (iter != last)
  {
    iter->second.printToStream(flux);
    ++iter;
  }
  return flux;
}

template<class T>
ostream &
MyMap<T>::toStream(ostream & flux) const
{
  typename MyMap<T>::const_iterator iter = begin();
  typename MyMap<T>::const_iterator last = end();

  flux << " " << size() << endl;
  while (iter != last)
  {
    iter->second.toStream(flux);
    ++iter;
  }
  return flux;
}

template<class T>
istream &
MyMap<T>::fromStream(istream & flux)
{
  unsigned int size;

  flux >> size;
  clear();
  for(unsigned int i=0;i<size;i++)
  {
    T object;
    object.fromStream(flux);
    add(object);
  }
  return flux;
}

template<class T>
class MyStarMap :public map<MyString, T>
{
  public:
    inline T & operator[](MyString const & name) throw (HKLException);
    inline T const & operator[](MyString const & name) const throw (HKLException);
    bool add(T const & object) throw (HKLException);
    bool operator== (MyStarMap const & myStarMap) const;
    vector<MyString> getNames(void) const;
    ostream & printToStream(ostream & flux) const;
    ostream & toStream(ostream & flux) const;
    istream & fromStream(istream & flux);
};

} // namespace hkl

/**
 * @brief Overload of the << operator for the MyStarMap class
 * @param flux The flux to write into.
 * @param myVector The MyVector to stream.
 * @return The modified flux.
 */
template<class T>
ostream & operator<<(ostream & flux, hkl::MyStarMap<T> const & myStarMap)
{
  myStarMap.printToStream(flux);
  return flux;
}

namespace hkl {

template<class T>
T & 
MyStarMap<T>::operator[] (MyString const & name) throw (HKLException)
{
  typename MyMap<T>::iterator iter = find(name);
  typename MyMap<T>::iterator last = end();

  if (iter == last){
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
    iter = begin();
    while (iter != last){
      description << iter->first << " ";
      ++iter;
    }   
    location << "MyStarMap<T>::operator[]";
    throw HKLException(reason.str(),
        description.str(),
        location.str());
  }
  return iter->second;
}

template<class T>
T const &
MyStarMap<T>::operator[] (MyString const & name) const throw (HKLException)
{
  typename MyMap<T>::const_iterator iter = find(name);
  typename MyMap<T>::const_iterator last = end();
  if (iter == last){
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
    iter = begin();

    while (iter != last){
      description << iter->first << " ";
      ++iter;
    }   
    location << "MyStarMap<T>::operator[]";
    throw HKLException(reason.str(),
        description.str(),
        location.str());
  }
  return iter->second;
}

template<class T>
bool
MyStarMap<T>::operator== (MyStarMap const & myStarMap) const
{
  typename MyStarMap<T>::const_iterator iter = begin();
  typename MyStarMap<T>::const_iterator last = end();
  typename MyStarMap<T>::const_iterator myStarMap_iter = myStarMap.begin();

  if (size() != myStarMap.size())
    return false;
  else 
    while(iter != last)
    {
      if (!(*(iter->second) == *(myStarMap_iter->second)))
        return false;
      else {
        ++iter;
        ++myStarMap_iter;
      }
    }
    return true;
}

template<class T>
bool
MyStarMap<T>::add(T const & object) throw (HKLException)
{
  typename MyStarMap<T>::iterator iter;

  typename MyStarMap<T>::iterator last = end();
  pair<MyStarMap<T>::iterator, bool> is_insert = insert(MyStarMap<T>::value_type(object->get_name(), object));

  if (!is_insert.second){
    ostringstream reason;
    reason << "The object \"" << object->get_name() << "\" already exist";
    throw HKLException(reason.str(),
        "Please change the name of the object",
        "MyStarMap<T>::add(T const & object)");
  } else
    return true;
}

template<class T>
vector<MyString>
MyStarMap<T>::getNames(void) const
{
  typename MyMap<T>::const_iterator iter = begin();
  typename MyMap<T>::const_iterator last = end();
  vector<MyString> crystalNames;

  while (iter != last){
    crystalNames.push_back(iter->first);
    ++iter;
  }
  return crystalNames;
}

template<class T>
ostream &
MyStarMap<T>::printToStream(ostream & flux) const
{
  typename MyStarMap<T>::const_iterator iter = begin();
  typename MyStarMap<T>::const_iterator last = end();
  while (iter != last)
  {
    iter->second->printToStream(flux);
    ++iter;
  }
  return flux;
}

/**
 * @brief print on a stream the content of the MyMap
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
template<class T>
ostream & 
MyStarMap<T>::toStream(ostream  & flux) const
{
  typename MyStarMap<T>::const_iterator iter = begin();
  typename MyStarMap<T>::const_iterator last = end();

  flux << " " << size() << endl;
  while (iter != last)
  {
    iter->first.toStream(flux);
    iter->second->toStream(flux);
    ++iter;
  }
  return flux;
}

/**
 * @brief restore the content of the MyMap from an istream
 * @param flux the istream.
 * @return the modified istream.
 */
template<class T>
istream &
MyStarMap<T>::fromStream(istream & flux)
{
  unsigned int size;
  MyString name;
  
  flux >> size;
  for(unsigned int i=0;i<size;i++)
  {
    name.fromStream(flux);
    (*this)[name]->fromStream(flux);
  }
  return flux;
}

} // namespace hkl
#endif //_MYMAP_H_
