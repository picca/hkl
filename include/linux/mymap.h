#ifndef _MYMAP_H_
#define _MYMAP_H_

#include <map>
#include <string>
#include <vector>
#include <sstream>

#include "HKLException.h"

using namespace std;

template<class T>
class MyMap : public map<string, T>
{
  public:
    MyMap(void);
    MyMap(MyMap const & myMap);
    T & operator[] (string const & name) throw (HKLException);
    T const & operator[] (string const & name) const throw (HKLException);
    bool add(T const & object) throw (HKLException);
    bool remove(string const & name) throw (HKLException);
    vector<string> getNames(void) const;
    ostream & toStream(ostream & flux) const;
    istream & fromStream(istream & flux);
  };

template<class T>
MyMap<T>::MyMap(void)
{}

template<class T>
MyMap<T>::MyMap(MyMap const & mymap) 
  : map<string, T>(mymap)
{}

template<class T>
T &
MyMap<T>::operator[] (string const & name) throw (HKLException)
{
  typename MyMap<T>::iterator iter = map<string, T>::find(name);
  typename MyMap<T>::iterator last = map<string, T>::end();

  if (iter == last)
  {
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";

    iter = map<string, T>::begin();
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
MyMap<T>::operator[] (string const & name) const throw (HKLException)
{
  typename MyMap<T>::const_iterator iter = map<string, T>::find(name);
  typename MyMap<T>::const_iterator last = map<string, T>::end();

  if (iter == last)
  {
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";

    iter = map<string, T>::begin();
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
  typename MyMap<T>::iterator last = map<string, T>::end();
  pair<typename MyMap<T>::iterator, bool> is_insert = insert(typename MyMap<T>::value_type(object.get_name(), object));

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
MyMap<T>::remove(string const & name) throw (HKLException)
{
  unsigned int n = map<string, T>::erase(name);

  if (n == 0)
  {
    typename MyMap<T>::iterator iter = map<string, T>::begin();
    typename MyMap<T>::iterator last = map<string, T>::end();

    ostringstream reason;
    ostringstream description;

    reason << "The object \"" << name << "\" do not exist";
    description << "Removable Object are: ";
    
    iter = map<string, T>::begin();
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
vector<string>
MyMap<T>::getNames(void) const
{
  typename MyMap<T>::const_iterator iter = map<string, T>::begin();
  typename MyMap<T>::const_iterator last = map<string, T>::end();

  vector<string> crystalNames;

  while (iter != last){
    crystalNames.push_back(iter->first);
    ++iter;
  }
  return crystalNames;
}

/**
 * @brief print on a stream the content of the MyMap
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
template<class T>
ostream & 
MyMap<T>::toStream(ostream  & flux) const
{
  typename MyMap<T>::const_iterator iter = map<string, T>::begin();
  typename MyMap<T>::const_iterator last = map<string, T>::end();

  flux << map<string, T>::size() << " ";
  while (iter != last)
  {
    iter->second.toStream(flux);
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
MyMap<T>::fromStream(istream  & flux)
{
  unsigned int size;

  flux >> size;

  pair<typename MyMap<T>::iterator, bool> is_insert;
  map<string, T>::clear();
  for(unsigned int i=0;i<size;i++)
  {
    T object;
    object.fromStream(flux);
    add(object);
  }
  return flux;
}

// Specialisation for the pointer type. 
template<class T>
class MyMap<T*> : public map<string, T*>
{
  public:
    void add(T const & object) throw (HKLException);
    ostream & toStream(ostream & flux) const;
    istream & fromStream(istream & flux);
};

template<class T>
void
MyMap<T*>::add(T const & object) throw (HKLException)
{
  typename MyMap<T*>::iterator iter;
  typename MyMap<T*>::iterator last = map<string, T*>::end();
  iter = map<string, T*>::insert(MyMap<T*>::value_type(object->get_name(), object));
  if (iter == last){
    ostringstream reason;
    reason << "The object \"" << object->get_name() << "\" already exist";
    throw HKLException(reason.str(),
        "Please change the name of the object",
        "MyMap<T*>::add(T const & object)");
  }
}

/**
 * @brief print on a stream the content of the MyMap
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
template<class T>
ostream & 
MyMap<T*>::toStream(ostream  & flux) const
{
  typename MyMap<T*>::const_iterator iter = map<string, T*>::begin();
  typename MyMap<T*>::const_iterator last = map<string, T*>::end();

  while (iter != last)
  {
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
/*
template<class T>
istream &
MyMap<T*>::fromStream(istream  & flux)
{
  typename MyMap<T*>::iterator iter = map<string, T*>::begin();
  typename MyMap<T*>::iterator last = map<string, T*>::end();

  while (iter != last){
    iter->second->fromStream(flux);
    ++iter;
  }
  return flux;
}
*/
#endif //_MYMAP_H_
