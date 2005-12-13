#ifndef _MYMAP_H_
#define _MYMAP_H_

#include "config.h"

#include <map>
#include <string>
#include <vector>
#include <iostream>
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
MyMap<T>::operator[] (string const & name) const throw (HKLException)
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
MyMap<T>::remove(string const & name) throw (HKLException)
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
vector<string>
MyMap<T>::getNames(void) const
{
  typename MyMap<T>::const_iterator iter = begin();
  typename MyMap<T>::const_iterator last = end();
  vector<string> crystalNames;

  while (iter != last){
    crystalNames.push_back(iter->first);
    ++iter;
  }
  return crystalNames;
}

template<class T>
ostream &
MyMap<T>::toStream(ostream & flux) const
{
  return flux;
}

template<class T>
istream &
MyMap<T>::fromStream(istream & flux)
{
  return flux;
}

template<class T>
class MyStarMap :public map<string, T>
{
  public:
    inline T & operator[](string const & name) throw (HKLException);
    inline T const & operator[](string const & name) const throw (HKLException);
    bool add(T const & object) throw (HKLException);
    vector<string> getNames(void) const;
};

template<class T>
T & 
MyStarMap<T>::operator[] (string const & name) throw (HKLException)
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
MyStarMap<T>::operator[] (string const & name) const throw (HKLException)
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
vector<string>
MyStarMap<T>::getNames(void) const
{
  typename MyMap<T>::const_iterator iter = begin();
  typename MyMap<T>::const_iterator last = end();
  vector<string> crystalNames;

  while (iter != last){
    crystalNames.push_back(iter->first);
    ++iter;
  }
  return crystalNames;
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

#endif //_MYMAP_H_
