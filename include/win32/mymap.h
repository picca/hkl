#ifndef _MYMAP_H_
#define _MYMAP_H_

#include "config.h"

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
};

template<class T>
MyMap<T>::MyMap(void)
{}

template<class T>
MyMap<T>::MyMap(MyMap const & mymap) 
  : map<string, T>(mymap)
{}

template<class T>
T & MyMap<T>::operator[] (string const & name) throw (HKLException)
{
#ifdef VCPP6
  typename MyMap<T>::iterator iter = find(name);
  typename MyMap<T>::iterator last = end();
#else
  typename MyMap<T>::iterator iter = map<string, T>::find(name);
  typename MyMap<T>::iterator last = map<string, T>::end();
#endif
  if (iter == last)
  {
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
#ifdef VCPP6
    iter = begin();
#else
    iter = map<string, T>::begin();
#endif
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
T const & MyMap<T>::operator[] (string const & name) const throw (HKLException)
{

#ifdef VCPP6
  typename MyMap<T>::const_iterator iter = find(name);
  typename MyMap<T>::const_iterator last = end();
#else
  typename MyMap<T>::const_iterator iter = map<string, T>::find(name);
  typename MyMap<T>::const_iterator last = map<string, T>::end();
#endif
  if (iter == last)
  {
    ostringstream reason;
    ostringstream description;
    ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
#ifdef VCPP6
    iter = begin();
#else
    iter = map<string, T>::begin();
#endif
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
bool MyMap<T>::add(T const & object) throw (HKLException)
{
  typename MyMap<T>::iterator iter;
#ifdef VCPP6
  typename MyMap<T>::iterator last = end();
  pair<MyMap<T>::iterator, bool> is_insert = insert(MyMap<T>::value_type(object.get_name(), object));
#else
  typename MyMap<T>::iterator last = map<string, T>::end();
  pair<typename MyMap<T>::iterator, bool> is_insert = insert(typename MyMap<T>::value_type(object.get_name(), object));
#endif

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
bool MyMap<T>::remove(string const & name) throw (HKLException)
{
#ifdef VCPP6
  unsigned int n = erase(name);
#else
  unsigned int n = map<string, T>::erase(name);
#endif
  if (n == 0){
#ifdef VCPP6
    typename MyMap<T>::iterator iter = begin();
    typename MyMap<T>::iterator last = end();
#else
    typename MyMap<T>::iterator iter = map<string, T>::begin();
    typename MyMap<T>::iterator last = map<string, T>::end();
#endif
    ostringstream reason;
    ostringstream description;

    reason << "The object \"" << name << "\" do not exist";
    description << "Removable Object are: ";
#ifdef VCPP6
    iter = begin();
#else
    iter = map<string, T>::begin();
#endif
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
vector<string> MyMap<T>::getNames(void) const
{
#ifdef VCPP6
  typename MyMap<T>::const_iterator iter = begin();
  typename MyMap<T>::const_iterator last = end();
#else
  typename MyMap<T>::const_iterator iter = map<string, T>::begin();
  typename MyMap<T>::const_iterator last = map<string, T>::end();
#endif 
  vector<string> crystalNames;

  while (iter != last){
    crystalNames.push_back(iter->first);
    ++iter;
  }
  return crystalNames;
}

#ifdef VCPP6 // BUG de VC++6 Q240866 on ne peut pas specialiser un template SUPER!!!

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
T & MyStarMap<T>::operator[] (string const & name) throw (HKLException)
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
T const & MyStarMap<T>::operator[] (string const & name) const throw (HKLException)
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
vector<string> MyStarMap<T>::getNames(void) const
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
bool MyStarMap<T>::add(T const & object) throw (HKLException)
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

#else // ici on specialise MyMap pour le type pointer de C pour les compilateurs dignent de ce nom :)

template<class T>
class MyMap<T*> : public map<string, T*>
{
  public:
    void add(T const & object) throw (HKLException);
};

  template<class T>
void MyMap<T*>::add(T const & object) throw (HKLException)
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

#endif // VCPP6

#endif //_MYMAP_H_
