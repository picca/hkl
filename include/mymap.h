#ifndef _MYMAP_H_
#define _MYMAP_H_

#include "config.h"

#include <map>
#include <string>
#include <vector>
#include <sstream>

#include "HKLException.h"

template<class T>
class MyMap : public std::map<std::string, T>
{
  public:
    MyMap();
    MyMap(MyMap const & myMap);
    T & operator[] (std::string const & name) throw (HKLException);
    T const & operator[] (std::string const & name) const throw (HKLException);
    bool add(T const & object) throw (HKLException);
    bool remove(std::string const & name) throw (HKLException);
    std::vector<std::string> getNames(void) const;
};

template<class T>
MyMap<T>::MyMap()
{}

template<class T>
MyMap<T>::MyMap( MyMap const & mymap) 
  : std::map<std::string, T>(mymap)
{}

template<class T>
T & MyMap<T>::operator[] (std::string const & name) throw (HKLException)
{
#ifdef VCPP6
  typename MyMap<T>::iterator iter = find(name);
  typename MyMap<T>::iterator last = end();
#else
  typename MyMap<T>::iterator iter = std::map<std::string, T>::find(name);
  typename MyMap<T>::iterator last = std::map<std::string, T>::end();
#endif
  if (iter == last){
    std::ostringstream reason;
    std::ostringstream description;
    std::ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
#ifdef VCPP6
	iter = begin();
#else
    iter = std::map<std::string, T>::begin();
#endif
    while (iter != last){
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
T const & MyMap<T>::operator[] (std::string const & name) const throw (HKLException)
{

#ifdef VCPP6
  typename MyMap<T>::const_iterator iter = find(name);
  typename MyMap<T>::const_iterator last = end();
#else
  typename MyMap<T>::const_iterator iter = std::map<std::string, T>::find(name);
  typename MyMap<T>::const_iterator last = std::map<std::string, T>::end();
#endif
  if (iter == last){
    std::ostringstream reason;
    std::ostringstream description;
    std::ostringstream location;
    reason << "The Object \"" << name << "\" does not exist";
    description << "Object available are: ";
#ifdef VCPP6
	iter = begin();
#else
    iter = std::map<std::string, T>::begin();
#endif
    while (iter != last){
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
  std::pair<MyMap<T>::iterator, bool> is_insert = insert(MyMap<T>::value_type(object.get_name(), object));
#else
  typename MyMap<T>::iterator last = std::map<std::string, T>::end();
  std::pair<typename MyMap<T>::iterator, bool> is_insert = insert(typename MyMap<T>::value_type(object.get_name(), object));
#endif

  if (!is_insert.second){
    std::ostringstream reason;
    reason << "The object \"" << object.get_name() << "\" already exist";
    throw HKLException(reason.str(),
                       "Please change the name of the object",
                       "MyMap<T>::add(T const & object)");
  } else
    return true;
}

template<class T>
bool MyMap<T>::remove(std::string const & name) throw (HKLException)
{
#ifdef VCPP6
  unsigned int n = erase(name);
#else
  unsigned int n = std::map<std::string, T>::erase(name);
#endif
  if (n == 0){
#ifdef VCPP6
    typename MyMap<T>::iterator iter = begin();
    typename MyMap<T>::iterator last = end();
#else
    typename MyMap<T>::iterator iter = std::map<std::string, T>::begin();
    typename MyMap<T>::iterator last = std::map<std::string, T>::end();
#endif
    std::ostringstream reason;
    std::ostringstream description;
    
    reason << "The object \"" << name << "\" do not exist";
    description << "Removable Object are: ";
#ifdef VCPP6
    iter = begin();
#else
    iter = std::map<std::string, T>::begin();
#endif
    while (iter != last){
      description << iter->first << " ";
      ++iter;
    }   
    throw HKLException(reason.str(),
                       description.str(),
                       "MyMap<T>::remove(std::string const & name)");
  } else
    return true;
}

template<class T>
std::vector<std::string> MyMap<T>::getNames(void) const
{
#ifdef VCPP6
  typename MyMap<T>::const_iterator iter = begin();
  typename MyMap<T>::const_iterator last = end();
#else
  typename MyMap<T>::const_iterator iter = std::map<std::string, T>::begin();
  typename MyMap<T>::const_iterator last = std::map<std::string, T>::end();
#endif 
  std::vector<std::string> crystalNames;
  
  while (iter != last){
    crystalNames.push_back(iter->first);
    ++iter;
  }
  return crystalNames;
}

#ifdef VCPP6 // BUG de VC++6 Q240866 on ne peut pas specialiser un template SUPER!!!

template<class T>
class MyStarMap :public std::map<std::string, T>
{
  public:
    inline T & operator[](std::string const & name) throw (HKLException);
    inline T const & operator[](std::string const & name) const throw (HKLException);
    bool add(T const & object) throw (HKLException);
    std::vector<std::string> getNames(void) const;
};

template<class T>
T & MyStarMap<T>::operator[] (std::string const & name) throw (HKLException)
{
  typename MyMap<T>::iterator iter = find(name);
  typename MyMap<T>::iterator last = end();

  if (iter == last){
    std::ostringstream reason;
    std::ostringstream description;
    std::ostringstream location;
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
T const & MyStarMap<T>::operator[] (std::string const & name) const throw (HKLException)
{
  typename MyMap<T>::const_iterator iter = find(name);
  typename MyMap<T>::const_iterator last = end();
  if (iter == last){
    std::ostringstream reason;
    std::ostringstream description;
    std::ostringstream location;
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
std::vector<std::string> MyStarMap<T>::getNames(void) const
{
  typename MyMap<T>::const_iterator iter = begin();
  typename MyMap<T>::const_iterator last = end();
  std::vector<std::string> crystalNames;
  
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
  std::pair<MyStarMap<T>::iterator, bool> is_insert = insert(MyStarMap<T>::value_type(object->get_name(), object));

  if (!is_insert.second){
    std::ostringstream reason;
    reason << "The object \"" << object->get_name() << "\" already exist";
    throw HKLException(reason.str(),
                       "Please change the name of the object",
                       "MyStarMap<T>::add(T const & object)");
  } else
    return true;
}

#else // ici on specialise MyMap pour le type pointer de C pour les compilateurs dignent de ce nom :)

template<class T>
class MyMap<T*> : public std::map<std::string, T*>
{
  public:
    void add(T const & object) throw (HKLException);
};

template<class T>
void MyMap<T*>::add(T const & object) throw (HKLException)
{
  typename MyMap<T*>::iterator iter;
  typename MyMap<T*>::iterator last = std::map<std::string, T*>::end();
  iter = std::map<std::string, T*>::insert(MyMap<T*>::value_type(object->get_name(), object));
  if (iter == last){
    std::ostringstream reason;
    reason << "The object \"" << object->get_name() << "\" already exist";
    throw HKLException(reason.str(),
                       "Please change the name of the object",
                       "MyMap<T*>::add(T const & object)");
  }
}

#endif // VCPP6

#endif //_MYMAP_H_
