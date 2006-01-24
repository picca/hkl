#ifndef _MYMAP_H_
#define _MYMAP_H_

#include <map>
#include <vector>
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

} //namespace hkl

/**
 * @brief Overload of the << operator for the MyVector class
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
    typename MyMap<T>::iterator iter = map<MyString, T>::find(name);
    typename MyMap<T>::iterator last = map<MyString, T>::end();

    if (iter == last)
    {
      ostringstream reason;
      ostringstream description;
      ostringstream location;
      reason << "The Object \"" << name << "\" does not exist";
      description << "Object available are: ";

      iter = map<MyString, T>::begin();
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
    typename MyMap<T>::const_iterator iter = map<MyString, T>::find(name);
    typename MyMap<T>::const_iterator last = map<MyString, T>::end();

    if (iter == last)
    {
      ostringstream reason;
      ostringstream description;
      ostringstream location;
      reason << "The Object \"" << name << "\" does not exist";
      description << "Object available are: ";

      iter = map<MyString, T>::begin();
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
    typename MyMap<T>::iterator last = map<MyString, T>::end();
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
  MyMap<T>::remove(MyString const & name) throw (HKLException)
  {
    unsigned int n = map<MyString, T>::erase(name);

    if (n == 0)
    {
      typename MyMap<T>::iterator iter = map<MyString, T>::begin();
      typename MyMap<T>::iterator last = map<MyString, T>::end();

      ostringstream reason;
      ostringstream description;

      reason << "The object \"" << name << "\" do not exist";
      description << "Removable Object are: ";
      
      iter = map<MyString, T>::begin();
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
    typename MyMap<T>::const_iterator iter = map<MyString, T>::begin();
    typename MyMap<T>::const_iterator last = map<MyString, T>::end();

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
    typename MyMap<T>::const_iterator iter = map<MyString, T>::begin();
    typename MyMap<T>::const_iterator end = map<MyString, T>::end();
    while (iter != end)
    {
      iter->second.printToStream(flux);
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
  MyMap<T>::toStream(ostream  & flux) const
  {
    typename MyMap<T>::const_iterator iter = map<MyString, T>::begin();
    typename MyMap<T>::const_iterator end = map<MyString, T>::end();

    flux << " " << map<MyString, T>::size() << endl;
    while (iter != end)
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
    map<MyString, T>::clear();
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
  class MyMap<T*> : public map<MyString, T*>
  {
    public:
      MyMap(void);
      MyMap(MyMap const & myMap);
      T * & operator[] (MyString const & name) throw (HKLException);
      T * const & operator[] (MyString const & name) const throw (HKLException);
      bool operator== (MyMap const & myMap) const;
      bool add(T * const & object) throw (HKLException);
      vector<MyString> getNames(void) const;
      ostream & printToStream(ostream & flux) const;
      ostream & toStream(ostream & flux) const;
      istream & fromStream(istream & flux);
  };

} // namespace hkl

/**
 * @brief Overload of the << operator for the MyVector class
 * @param flux The flux to write into.
 * @param myVector The MyVector to stream.
 * @return The modified flux.
 */
template<class T>
ostream & operator<<(ostream & flux, hkl::MyMap<T*> const & myMap)
{
  myMap.printToStream(flux);
  return flux;
} 

namespace hkl {

  template<class T>
  MyMap<T*>::MyMap(void)
  {}

  template<class T>
  MyMap<T*>::MyMap(MyMap const & mymap) 
    : map<MyString, T*>(mymap)
  {}

  template<class T>
  T * &
  MyMap<T*>::operator[] (MyString const & name) throw (HKLException)
  {
    typename MyMap<T*>::iterator iter = map<MyString, T*>::find(name);
    typename MyMap<T*>::iterator last = map<MyString, T*>::end();

    if (iter == last)
    {
      ostringstream reason;
      ostringstream description;
      ostringstream location;
      reason << "The Object \"" << name << "\" does not exist";
      description << "Object available are: ";

      iter = map<MyString, T*>::begin();
      while (iter != last)
      {
        description << iter->first << " ";
        ++iter;
      }   
      location << "MyMap<T*>::operator[]";
      throw HKLException(reason.str(),
                                                 description.str(),
                         location.str());
    }
    return iter->second;
  }

  template<class T>
  T * const &
  MyMap<T*>::operator[] (MyString const & name) const throw (HKLException)
  {
    typename MyMap<T*>::const_iterator iter = map<MyString, T*>::find(name);
    typename MyMap<T*>::const_iterator last = map<MyString, T*>::end();

    if (iter == last)
    {
      ostringstream reason;
      ostringstream description;
      ostringstream location;
      reason << "The Object \"" << name << "\" does not exist";
      description << "Object available are: ";

      iter = map<MyString, T*>::begin();
      while (iter != last)
      {
        description << iter->first << " ";
        ++iter;
      }   
      location << "MyMap<T*>::operator[]";
      throw HKLException(reason.str(),
          description.str(),
          location.str());
    }
    return iter->second;
  }

  template<class T>
  bool
  MyMap<T*>::operator== (MyMap const & myMap) const
  {
    typename MyMap<T*>::const_iterator iter = map<MyString, T*>::begin();
    typename MyMap<T*>::const_iterator last = map<MyString, T*>::end();
    typename MyMap<T*>::const_iterator myMap_iter = myMap.begin();

    if (map<MyString, T*>::size() != myMap.size())
      return false;
    else 
      while(iter != last)
      {
        if (!(*(iter->second) == *(myMap_iter->second)))
          return false;
        else {
          ++iter;
          ++myMap_iter;
        }
      }
      return true;
  }

  template<class T>
  bool
  MyMap<T*>::add(T * const & object) throw (HKLException)
  {
    typename MyMap<T*>::iterator iter;
    typename MyMap<T*>::iterator last = map<MyString, T*>::end();
    pair<typename MyMap<T*>::iterator, bool> is_insert = insert(typename MyMap<T*>::value_type(object->get_name(), object));

    if (!is_insert.second){
      ostringstream reason;
      reason << "The object \"" << object->get_name() << "\" already exist";
      throw HKLException(reason.str(),
                         "Please change the name of the object",
                         "MyMap<T>::add(T const & object)");
    } else
      return true;
  }

  template<class T>
  vector<MyString>
  MyMap<T*>::getNames(void) const
  {
    typename MyMap<T*>::const_iterator iter = map<MyString, T*>::begin();
    typename MyMap<T*>::const_iterator last = map<MyString, T*>::end();

    vector<MyString> Names;

    while (iter != last){
      Names.push_back(iter->first);
      ++iter;
    }
    return Names;
  }

  template<class T>
  ostream &
  MyMap<T*>::printToStream(ostream & flux) const
  {
    typename MyMap<T*>::const_iterator iter = map<MyString, T*>::begin();
    typename MyMap<T*>::const_iterator end = map<MyString, T*>::end();
    while (iter != end)
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
  MyMap<T*>::toStream(ostream  & flux) const
  {
    typename MyMap<T*>::const_iterator iter = map<MyString, T*>::begin();
    typename MyMap<T*>::const_iterator end = map<MyString, T*>::end();

    flux << " " << map<MyString, T*>::size() << endl;
    while (iter != end)
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
  MyMap<T*>::fromStream(istream & flux)
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
