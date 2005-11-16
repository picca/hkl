#ifndef _MYVECTOR_H_
#define _MYVECTOR_H_

#include "config.h"

#include <vector>
#include <sstream>

#include "HKLException.h"

template<class T>
class MyVector : public std::vector<T>
{
  public:
    MyVector(void);
    MyVector(MyVector const & myVector);
    virtual ~MyVector(void);
    bool operator== (MyVector const & myVector) const;
    T & operator[] (std::string const & name) throw (HKLException);
    T const & operator[] (std::string const & name) const throw (HKLException);
    std::ostream & printToStream(std::ostream & flux) const;
    void add(T const & object) throw (HKLException);
    std::vector<std::string> getNames(void) const;
};

/**
 * @brief Overload of the << operator for the #MyVector class
 * @param flux The flux to write into.
 * @param myVector The #MyVector to stream.
 * @return The modified flux.
 */
template<class T>
std::ostream & operator<<(std::ostream & flux, MyVector<T> const & myVector)
{
  myVector.printToStream(flux);
  return flux;
} 

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
  : std::vector<T>(myVector)
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
bool MyVector<T>::operator== (MyVector const & myVector) const
{
  // Pourquoi ne peut-on pas utiliser std::vector<T>::operator== ?
#ifdef VCPP6
  if (size() != myVector.size())
    return false;
  typename MyVector<T>::const_iterator iter = begin();
  typename MyVector<T>::const_iterator last = end();
  typename MyVector<T>::const_iterator iter2 = myVector.begin();
#else
  if (std::vector<T>::size() != myVector.size())
    return false;
  typename MyVector<T>::const_iterator iter = std::vector<T>::begin();
  typename MyVector<T>::const_iterator last = std::vector<T>::end();
  typename MyVector<T>::const_iterator iter2 = myVector.begin();
#endif
   
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
T & MyVector<T>::operator[] (std::string const & name) throw (HKLException)
{
#ifdef VCPP6
  typename MyVector<T>::iterator iter = begin();
  typename MyVector<T>::iterator last = end();
#else
  typename MyVector<T>::iterator iter = std::vector<T>::begin();
  typename MyVector<T>::iterator last = std::vector<T>::end();
#endif
  while (iter != last){
    if (iter->get_name() == name)
      return *iter;
    ++iter;
  }
  
  std::ostringstream reason;
  std::ostringstream description;
  std::ostringstream location;
  reason << "The Object \"" << name << "\" does not exist";
  description << "Objects available are: ";
#ifdef VCPP6
  iter = begin();
#else
  iter = std::vector<T>::begin();
#endif
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
T const & MyVector<T>::operator[] (std::string const & name) const throw (HKLException)
{
#ifdef VCPP6
  typename MyVector<T>::const_iterator iter = begin();
  typename MyVector<T>::const_iterator last = end();
#else
  typename MyVector<T>::const_iterator iter = std::vector<T>::begin();
  typename MyVector<T>::const_iterator last = std::vector<T>::end();
#endif

  while (iter != last){
    if (iter->get_name() == name)
      return *iter;
    ++iter;
  }
  
  std::ostringstream reason;
  std::ostringstream description;
  std::ostringstream location;
  reason << "The Object \"" << name << "\" does not exist";
  description << "Object available are: ";
#ifdef VCPP6
  iter = begin();
#else
  iter = std::vector<T>::begin();
#endif
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
std::ostream & MyVector<T>::printToStream(std::ostream  & flux) const
{
#ifdef VCPP6
  typename MyVector<T>::const_iterator iter = begin();
  typename MyVector<T>::const_iterator last = end();
#else
  typename MyVector<T>::const_iterator iter = std::vector<T>::begin();
  typename MyVector<T>::const_iterator last = std::vector<T>::end();
#endif

  while (iter != last){
    flux << *iter;
    ++iter;
  }
  return flux;
}

/**
 * @brief add a parameter to the #MyVector
 * @param object the #Object to add
 */
template<class T>
void MyVector<T>::add(T const & object) throw (HKLException)
{
#ifdef VCPP6
  typename MyVector<T>::iterator iter = begin();
  typename MyVector<T>::iterator last = end();
#else
  typename MyVector<T>::iterator iter = std::vector<T>::begin();
  typename MyVector<T>::iterator last = std::vector<T>::end();
#endif
  std::string const & name = object.get_name();
  while(iter != last){
    if (iter->get_name() == name){
      std::ostringstream reason;
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
 * @return All the names as a vector of string.
 */
template<class T>
std::vector<std::string> 
MyVector<T>::getNames (void) const
{
#ifdef VCPP6
  typename MyVector<T>::const_iterator iter = begin();
  typename MyVector<T>::const_iterator last = end();
#else
  typename MyVector<T>::const_iterator iter = std::vector<T>::begin();
  typename MyVector<T>::const_iterator last = std::vector<T>::end();
#endif
  std::vector<std::string> result;

  while (iter != last){
    result.push_back(iter->get_name());
    ++iter;
  }
  return result;
}

#ifdef VCPP6 // BUG de VC++6 Q240866 on ne peut pas specialiser un template ici pour les pointeurs.

template<class T>
class MyStarVector: public std::vector<T>
{
  public:
    MyStarVector(void);
    MyStarVector(MyStarVector const & myStarVector);
    virtual ~MyStarVector(void);
    inline T & operator[](std::string const & name) throw (HKLException);
    inline T const & operator[](std::string const & name) const throw (HKLException);
    std::ostream & printToStream(std::ostream & flux) const;
    void add(T const & object) throw (HKLException);
    std::vector<std::string> getNames(void) const;
};

/**
 * @brief Overload of the << operator for the MyStarVector class
 * @param flux The flux to write into.
 * @param myVector The MyStarVector to stream.
 * @return The modified flux.
 */
template<class T>
std::ostream & operator<<(std::ostream & flux, MyStarVector<T> const & myVector)
{
  myStarVector.printToStream(flux);
  return flux;
} 

template<class T>
MyStarVector<T>::MyStarVector(void)
{}

template<class T>
MyStarVector<T>::MyStarVector(MyStarVector const & myStarVector) 
  : std::vector<T>(myStarVector)
{}

template<class T>
MyStarVector<T>::~MyStarVector(void)
{}

template<class T>
T & MyStarVector<T>::operator[] (std::string const & name) throw (HKLException)
{
  typename MyStarVector<T>::iterator iter = begin();
  typename MyStarVector<T>::iterator last = end();
  
  while (iter != last){
    if ((*iter)->get_name() == name)
      return *iter;
    ++iter;
  }
  
  std::ostringstream reason;
  std::ostringstream description;
  std::ostringstream location;
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
T const & MyStarVector<T>::operator[] (std::string const & name) const throw (HKLException)
{
  typename MyStarVector<T>::const_iterator iter = begin();
  typename MyStarVector<T>::const_iterator last = end();
  
  while (iter != last){
    if ((*iter)->get_name() == name)
      return *iter;
    ++iter;
  }
  
  std::ostringstream reason;
  std::ostringstream description;
  std::ostringstream location;
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
std::ostream & MyStarVector<T>::printToStream(std::ostream  & flux) const
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
void MyStarVector<T>::add(T const & object) throw (HKLException)
{
  typename MyStarVector<T>::iterator iter = begin();
  typename MyStarVector<T>::iterator last = end();

  std::string const & name = object->get_name();
  while(iter != last){
    if ((*iter)->get_name() == name){
      std::ostringstream reason;
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
std::vector<std::string> 
MyStarVector<T>::getNames (void) const
{
  typename MyStarVector<T>::const_iterator iter = begin();
  typename MyStarVector<T>::const_iterator last = end();
  std::vector<std::string> result;

  while (iter != last){
    result.push_back((*iter)->get_name());
    ++iter;
  }
  return result;
}

#else // ici on specialise MyVector pour le type pointer de C pour les compilateurs dignent de ce nom :)

template<class T>
class MyVector<T*> : public std::vector<T*>
{
  public:
    virtual ~MyVector(void);
    inline T * operator[](std::string const & name) throw (HKLException);
    inline T const * operator[](std::string const & name) const throw (HKLException);
    void add(T * const  & object) throw (HKLException);
    std::vector<std::string> getNames(void) const;
};

template<class T>
MyVector<T*>::~MyVector(void)
{}

template<class T>
T * MyVector<T*>::operator[] (std::string const & name) throw (HKLException)
{
  typename MyVector<T*>::iterator iter = std::vector<T*>::begin();
  typename MyVector<T*>::iterator last = std::vector<T*>::end();

  while(iter != last){
    if ((*iter)->get_name() == name)
      return *iter;
    ++iter;
  }
  
  std::ostringstream reason;
  std::ostringstream description;
  std::ostringstream location;
  reason << "The Object \"" << name << "\" does not exist";
  description << "Object available are: ";
  iter = std::vector<T*>::begin();
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
T const * MyVector<T*>::operator[] (std::string const & name) const throw (HKLException)
{
  typename MyVector<T*>::const_iterator iter = std::vector<T*>::begin();
  typename MyVector<T*>::const_iterator last = std::vector<T*>::end();

  while(iter != last){
    if ((*iter)->get_name() == name)
      return *iter;
    ++iter;
  }
  
  std::ostringstream reason;
  std::ostringstream description;
  std::ostringstream location;
  reason << "The Object \"" << name << "\" does not exist";
  description << "Object available are: ";
  iter = std::vector<T*>::begin();
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
void MyVector<T*>::add(T * const & object) throw (HKLException)
{
  typename MyVector<T*>::const_iterator iter = std::vector<T*>::begin();
  typename MyVector<T*>::const_iterator last = std::vector<T*>::end();
  std::string const & name = object->get_name();
  while(iter != last){
    if ((*iter)->get_name() == name){
      std::ostringstream reason;
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
std::vector<std::string> 
MyVector<T*>::getNames (void) const
{
  typename MyVector<T*>::const_iterator iter = std::vector<T*>::begin();
  typename MyVector<T*>::const_iterator last = std::vector<T*>::end();
  std::vector<std::string> result;

  while (iter != last){
    result.push_back((*iter)->get_name());
    ++iter;
  }
  return result;
}

#endif // VCPP6

#endif //_MYVECTOR_H_
