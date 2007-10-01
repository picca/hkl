#ifndef _MYMAP_H
#define _MYMAP_H


#include <map>
#include <string>
#include "HKLException.h"
#include <vector>
#include <ostream>
#include <istream>

namespace hkl
  {

  template<class T>
  class MyMap : public std::map<std::string, T>
    {
    public:
      /**
       * @brief Overload of the [] operator.
       * @param name The name of the element to return.
       * @throw HKLException the element named name is not in the MyMap.
       * @return The element named name.
       */
      T & operator[](const std::string & name) throw(hkl::HKLException);

      /**
       * @brief Overload of the [] operator.
       * @param name The name of the element to return.
       * @throw HKLException the element named name is not in the MyMap.
       * @return The element named name.
       */
      const T & operator[](const std::string & name) const throw(hkl::HKLException);

      /**
       * @brief Add a T to the MyMap.
       * @param object The T to add.
       * @throw HKLException The object is already in the MyMap.
       * @return True if ok false otherwise.
       */
      bool add(const T & object) throw(hkl::HKLException);

      /**
       * @brief remove the std::string named name from the MyMap
       * @param name The name of the std::string.
       * @throw HKLException The object names name is no present in the MyMap.
       * @return True if object removed.
       */
      bool remove(const std::string & name) throw(hkl::HKLException);

      vector<string> getNames() const;

      /*!
       * \brief print the MyMap into a flux
       * \param flux The stream to print into.
       */
      std::ostream & printToStream(std::ostream & flux) const;

    };
  /**
   * @brief Overload of the [] operator.
   * @param name The name of the element to return.
   * @throw HKLException the element named name is not in the MyMap<T>.
   * @return The element named name.
   */
  template<class T>
  T & MyMap<T>::operator[](const std::string & name) throw(hkl::HKLException)
  {
    // Bouml preserved body begin 00026402
    typename MyMap<T>::iterator iter = map<string, T>::find(name);
    typename MyMap<T>::iterator last = map<string, T>::end();

    if (iter == last)
      {
        ostringstream reason;
        ostringstream description;
        reason << "The " << typeid(T).name() << " named \"" << name << "\" does not exist.";
        if (map<string, T>::size())
          {
            description << "Available " << typeid(T).name() << " are:";

            iter = map<string, T>::begin();
            while (iter != last)
              {
                description << " \"" << iter->first << "\"";
                ++iter;
              }
          }
        else
          description << "No " << typeid(T).name() << " available.";
        HKLEXCEPTION(reason.str(),
                     description.str());
      }
    return iter->second;
    // Bouml preserved body end 00026402
  }

  /**
   * @brief Overload of the [] operator.
   * @param name The name of the element to return.
   * @throw HKLException the element named name is not in the MyMap<T>.
   * @return The element named name.
   */
  template<class T>
  const T & MyMap<T>::operator[](const std::string & name) const throw(hkl::HKLException)
  {
    // Bouml preserved body begin 00026482
    typename MyMap<T>::const_iterator iter = map<string, T>::find(name);
    typename MyMap<T>::const_iterator last = map<string, T>::end();

    if (iter == last)
      {
        ostringstream reason;
        ostringstream description;
        reason << "The " << typeid(T).name() << " named \"" << name << "\" does not exist.";
        if (map<string, T>::size())
          {
            description << "Available " << typeid(T).name() << " are:";

            iter = map<string, T>::begin();
            while (iter != last)
              {
                description << " \"" << iter->first << "\"";
                ++iter;
              }
          }
        else
          description << "No " << typeid(T).name() << " available.";
        HKLEXCEPTION(reason.str(),
                     description.str());
      }
    return iter->second;
    // Bouml preserved body end 00026482
  }

  /**
   * @brief Add a T to the MyMap<T>.
   * @param object The T to add.
   * @throw HKLException The object is already in the MyMap<T>.
   * @return True if ok false otherwise.
   */
  template<class T>
  bool MyMap<T>::add(const T & object) throw(hkl::HKLException)
  {
    // Bouml preserved body begin 00026502
    typename MyMap<T>::iterator iter;
    typename MyMap<T>::iterator last = map<string, T>::end();

    pair<typename MyMap<T>::iterator, bool> is_insert = map<string, T>::insert(typename MyMap<T>::value_type(object.get_name(), object));


    if (!is_insert.second)
      {
        ostringstream reason;
        reason << "The " << typeid(T).name() << " named \"" << object.get_name() << "\" already exist.";
        HKLEXCEPTION(reason.str(),
                     "Please change its name.");
      }
    else
      return true;
    // Bouml preserved body end 00026502
  }

  /**
   * @brief remove the std::string named name from the MyMap<T>
   * @param name The name of the std::string.
   * @throw HKLException The object names name is no present in the MyMap<T>.
   * @return True if object removed.
   */
  template<class T>
  bool MyMap<T>::remove(const std::string & name) throw(hkl::HKLException)
  {
    // Bouml preserved body begin 00026582
    unsigned int n = map<string, T>::erase(name);

    if (n == 0)
      {
        typename MyMap<T>::iterator iter = map<string, T>::begin();
        typename MyMap<T>::iterator last = map<string, T>::end();

        ostringstream reason;
        ostringstream description;

        reason << "The " << typeid(T).name() << " named \"" << name << "\" do not exist.";
        if (map<string, T>::size())
          {
            description << "Removable " << typeid(T).name() << " are:";

            while (iter != last)
              {
                description << " \"" << iter->first << "\"";
                ++iter;
              }
          }
        else
          description << "No " << typeid(T).name() << " available.";
        HKLEXCEPTION(reason.str(),
                     description.str());
      }
    else
      return true;
    // Bouml preserved body end 00026582
  }

  template<class T>
  vector<string> MyMap<T>::getNames() const
    {
      // Bouml preserved body begin 00026602
      typename MyMap<T>::const_iterator iter = map<string, T>::begin();
      typename MyMap<T>::const_iterator last = map<string, T>::end();

      vector<string> crystalNames;

      while (iter != last)
        {
          crystalNames.push_back(iter->first);
          ++iter;
        }
      return crystalNames;
      // Bouml preserved body end 00026602
    }

  /*!
   * \brief print the MyMap<T> into a flux
   * \param flux The stream to print into.
   */
  template<class T>
  std::ostream & MyMap<T>::printToStream(std::ostream & flux) const
    {
      // Bouml preserved body begin 00026682
      typename MyMap<T>::const_iterator iter = map<string, T>::begin();
      typename MyMap<T>::const_iterator end = map<string, T>::end();
      while (iter != end)
        {
          iter->second.printToStream(flux);
          ++iter;
        }
      return flux;
      // Bouml preserved body end 00026682
    }

} // namespace hkl
#endif
