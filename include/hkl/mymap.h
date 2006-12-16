#ifndef _MYMAP_H_
#define _MYMAP_H_

#include "portability.h"

#include <map>
#include <sstream>
#include <typeinfo>

#include "mystring.h"
#include "HKLException.h"

using namespace std;

namespace hkl
{

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

/*!
 * \brief Default constructor
 */
template<class T>
MyMap<T>::MyMap(void)
{}

/*!
 * \brief Copy Constructor
 * \param mymap The MyMap to copy from.
 */
template<class T>
MyMap<T>::MyMap(MyMap const & mymap)
        : map<MyString, T>(mymap)
{}

/*!
 * \brief Overload of the [] operator.
 * \param name The name of the element to return.
 * \throw HKLException the element named name is not in the MyMap.
 * \return The element named name.
 */
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
        reason << "The " << typeid(T).name() << " named \"" << name << "\" does not exist.";
        if (map<MyString, T>::size())
        {
            description << "Available " << typeid(T).name() << " are:";

            iter = map<MyString, T>::begin();
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
}

/*!
 * \brief Overload of the [] operator.
 * \param name The name of the element to return.
 * \throw HKLException the element named name is not in the MyMap.
 * \return The a constant reference on the element named name.
 */
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
        reason << "The " << typeid(T).name() << " named \"" << name << "\" does not exist.";
        if (map<MyString, T>::size())
        {
            description << "Available " << typeid(T).name() << " are:";

            iter = map<MyString, T>::begin();
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
}

/*!
 * \brief Add an Object to the MyMap.
 * \param object The Object to add.
 * \throw HKLException The object is already in the MyMap.
 * \return True if ok false otherwise.
 */
template<class T>
bool
MyMap<T>::add(T const & object) throw (HKLException)
{
    typename MyMap<T>::iterator iter;
    typename MyMap<T>::iterator last = map<MyString, T>::end();
#ifdef MSVC6
    pair<MyMap<T>::iterator, bool> is_insert = map<MyString, T>::insert(MyMap<T>::value_type(object.get_name(), object));
#else
    pair<typename MyMap<T>::iterator, bool> is_insert = map<MyString, T>::insert(typename MyMap<T>::value_type(object.get_name(), object));
#endif

    if (!is_insert.second)
    {
        ostringstream reason;
        reason << "The " << typeid(T).name() << " named \"" << object.get_name() << "\" already exist.";
        HKLEXCEPTION(reason.str(),
                     "Please change its name.");
    }
    else
        return true;
}

/*!
 * \brief remove the object named name from the MyMap
 * \param name The name of the object.
 * \throw HKLException The object names name is no present int he MyMap.
 * \return True if object removed.
 */
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

        reason << "The " << typeid(T).name() << " named \"" << name << "\" do not exist.";
        if (map<MyString, T>::size())
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
}

/*!
 * \brief Return The names of all objects in the MyMap.
 * \return A vector of MyString filled with the Object names.
 */
template<class T>
vector<MyString>
MyMap<T>::getNames(void) const
{
    typename MyMap<T>::const_iterator iter = map<MyString, T>::begin();
    typename MyMap<T>::const_iterator last = map<MyString, T>::end();

    vector<MyString> crystalNames;

    while (iter != last)
    {
        crystalNames.push_back(iter->first);
        ++iter;
    }
    return crystalNames;
}

/*!
 * \brief Prient the MyMap in a human readable way.
 * \param flux The stream to write into.
 * \return The modified stream.
 */
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

/*!
 * \brief print on a stream the content of the MyMap
 * \param flux the ostream to modify.
 * \return the modified ostream
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

/*!
 * \brief restore the content of the MyMap from an istream
 * \param flux the istream.
 * \return the modified istream.
 */
template<class T>
istream &
MyMap<T>::fromStream(istream  & flux)
{
    unsigned int size;

    flux >> size;
    if (map<MyString, T>::size() == size)
    {
        typename MyMap<T>::iterator iter = MyMap<T>::begin();
        typename MyMap<T>::iterator end = MyMap<T>::end();
        while(iter != end)
        {
            iter->second.fromStream(flux);
            ++iter;
        }
    }
    return flux;
}

} // namespace hkl

/*!
 * @brief Overload of the << operator for the MyVector class
 * @param flux The flux to write into.
 * @param myMap The MyMap to stream.
 * @return The modified flux.
 */
template<class T>
ostream & operator<<(ostream & flux, hkl::MyMap<T> const & myMap)
{
    myMap.printToStream(flux);
    return flux;
}

#ifdef MSVC6
# define MyMap MyStarMap
#define _T T
#else
#define _T T*
#endif

namespace hkl
{

// Specialisation for the pointer type.
template<typename T>
class
#ifdef MSVC6
            MyMap
#else
            MyMap<_T>
#endif
            : public map<MyString, _T>
{
public:
    MyMap(void);
    MyMap(MyMap const & myMap);
    bool operator== (MyMap const & myMap) const;
    MyMap & operator= (MyMap const & myMap);
    _T & operator[] (MyString const & name) throw (HKLException);
    _T const & operator[] (MyString const & name) const throw (HKLException);
    bool add(_T const & object) throw (HKLException);
    vector<MyString> getNames(void) const;
    void free(void);
    ostream & printToStream(ostream & flux) const;
    ostream & toStream(ostream & flux) const;
    istream & fromStream(istream & flux);
};

/*!
 * \brief Default constructor
 */
template<class T>
MyMap<_T>::MyMap(void) :
        map<MyString, _T>()
{}

/*!
 * \brief Copy constructor
 */
template<class T>
MyMap<_T>::MyMap(MyMap const & mymap)
        : map<MyString, _T>(mymap)
{}

/*!
 * \brief Overload of the [] operator.
 * \param name The name of the element to return.
 * \throw HKLException the element named name is not in the MyMap.
 * \return The element named name.
 */
template<class T>
_T &
MyMap<_T>::operator[] (MyString const & name) throw (HKLException)
{
    typename MyMap<_T>::iterator iter = map<MyString, _T>::find(name);
    typename MyMap<_T>::iterator last = map<MyString, _T>::end();

    if (iter == last)
    {
        ostringstream reason;
        ostringstream description;
        reason << "The " << typeid(T).name() << " \"" <<  name << "\" does not exist";
        if (map<MyString, _T>::size())
        {
            description << typeid(T).name() << " available are: ";

            iter = map<MyString, _T>::begin();
            while (iter != last)
            {
                description << iter->first << " ";
                ++iter;
            }
        }
        else
            description << "No " <<  typeid(T).name() << " available.";
        HKLEXCEPTION(reason.str(),
                     description.str());
    }
    return iter->second;
}

/*!
 * \brief Overload of the [] operator.
 * \param name The name of the element to return.
 * \throw HKLException the element named name is not in the MyMap.
 * \return The a constant reference on the element named name.
 */
template<class T>
_T const &
MyMap<_T>::operator[] (MyString const & name) const throw (HKLException)
{
    typename MyMap<_T>::const_iterator iter = map<MyString, _T>::find(name);
    typename MyMap<_T>::const_iterator last = map<MyString, _T>::end();

    if (iter == last)
    {
        ostringstream reason;
        ostringstream description;
        reason << "The " << typeid(T).name() << " \"" <<  name << "\" does not exist";
        description << typeid(T).name() << " available are: ";

        iter = map<MyString, _T>::begin();
        while (iter != last)
        {
            description << iter->first << " ";
            ++iter;
        }
        HKLEXCEPTION(reason.str(),
                     description.str());
    }
    return iter->second;
}

/*!
 * \brief Compare with another MyMap.
 * \param myMap The MyMap to compare with.
 * \return True if both are equals.
 */
template<class T>
bool
MyMap<_T>::operator== (MyMap const & myMap) const
{
    typename MyMap<_T>::const_iterator iter = map<MyString, _T>::begin();
    typename MyMap<_T>::const_iterator last = map<MyString, _T>::end();
    typename MyMap<_T>::const_iterator myMap_iter = myMap.begin();

    if (map<MyString, _T>::size() != myMap.size())
        return false;
    else
        while(iter != last)
        {
            if (!(*(iter->second) == *(myMap_iter->second)))
                return false;
            else
            {
                ++iter;
                ++myMap_iter;
            }
        }
    return true;
}

/**
 * @brief Assign a MyMap to another one.
 * @param myMap the MyMap to assign.
 * @return The modified MyMap
 */
template<typename T>
MyMap<_T> &
MyMap<_T>::operator=(MyMap const & myMap)
{
    typename MyMap<_T>::iterator iter = map<MyString, _T>::begin();
    typename MyMap<_T>::iterator last = map<MyString, _T>::end();
    typename MyMap<_T>::const_iterator myMap_iter = myMap.begin();

    while(iter != last)
    {
        *(myMap_iter->second) = *(iter->second);
        ++iter;
        ++myMap_iter;
    }
    return *this;
}

/*!
 * \brief Add an Object to the MyMap.
 * \param object The Object to add.
 * \throw HKLException The object is already in the MyMap.
 * \return True if ok false otherwise.
 */
template<class T>
bool
MyMap<_T>::add(_T const & object) throw (HKLException)
{
    typename MyMap<_T>::iterator iter;
    typename MyMap<_T>::iterator last = map<MyString, _T>::end();
#ifdef MSVC6
    pair<MyMap<_T>::iterator, bool> is_insert = map<MyString, _T>::insert(MyMap<_T>::value_type(object->get_name(), object));
#else
    pair<typename MyMap<_T>::iterator, bool> is_insert = map<MyString, _T>::insert(typename MyMap<_T>::value_type(object->get_name(), object));
#endif

    if (!is_insert.second)
    {
        ostringstream reason;
        reason << "The \"" << typeid(_T).name() << "\" " << "named :\"" << object->get_name() << "\" already exist";
        HKLEXCEPTION(reason.str(), "Please change its name.");
    }
    else
        return true;
}

/*!
 * \brief Return The names of all objects in the MyMap.
 * \return A vector of MyString filled with the Object names.
 */
template<class T>
vector<MyString>
MyMap<_T>::getNames(void) const
{
    typename MyMap<_T>::const_iterator iter = map<MyString, _T>::begin();
    typename MyMap<_T>::const_iterator last = map<MyString, _T>::end();

    vector<MyString> Names;

    while (iter != last)
    {
        Names.push_back(iter->first);
        ++iter;
    }
    return Names;
}

/*!
 * \brief delete all objects the objets in the MyMap.
 */
template<class T>
void
MyMap<_T>::free(void)
{
    typename MyMap<_T>::iterator iter = map<MyString, _T>::begin();
    typename MyMap<_T>::iterator last = map<MyString, _T>::end();

    while (iter != last)
    {
        delete iter->second;
        ++iter;
    }
    map<MyString, _T>::clear();
}

/*!
 * \brief Prient the MyMap in a human readable way.
 * \param flux The stream to write into.
 * \return The modified stream.
 */
template<class T>
ostream &
MyMap<_T>::printToStream(ostream & flux) const
{
    typename MyMap<_T>::const_iterator iter = map<MyString, _T>::begin();
    typename MyMap<_T>::const_iterator end = map<MyString, _T>::end();
    while (iter != end)
    {
        iter->second->printToStream(flux);
        ++iter;
    }
    return flux;
}

/*!
 * \brief print on a stream the content of the MyMap
 * \param flux the ostream to modify.
 * \return the modified ostream
 */
template<class T>
ostream &
MyMap<_T>::toStream(ostream  & flux) const
{
    typename MyMap<_T>::const_iterator iter = map<MyString, _T>::begin();
    typename MyMap<_T>::const_iterator end = map<MyString, _T>::end();

    flux << " " << map<MyString, _T>::size() << endl;
    while (iter != end)
    {
        iter->first.toStream(flux);
        iter->second->toStream(flux);
        ++iter;
    }
    return flux;
}

/*!
 * \brief restore the content of the MyMap from an istream
 * \param flux the istream.
 * \return the modified istream.
 */
template<class T>
istream &
MyMap<_T>::fromStream(istream & flux)
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

/**
 * @brief Overload of the << operator for the MyMap class
 * @param flux The flux to write into.
 * @param myMap The MyMap to stream.
 * @return The modified flux.
 */
template<class T>
ostream & operator<<(ostream & flux, hkl::MyMap<_T> const & myMap)
{
    myMap.printToStream(flux);
    return flux;
}

#ifdef MSVC6
# undef MyMap
# undef _T
#endif

#endif //_MYMAP_H_
