#ifndef _OBJECT_H
#define _OBJECT_H

#include <iostream>

#include "mystring.h"
#include "HKLException.h"

using namespace std;

namespace hkl
{

/**
 * @brief the base object contain a name and a description.
 * It is use to name the Mode, the Pseudoaxes etc...
 */
class BaseObject
{
public:

    /**
     * @brief Another constructor
     * @param name The name of the Object
     * @param description The description of the Object
     * @throw HKLException if the name or the description is empty.
     */
    BaseObject(MyString const & name, MyString const & description) throw (HKLException);

    /**
     * @brief print the Object into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    ostream & printToStream(ostream & flux) const;

    /**
     * \brief Are two Object equals ?
     * \param object the Object to compare with.
     * \return true if both are equals flase otherwise.
     */
    bool operator ==(BaseObject const & object) const;

    /**
     * \brief Save the Object into a stream.
     * \param flux the stream to save the Object into.
     * \return The modified stream.
     */
    ostream & toStream(ostream & flux) const;

    /**
     * \brief Restore a Object from a stream.
     * \param flux The stream containing the Object.
     * \return The modified flux.
     */
    istream & fromStream(istream & flux);

protected:

    MyString _name; //!< The name of the BaseObject.
    MyString _description; //!< the description of the BaseObject.

    /**
     * @brief Get the Name of the Object.
     * @return a MyString with the name of the Object.
     */
    MyString const & _get_name(void) const
    {
        return _name;
    }

    /**
     * @brief Get the description of the BaseObject.
     * @return a MyString with the description of the BaseObject.
     */
    MyString const & _get_description(void) const
    {
        return _description;
    }

    /**
     * @brief Set the Name of the Object.
     * @return a MyString with the name of the BaseObject.
     */
    void _set_name(MyString const & name) throw (HKLException);

    /**
     * @brief Set the description of the BaseObject.
     * @return a MyString with the description of the BaseObject.
     */
    void _set_description(MyString const & description) throw (HKLException);
};

class ObjectReadOnly : public BaseObject

{
public :
    /*!
     * \brief Another constructor
     * \param name The name of the Object
     * \param description The description of the Object
     * \throw HKLException if the name or the description is empty.
     */
    ObjectReadOnly(MyString const & name, MyString const & description) throw (HKLException);

    /*!
     * \brief Get the Name of the Object.
     * \return a MyString with the name of the Object.
     */
    MyString const & get_name(void) const
    {
        return _get_name();
    }

    /*!
     * \brief Get the description of the Object.
     * \return a MyString with the description of the Object.
     */
    MyString const & get_description(void) const
    {
        return _get_description();
    }

};

/*!
 * \brief This class Implémente the ObjectInterface
 * it provide a name and a description for the Object.
 */
class Object : public ObjectReadOnly
{
public:

    /*!
     * \brief the default constructor
     */
    Object(void);

    /*!
     * \brief Another constructor
     * \param name The name of the Object
     * \throw HKLException if the name is empty.
     */
    Object(MyString const & name) throw (HKLException);

    /*!
     * \brief Another constructor
     * \param name The name of the Object
     * \param description The description of the Object
     * \throw HKLException if the name or the description is empty.
     */
    Object(MyString const & name, MyString const & description) throw (HKLException);

    /*!
     * \brief set the name of the Object.
     * \param name The name of the Object to set.
     * \throw HKLException if the name is empty.
     */
    void
    set_name(MyString const & name) throw (HKLException)
    {
        _set_name(name);
    }

    /*!
     * \brief set the description of the Object.
     * \param description The description of the Object to set.
     * \throw HKLException if the descriotion is empty.
     */
    void
    set_description(MyString const & description) throw (HKLException)
    {
        _set_description(description);
    }

};

} // namespace hkl

inline ostream &
operator << (ostream& flux, hkl::ObjectReadOnly const & object)
{
    return object.printToStream(flux);
};

inline ostream &
operator << (ostream& flux, hkl::Object const & object)
{
    return object.printToStream(flux);
};

#endif // _OBJECT_H
