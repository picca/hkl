#ifndef _OBJECT_H
#define _OBJECT_H


#include "HKLException.h"
#include "strbuf.h"

namespace hkl
  {

  class ObjectBase
    {
    private:
      std::string _name;

      std::string _description;


    public:
      /**
       * @brief Another constructor
       * @param name The name of the Object
       * @param description The description of the Object
       * @throw HKLException if the name or the description is empty.
       */
      ObjectBase(const std::string & name, const std::string & description) throw(hkl::HKLException);

      /**
       * @brief Copy constructor
       * @param source the ObjectBase to copy.
       */
      ObjectBase(const ObjectBase & source);

      /**
       * \brief Are two Object equals ?
       * \param object the Object to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const ObjectBase & object) const;


    protected:
      /**
       * @brief Get the name of the ObjectBase.
       * @return a string ref with the name of the ObjectBase.
       */
      const std::string & _get_name() const;

      /**
       * @brief Set the name of the BaseObject.
       * @param name The new name of the BaseObject.
       */
      void _set_name(const std::string & name) throw(hkl::HKLException);

      /**
       * @brief Get the description of the ObjectBase.
       * @return a string ref with the name of the ObjectBase.
       */
      const std::string & _get_description() const;

      /**
       * @brief Set the description of the Object.
       * @param description The new BaseObject description.
       */
      void _set_description(const std::string & description) throw(hkl::HKLException);


    public:
      /**
       * @brief print the ObjectBase into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the ObjectBase
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the ObjectBase from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);

    };
  class ObjectReadOnly : public hkl::ObjectBase
    {
    public:
      /**
       * @brief The default constructor.
       * @param name The name of the Object
       * @param description The description of the Object
       * @throw HKLException if the name or the description is empty.
       */
      ObjectReadOnly(const std::string & name, const std::string & description) throw(hkl::HKLException);

      /**
       * @brief Copy constructor
       * @param source the ObjectReadOnly to copy.
       */
      ObjectReadOnly(const ObjectReadOnly & source);

      /**
       * @brief Get the name of the ObjectReadOnly.
       * @return a string reference with the name of the ObjectReadOnly.
       */
      const std::string & get_name() const;

      /**
       * @brief Get the description of the ObjectReadOnly.
       * @return a string reference with the description of the ObjectReadOnly.
       */
      const std::string & get_description() const;

    };
  class Object : public hkl::ObjectReadOnly
    {
    public:
      /**
       * @brief The default constructor.
       */
      Object();

      /**
       * @brief The default constructor.
       * @param name The name of the Object
       * @param description The description of the Object
       * @throw HKLException if the name or the description is empty.
       */
      Object(const std::string & name, const std::string & description) throw(hkl::HKLException);

      /**
       * @brief Copy constructor
       * @param source the Object to copy.
       */
      Object(const Object & source);

      /**
       * @brief Set the name of the Object.
       * @param name
       */
      void set_name(const std::string & name);

      /**
       * @brief Set the name of the Object.
       * @param description
       */
      void set_description(const std::string & description) throw(hkl::HKLException);

    };

} // namespace hkl
/**
 * \brief Surcharge de l'operateur << pour la class BaseObject
 * @param flux
 * @param m
 * @return
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::ObjectBase const & object)
{
  return object.printToStream(flux);
}
#endif
