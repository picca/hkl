#ifndef _PARAMETER_H
#define _PARAMETER_H


#include "object.h"
#include "range.h"
#include <string>
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl { class Value; } 

namespace hkl {

class Parameter : public hkl::ObjectReadOnly, public hkl::Range {
  public:
    /**
     * @brief The default constructor
     * @param name The name std::string of the Parameter.
     * @param description The description std::string of the Parameter.
     * @param min the minimum hkl::Value of the Parameter.
     * @param current The current hkl::Value of the Parameter.
     * @param max The maximum hkl::Value of the Parameter.
     * @throw HKLException if the min <= current <= max is not verify.
     */
    Parameter(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max) throw(hkl::HKLException);

    /*!
     * \brief Are two Parameter equals ?
     * \param parameter the Parameter to compare with.
     */
    
    bool operator==(const Parameter & parameter) const;

    /*!
     * \brief print the Parameter into a flux
     * \param flux The stream to print into.
     */
    std::ostream & printToStream(std::ostream & flux) const;

    /*!
     * \brief Save the Parameter into a stream.
     * \param flux the stream to save the Parameter into.
     * \return The stream with the Parameter.
     */
    std::ostream & toStream(std::ostream & flux) const;

    /*!
     * \brief Restore a Parameter from a stream.
     * \param flux The stream containing the Parameter to restore.
     * @todo call update_observers or not ?
     */
    std::istream & fromStream(std::istream & flux);

};

} // namespace hkl

/*!
 * \brief Overload of the << operator for the Parameter class
 * \param flux The ostream to modify.
 * \param parameter The Parameter to print.
 * \return the modified ostream 
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::Parameter const & parameter)
{
  return parameter.printToStream(flux);
}
#endif
