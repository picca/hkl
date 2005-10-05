#ifndef _RANGE_H
#define _RANGE_H

#include <string>
#include <vector>
#include <iostream>

#include "value.h"
#include "constants.h"

namespace hkl {

/**
 * \brief A class design to describe a Range
 */
class Range : public Value
{
  public:
    
    /**
     * @brief Default constructor
     */
    Range();
    
    /**
     * \brief constructor
     * \param name The name of the #Range
     * \param value the current value of the #Range.
     * \param min the minimum
     * \param max the maximum
     */
    Range(std::string const & name, double value, double min, double max);

    /**
     * \brief Copy constructor
     * \param R A #Range to copy from.
     */
    Range(Range const & R);
    
    /**
     * \brief The default destructor
     */
    virtual ~Range(void);

    /**
     * \brief Get the current minimum of the #Range
     * \return The minimum
     */
    double const & get_min(void) const {return m_min;}
   
    /**
     * \brief Get the current maximum of the #Range
     * \return The maximum
     */
    double const & get_max(void) const {return m_max;}

    /**
     * \brief Set the minimum of the #Value
     */
    void set_min(double min) {m_min = min;}

    /**
     * \brief Set the maximum of the #Value
     */
    void set_max(double max) {m_max = max;}

    /**
     * \brief Are two #Range equals ?
     * \param range the #Range to compare with
     */
    bool operator ==(Range const & range) const;
   
    /**
     * @brief Add a value to another
     * @param value The #Value to add
     * @return The modified value
     */
//    Range & operator +=(Range const & range);

    /**
     * @brief Substract a value to another
     * @param value The #Value to substract
     * @return The modified value
     */
//    Range & operator -=(Range const & range);

    /**
     * @brief Multiply a value by a double
     * @param d The double to multiply by.
     * @return The modified value.
     */
//    Range & operator *=(double const & d);

    /**
     * @brief Divide a value by a double
     * @param d The double to divide by.
     * @return The modified value.
     */
//    Range & operator /=(double const & d);

    /**
     * \brief print the #Range into a flux
     * \param flux The stream to print into.
     */
    std::ostream & printToStream(std::ostream & flux) const;

  private:
    double m_min; //!< the minimum of the range
    double m_max; //!< the maximum of the range
};

typedef std::vector<Range> RangeList;

} // namespace hkl

/**
 * \brief Overload of the << operator for the #Range class
 */
std::ostream & operator<<(std::ostream & flux, hkl::Range const & range); 

#endif // _RANGE_H
