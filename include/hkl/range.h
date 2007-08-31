#ifndef _RANGE_H
#define _RANGE_H


#include "value.h"
#include "HKLException.h"
#include "affinement.h"
#include <ostream>
#include <istream>

namespace hkl {

class Range {
  protected:
    hkl::Value _min;

    hkl::Value _current;

    hkl::Value _consign;

    hkl::Value _max;


  public:
    /**
     * @brief The default constructor.
     */
    Range();

    /**
     * @brief constructor of the Range class.
     * 
     * @param min The minimum value of the Range.
     * @param current The current value of the Range.
     * @param consign The consign value of the Range.
     * @param max The maximum value of the Range.
     * @throw HKLException if not min < current, consign < max; 
     */
    Range(const hkl::Value & min, const hkl::Value & current, const hkl::Value & consign, const hkl::Value & max) throw(hkl::HKLException);

    Range(const Range & source);

    /**
     * @brief Get the _min Value of the Range class.
     * @return The minimum Value.
     */
    inline hkl::Value const & get_min() const;

    /**
     * @brief Get the _current Value of the Range class.
     * @return The current Value.
     */
    inline const hkl::Value & get_current() const;

    inline hkl::Value const & get_consign() const;

    /**
     * @brief Get the _max Value of the Range class.
     * @return The maximum Value.
     */
    inline hkl::Value const & get_max() const;

    /**
     * @brief Set the _current hkl::Value of the Range class.
     * @param current The hkl::Value to set.
     * @throw An HKLException if the current hkl::Value in not between min and max.
     */
    void set_current(const hkl::Value & current) throw(hkl::HKLException);

    /**
     * @brief Set the _current double of the Range class.
     * @param current The double to set.
     * 
     * This method do not check for the validity of the Range. This method
     * is requiered by the simplex affinement.
     */
    void set_current(double current);

    /**
     * @brief Set the consign hkl::Value of the Range class.
     * @param consign The hkl::Value to set.
     * @throw An HKLException if the consign hkl::Value in not in between min and max.
     */
    void set_consign(const hkl::Value & consign) throw(hkl::HKLException);

    /**
     * @brief Set the minimum and the maximum of the Range class.
     * @param min The minimum hkl::Value to set.
     * @param max The maximum hkl::Value to set.
     * @throw HKLException if the new Range is not valid.
     * @todo maybe split in set_min and set_max
     * 
     * this method check that the new minimun is not bigger than the current or the consign
     * value of the Range and than the maximum is not lower than the current or consign.
     */
    void set_range(const hkl::Value & min, const hkl::Value & max) throw(hkl::HKLException);

    /**
     * @brief Set the minimum and the maximum of the Range class.
     * @param min The minimum double to set.
     * @param current The current double to set.
     * @param consign The consign double to set.
     * @param max The maximum double to set.
     * @throw HKLException if the new Range is not valid.
     * 
     * this method do not check that the new minimun is not bigger than the current
     * value of the range and greater than the maximum.
     */
    void set(double min, double current, double consign, double max);

    /**
     * @brief Set a Range from another one.
     * @param range The Range to set.
     * 
     * this method set only the _min, _current, _consign and _max Value of the Range.
     */
    void set(const Range & range);

    /**
     * @brief Add a Range to another one.
     * @param range The Range to add.
     * @return A Range ref on the Range after the addition.
     * 
     * This method modify min, current and max to reflect the addition.
     */
    Range & operator+=(const Range & range);

    /**
     * @brief Add a Range to another one.
     * @param value The Range to add.
     * @return A Range ref on the Range after the addition.
     * 
     * This method modify min, current and max to reflect the addition.
     */
    Range & operator+=(const double & value) throw(hkl::Affinement);

    /**
     * @brief Multiply a Range by another one.
     * @param range The Range to multiply by.
     * @return A Range ref on the Range after the multiplication.
     * 
     * This method modify min, current and max to reflect the multiplication.
     */
    Range & operator*=(const Range & range);

    /**
     * @brief Multiply a Range by a double value.
     * @param d The double value.
     * @return The Range after the multiplication.
     * 
     * This method modify min, current and max to reflect the multiplication.
     */
    Range & operator*=(double d);

    /**
     * @brief Divide a Range by a double value.
     * @param d The double value.
     * @return The Range divided.
     * 
     * This method modify min, current, consign and max.
     */
    Range & operator/=(const double & d);

    /**
     * @brief check if the Range contain zero.
     * @return true if zero is include in between min, max.
     */
    
    bool contain_zero() const;

    /**
     * @brief compute the cos of the range.
     * @return The cosinus of the Range
     */
    Range & cos();

    /**
     * @brief compute the acos of the range.
     * @return The invert cosinus of the Range
     */
    Range & acos();

    /**
     * @brief compute the sinus of the range.
     * @return the sinus of the Range
     */
    Range & sin();

    /**
     * @brief compute the invert sinus of the range.
     * @return the invert sinus of the Range
     */
    Range & asin();

    /**
     * @brief compute the tangente of the range.
     * @todo test
     */
    Range & tan();

    /**
     * @brief compute the invert tangente of the range.
     * @todo test
     */
    Range & atan();

    /*!
     * \brief Are two Range equals ?
     * \param range the Range to compare with.
     */
    
    bool operator==(const Range & range) const;

    /*!
     * \brief print the Range into a flux
     * \param flux The stream to print into.
     */
    std::ostream & printToStream(std::ostream & flux) const;

    /*!
     * \brief Save the Range into a stream.
     * \param flux the stream to save the Range into.
     * \return The stream with the Range.
     */
    std::ostream & toStream(std::ostream & flux) const;

    /*!
     * \brief Restore a Range from a stream.
     * \param flux The stream containing the Range to restore.
     * @todo call update_observers or not ?
     */
    std::istream & fromStream(std::istream & flux);

};
/**
 * @brief Get the _min Value of the Range class.
 * @return The minimum Value.
 */
inline hkl::Value const & Range::get_min() const 
{
  return _min;
}

/**
 * @brief Get the _current Value of the Range class.
 * @return The current Value.
 */
inline const hkl::Value & Range::get_current() const 
{
  return _current;
}

inline hkl::Value const & Range::get_consign() const 
{
  return _consign;
}

/**
 * @brief Get the _max Value of the Range class.
 * @return The maximum Value.
 */
inline hkl::Value const & Range::get_max() const 
{
  return _max;
}


} // namespace hkl

/*!
 * \brief Overload of the << operator for the Range clas
 * \param flux The ostream to modify.
 * \param range The range to print.
 * 
 * \return the modified ostream 
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::Range const & range)
{
  return range.printToStream(flux);
}

inline hkl::Range cos(hkl::Range const & range)
{
  hkl::Range res(range);
  return res.cos();
}

inline hkl::Range acos(hkl::Range const & range)
{
  hkl::Range res(range);
  return res.acos();
}

inline hkl::Range sin(hkl::Range const & range)
{
  hkl::Range res(range);
  return res.sin();
}

inline hkl::Range asin(hkl::Range const & range)
{
  hkl::Range res(range);
  return res.asin();
}

inline hkl::Range tan(hkl::Range const & range)
{
  hkl::Range res(range);
  return res.tan();
}

inline hkl::Range atan(hkl::Range const & range)
{
  hkl::Range res(range);
  return res.atan();
}
#endif
