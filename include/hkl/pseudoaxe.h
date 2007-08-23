#ifndef _PSEUDOAXE_H
#define _PSEUDOAXE_H


#include "object.h"
#include <string>
#include "HKLException.h"
#include <ostream>

namespace hkl { class Range; } 
namespace hkl { class PseudoAxeEngine; } 
namespace hkl { class ParameterList; } 
namespace hkl { class AxeList; } 
namespace hkl { class Value; } 

namespace hkl {

class PseudoAxe : public hkl::ObjectReadOnly {
  protected:
    const hkl::Range & _read;

    hkl::Range & _write;

    hkl::PseudoAxeEngine * _engine;

    hkl::ParameterList & _parameters;


  public:
    /**
     * @brief The default constructor.
     * @param name The name of the PseudoAxeTemp.
     * @param description The description of the PseudoAxeTemp.
     * @param read The read part of the PseudoAxe.
     * @param write The write part of the PseudoAxe.
     * @param engine The engine use to compute the pseudoAxes value.
     * @todo be sure to be consistant with ModeTemp.
     */
    PseudoAxe(const std::string & name, const std::string & description, const Range & read, Range & write, hkl::PseudoAxeEngine * engine);

    AxeList & relatedAxes();

    /**
     * @brief Initialize the pseudoAxe.
     * This method must be call before using a pseudoAxe.
     */
    void initialize() throw(hkl::HKLException);

    /**
     * @brief uninitialize the PseudoAxe.
     * Uninitialize a PseudoAxe if you do not whant to use it.
     */
    void uninitialize();

    /**
     * @brief Get the initialized state of the PseudoAxe.
     * @return A bool fill with the initialized state of the PseudoAxe.
     */
    bool is_initialized() const;

    /**
     * @brief Get the readabled state of the PseudoAxe.
     * @return A bool fill with the readable state of the PseudoAxe.
     */
    bool is_readable() const;

    /**
     * @brief Get the writable state of the PseudoAxe.
     * @return A bool fill with the writable state of the PseudoAxe.
     */
    bool is_writable() const;

    /**
     * @brief Get the min Value of the PseudoAxe.
     * @return A Value fill with the minimum value of the PseudoAxe.
     * @throw HKLException if the PseudoAxe is not readable.
     */
    const hkl::Value & get_min() const throw(hkl::HKLException);

    /**
     * @brief Get the current Value of the PseudoAxe.
     * @return A Value fill with the current value of the PseudoAxe.
     * @throw HKLException if the PseudoAxe is not readable.
     */
    const hkl::Value & get_current() const throw(hkl::HKLException);

    /**
     * @brief Get the current Value of the PseudoAxe.
     * @return A Value fill with the current value of the PseudoAxe.
     * @throw HKLException if the PseudoAxe is not readable.
     */
    void get_read_write(hkl::Value & read, hkl::Value & write) const throw(hkl::HKLException);

    /**
     * @brief Get the maximum Value of the PseudoAxe.
     * @return A Value fill with the maximum Value of the PseudoAxe.
     * @throw HKLException if the PseudoAxe is not readable.
     */
    const hkl::Value & get_max() const throw(hkl::HKLException);

    /**
     * @brief Set the current value of the PseudoAxe.
     * @param value The Value to set. 
     * @throw HKLException If the PseudoAxe is not writable. 
     * 
     * This method set the write part of the pseudoAxe and compute
     * the corresponding geometry using the engine. 
     */
    void set_current(const hkl::Value & value) throw(hkl::HKLException);

    /**
     * @brief Set the engine use by the PseudoAxe.
     * @param engine The engine to set.
     *
     * This method is only use by the DerivedPseudoAxeEngine to modify
     * the engine part of the PseudoAxe.
     */
    void set_engine(hkl::PseudoAxeEngine * engine);

    void set_write_from_read();

    hkl::ParameterList & parameters();

    /**
     * \brief Are two PseudoAxe equals ?
     * \param pseudoAxe the PseudoAxe to compare with.
     * \return true if both are equals flase otherwise.
     */
    bool operator==(const PseudoAxe & pseudoAxe) const;

    /**
     * @brief print the PseudoAxe into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    std::ostream & printToStream(std::ostream & flux) const;

};

} // namespace hkl
/*!
 * \brief Overload of the << operator for the PseudoAxe class
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::PseudoAxe const & pseudoAxe)
{
  return pseudoAxe.printToStream(flux);
}
#endif
