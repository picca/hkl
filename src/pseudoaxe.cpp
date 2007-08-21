
#include "pseudoaxe.h"
#include "range.h"
#include "pseudoaxeengine.h"
#include "parameterlist.h"
#include "axe.h"
#include "value.h"

namespace hkl {

/**
 * @brief The default constructor.
 * @param name The name of the PseudoAxeTemp.
 * @param description The description of the PseudoAxeTemp.
 * @param read The read part of the PseudoAxe.
 * @param write The write part of the PseudoAxe.
 * @param engine The engine use to compute the pseudoAxes value.
 * @todo be sure to be consistant with ModeTemp.
 */
PseudoAxe::PseudoAxe(const std::string & name, const std::string & description, const Range & read, Range & write, hkl::PseudoAxeEngine * engine) :
  ObjectReadOnly(name, description),
  _read(read),
  _write(write),
  _engine(engine),
  _parameters(engine->parameters())
{
  // Bouml preserved body begin 0002FB02
  // Bouml preserved body end 0002FB02
}

AxeList & PseudoAxe::relatedAxes() 
{
  // Bouml preserved body begin 00038182
      return _engine->relatedAxes();
  // Bouml preserved body end 00038182
}

/**
 * @brief Initialize the pseudoAxe.
 * This method must be call before using a pseudoAxe.
 */
void PseudoAxe::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002FB82
      _engine->initialize();
  // Bouml preserved body end 0002FB82
}

/**
 * @brief uninitialize the PseudoAxe.
 * Uninitialize a PseudoAxe if you do not whant to use it.
 */
void PseudoAxe::uninitialize() 
{
  // Bouml preserved body begin 0002FC02
      _engine->uninitialize();
  // Bouml preserved body end 0002FC02
}

/**
 * @brief Get the initialized state of the PseudoAxe.
 * @return A bool fill with the initialized state of the PseudoAxe.
 */
bool PseudoAxe::is_initialized() const 
{
  // Bouml preserved body begin 0002FC82
      return _engine->is_initialized();
  // Bouml preserved body end 0002FC82
}

/**
 * @brief Get the readabled state of the PseudoAxe.
 * @return A bool fill with the readable state of the PseudoAxe.
 */
bool PseudoAxe::is_readable() const 
{
  // Bouml preserved body begin 0002FD02
      return _engine->is_readable();
  // Bouml preserved body end 0002FD02
}

/**
 * @brief Get the writable state of the PseudoAxe.
 * @return A bool fill with the writable state of the PseudoAxe.
 */
bool PseudoAxe::is_writable() const 
{
  // Bouml preserved body begin 0002FD82
      return _engine->is_writable();
  // Bouml preserved body end 0002FD82
}

/**
 * @brief Get the min Value of the PseudoAxe.
 * @return A Value fill with the minimum value of the PseudoAxe.
 * @throw HKLException if the PseudoAxe is not readable.
 */
const hkl::Value & PseudoAxe::get_min() const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002FE02
      if (_engine->is_readable())
        return _read.get_min();
      else
        {
          ostringstream reason;
          reason << "The pseudoAxe named : " << get_name() << " is not valid";
          HKLEXCEPTION(reason.str(), "initialize it");
        }
  // Bouml preserved body end 0002FE02
}

/**
 * @brief Get the current Value of the PseudoAxe.
 * @return A Value fill with the current value of the PseudoAxe.
 * @throw HKLException if the PseudoAxe is not readable.
 */
const hkl::Value & PseudoAxe::get_current() const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002FE82
      if (_engine->is_readable())
          return _read.get_current();
      else
        {
          ostringstream reason;
          reason << "The pseudoAxe named : " << get_name() << " is not valid";
          HKLEXCEPTION(reason.str(), "initialize it");
        }
  // Bouml preserved body end 0002FE82
}

/**
 * @brief Get the current Value of the PseudoAxe.
 * @return A Value fill with the current value of the PseudoAxe.
 * @throw HKLException if the PseudoAxe is not readable.
 */
void PseudoAxe::get_read_write(hkl::Value & read, hkl::Value & write) const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00038802
      if(_engine->is_readable())
        {
          read.set_value(_read.get_current().get_value());
          write.set_value(_write.get_current().get_value());
        }
      else
        {
          ostringstream reason;
          reason << "The pseudoAxe named : " << get_name() << " is not valid";
          HKLEXCEPTION(reason.str(), "initialize it");
        }
  // Bouml preserved body end 00038802
}

/**
 * @brief Get the maximum Value of the PseudoAxe.
 * @return A Value fill with the maximum Value of the PseudoAxe.
 * @throw HKLException if the PseudoAxe is not readable.
 */
const hkl::Value & PseudoAxe::get_max() const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002FF02
      if (_engine->is_readable())
        return _read.get_max();
      else
        {
          ostringstream reason;
          reason << "The pseudoAxe named : " << get_name() << " is not valid";
          HKLEXCEPTION(reason.str(), "initialize it");
        }
  // Bouml preserved body end 0002FF02
}

/**
 * @brief Set the current value of the PseudoAxe.
 * @param value The Value to set. 
 * @throw HKLException If the PseudoAxe is not writable. 
 * 
 * This method set the write part of the pseudoAxe and compute
 * the corresponding geometry using the engine. 
 */
void PseudoAxe::set_current(const hkl::Value & value) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002FF82
      // get the _read min and max then check if writting the value is OK.
      _write.set_current(value.get_value());
      _engine->set();
  // Bouml preserved body end 0002FF82
}

/**
 * @brief Set the engine use by the PseudoAxe.
 * @param engine The engine to set.
 *
 * This method is only use by the DerivedPseudoAxeEngine to modify
 * the engine part of the PseudoAxe.
 */
void PseudoAxe::set_engine(hkl::PseudoAxeEngine * engine) 
{
  // Bouml preserved body begin 00030002
      _engine = engine;
  // Bouml preserved body end 00030002
}

void PseudoAxe::set_write_from_read() 
{
  // Bouml preserved body begin 00038202
      _engine->set_write_from_read();
  // Bouml preserved body end 00038202
}

hkl::ParameterList & PseudoAxe::parameters() 
{
  // Bouml preserved body begin 00030082
      return _parameters;
  // Bouml preserved body end 00030082
}

/**
 * \brief Are two PseudoAxe equals ?
 * \param pseudoAxe the hkl::PseudoAxe to compare with.
 * \return true if both are equals flase otherwise.
 */
bool PseudoAxe::operator==(const hkl::PseudoAxe & pseudoAxe) const 
{
  // Bouml preserved body begin 00030102
      return ObjectReadOnly::operator==(pseudoAxe)
             && _engine == pseudoAxe._engine;
  // Bouml preserved body end 00030102
}

/**
 * @brief print the PseudoAxe into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & PseudoAxe::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00030182
      ObjectReadOnly::printToStream(flux);
      _read.printToStream(flux);
      _write.printToStream(flux);
      return flux;
  // Bouml preserved body end 00030182
}


} // namespace hkl
