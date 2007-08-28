
#include "pseudoaxe.h"
#include "pseudoaxeengine.h"
#include "axe.h"
#include "parameterlist.h"

namespace hkl {

/**
 * @brief The default constructor.
 * @param name The name of the PseudoAxeTemp.
 * @param description The description of the PseudoAxeTemp.
 * @param engine The engine use to compute the pseudoAxes value.
 * @todo be sure to be consistant with ModeTemp.
 */
PseudoAxe::PseudoAxe(const std::string & name, const std::string & description, hkl::PseudoAxeEngine * engine) :
  ObjectReadOnly(name, description),
  _engine(engine) 
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
        return _min;
      else
        {
          std::ostringstream reason;
          reason << "\"" << this->get_name() << "\" minimum value unreadable";
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
          return _current;
      else
        {
          std::ostringstream reason;
          reason << "\"" << this->get_name() << "\" current value unreadable";
          HKLEXCEPTION(reason.str(), "initialize it");
        }
  // Bouml preserved body end 0002FE82
}

/**
 * @brief Get the current Value of the PseudoAxe.
 * @return A Value fill with the current write value of the PseudoAxe.
 * @throw HKLException if the PseudoAxe is not readable.
 */
hkl::Value const & PseudoAxe::get_consign() const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00038802
      if(_engine->is_readable())
          return _consign;
      else
        {
          std::ostringstream reason;
          reason << "\"" << this->get_name() << "\" consign value unreadable";
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
        return _max;
    else
      {
        std::ostringstream reason;
          reason << "\"" << this->get_name() << "\" maximum value unreadable";
        HKLEXCEPTION(reason.str(), "initialize it");
      }
  // Bouml preserved body end 0002FF02
}

/**
 * @brief Set the consign value of the PseudoAxe.
 * @param value The Value to set. 
 * @throw HKLException If the PseudoAxe is not writable. 
 * 
 * This method set the write part of the pseudoAxe and compute
 * the corresponding geometry using the engine. 
 */
void PseudoAxe::set_consign(const hkl::Value & value) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002FF82
  // get the _read min and max then check if writting the value is OK.
    if (_engine->is_writable())
      {
        if (value >= _min && value <= _max)
          {
            _consign.set_value(value.get_value());
            _engine->set();
          }
        else
          {
            std::ostringstream reason;
            reason << "\"" << this->get_name() << "\" can not set this consign " << value << " [" << _min << ", " << _max << "]";
            HKLEXCEPTION(reason.str(), "set a correct consign");
          }
      }
    else
      {
        std::ostringstream reason;
        reason << "\"" << this->get_name() << "\" unwritable";
        HKLEXCEPTION(reason.str(), "initialize it");
      }
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

hkl::ParameterList & PseudoAxe::parameters() 
{
  // Bouml preserved body begin 00030082
      return _engine->parameters();
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
std::ostream & PseudoAxe::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00030182
      flux << "\"" << this->get_name() << "\" " << _current << ", " << _consign << " [" << _min << " : " << _max << "] " 
           << " (init : " << _engine->is_initialized() << ", readable : " << _engine->is_readable() << ", writable : " << _engine->is_writable() << ")";
      return flux;
  // Bouml preserved body end 00030182
}


} // namespace hkl
