/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2008 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "pseudoaxe.h"
#include "pseudoaxeengine.h"
#include "axe.h"
#include "parameterlist.h"

namespace hkl
  {

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
  }

  AxeList & PseudoAxe::relatedAxes()
  {
    return _engine->relatedAxes();
  }

  /**
   * @brief Initialize the pseudoAxe.
   * This method must be call before using a pseudoAxe.
   */
  void PseudoAxe::initialize() throw(hkl::HKLException)
  {
    _engine->initialize();
  }

  /**
   * @brief uninitialize the PseudoAxe.
   * Uninitialize a PseudoAxe if you do not whant to use it.
   */
  void PseudoAxe::uninitialize()
  {
    _engine->uninitialize();
  }

  /**
   * @brief Get the initialized state of the PseudoAxe.
   * @return A bool fill with the initialized state of the PseudoAxe.
   */
  bool PseudoAxe::is_initialized() const
    {
      return _engine->is_initialized();
    }

  /**
   * @brief Get the readabled state of the PseudoAxe.
   * @return A bool fill with the readable state of the PseudoAxe.
   */
  bool PseudoAxe::is_readable() const
    {
      return _engine->is_readable();
    }

  /**
   * @brief Get the writable state of the PseudoAxe.
   * @return A bool fill with the writable state of the PseudoAxe.
   */
  bool PseudoAxe::is_writable() const
    {
      return _engine->is_writable();
    }

  /**
   * @brief Get the min Value of the PseudoAxe.
   * @return A Value fill with the minimum value of the PseudoAxe.
   * @throw HKLException if the PseudoAxe is not readable.
   */
  const hkl::Value & PseudoAxe::get_min() const throw(hkl::HKLException)
  {
    if (_engine->is_readable())
      return _min;
    else
      {
        std::ostringstream reason;
        reason << "\"" << this->get_name() << "\" minimum value unreadable";
        HKLEXCEPTION(reason.str(), "initialize it");
      }
  }

  /**
   * @brief Get the current Value of the PseudoAxe.
   * @return A Value fill with the current value of the PseudoAxe.
   * @throw HKLException if the PseudoAxe is not readable.
   */
  const hkl::Value & PseudoAxe::get_current() const throw(hkl::HKLException)
  {
    if (_engine->is_readable())
      return _current;
    else
      {
        std::ostringstream reason;
        reason << "\"" << this->get_name() << "\" current value unreadable";
        HKLEXCEPTION(reason.str(), "initialize it");
      }
  }

  /**
   * @brief Get the current Value of the PseudoAxe.
   * @return A Value fill with the current write value of the PseudoAxe.
   * @throw HKLException if the PseudoAxe is not readable.
   */
  hkl::Value const & PseudoAxe::get_consign() const throw(hkl::HKLException)
  {
    if (_engine->is_readable())
      return _consign;
    else
      {
        std::ostringstream reason;
        reason << "\"" << this->get_name() << "\" consign value unreadable";
        HKLEXCEPTION(reason.str(), "initialize it");
      }
  }

  /**
   * @brief Get the maximum Value of the PseudoAxe.
   * @return A Value fill with the maximum Value of the PseudoAxe.
   * @throw HKLException if the PseudoAxe is not readable.
   */
  const hkl::Value & PseudoAxe::get_max() const throw(hkl::HKLException)
  {
    if (_engine->is_readable())
      return _max;
    else
      {
        std::ostringstream reason;
        reason << "\"" << this->get_name() << "\" maximum value unreadable";
        HKLEXCEPTION(reason.str(), "initialize it");
      }
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
    _engine = engine;
  }

  hkl::ParameterList & PseudoAxe::parameters()
  {
    return _engine->parameters();
  }

  /**
   * \brief Are two PseudoAxe equals ?
   * \param pseudoAxe the hkl::PseudoAxe to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool PseudoAxe::operator==(const hkl::PseudoAxe & pseudoAxe) const
    {
      return ObjectReadOnly::operator==(pseudoAxe)
             && _engine == pseudoAxe._engine;
    }

  /**
   * @brief print the PseudoAxe into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & PseudoAxe::printToStream(std::ostream & flux) const
    {
      flux << "\"" << this->get_name() << "\" " << _current << ", " << _consign << " [" << _min << " : " << _max << "] "
      << " (init : " << _engine->is_initialized() << ", readable : " << _engine->is_readable() << ", writable : " << _engine->is_writable() << ")";
      return flux;
    }


} // namespace hkl
