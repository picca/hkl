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
 * Copyright (C) 2003-2007 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "observer.h"

namespace hkl
  {

  Observer::Observer() :
      _connected(false)
  {
  }

  Observer::~Observer()
  {
  }

  void Observer::connect()
  {
    _connected = true;
  }

  void Observer::unconnect()
  {
    _connected = false;
  }

  /**
   * @brief The default constructor.
   */
  Observable::Observable() :
      _changed(false)
  {
  }

  /**
   * @brief Add an hkl::Observer to the Observable.
   * @param observer The hkl::Observer pointer to add.
   */
  void Observable::add_observer(hkl::Observer * observer)
  {
    _observers.push_back(observer);
  }

  /**
   * @brief Delete an hkl::Observer from the Observable.
   * @param observer The hkl::Observer pointer to remove.
   */
  void Observable::del_observer(hkl::Observer * observer)
  {
    std::vector<Observer *>::iterator iter = _observers.begin();
    std::vector<Observer *>::iterator end = _observers.end();
    while (iter != end)
      {
        if (*iter == observer)
          {
            _observers.erase(iter);
            break;
          }
        ++iter;
      }
  }

  /**
   * @brief Update all the Observer looking for this Observable.
   */
  void Observable::update_observers()
  {
    if (_changed)
      {
        std::vector<Observer *>::iterator iter = _observers.begin();
        std::vector<Observer *>::iterator end = _observers.end();
        while (iter != end)
          {
            (*iter)->update();
            ++iter;
          }
        _changed = false;
      }
  }

  /**
   * @brief Set the changed state of the Observable.
   *
   * If the changed state is not on, no Observer update is possible.
   */
  void Observable::set_changed()
  {
    _changed = true;
  }


} // namespace hkl
