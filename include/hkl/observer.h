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
#ifndef _OBSERVER_H
#define _OBSERVER_H


#include <vector>

namespace hkl
  {

  class Observer
    {
    protected:
      bool _connected;


    public:
      Observer();

      virtual ~Observer();

      virtual void update() = 0;

      void connect();

      void unconnect();

    };
  class Observable
    {
    protected:
      bool _changed;

      std::vector<hkl::Observer *> _observers;


    public:
      /**
       * @brief The default constructor.
       */
      Observable();

      /**
       * @brief Add an hkl::Observer to the Observable.
       * @param observer The hkl::Observer pointer to add.
       */
      void add_observer(hkl::Observer * observer);

      /**
       * @brief Delete an hkl::Observer from the Observable.
       * @param observer The hkl::Observer pointer to remove.
       */
      void del_observer(hkl::Observer * observer);

      /**
       * @brief Update all the Observer looking for this Observable.
       */
      void update_observers();

      /**
       * @brief Set the changed state of the Observable.
       *
       * If the changed state is not on, no Observer update is possible.
       */
      void set_changed();

    };

} // namespace hkl
#endif
