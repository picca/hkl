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
#ifndef _HKLEXCEPTION_H
#define _HKLEXCEPTION_H


#include <string>
#include <vector>

#include <sstream>

#define HKLEXCEPTION(a,b)  do {\
    std::ostringstream location;\
    location << __FILE__ << " line : " << __LINE__;\
    throw HKLException((a),(b),location.str());\
} while(0)

namespace hkl
  {

  enum ErrorSeverity
  {
    WARN,
    ERR,
    PANIC
  };
  class Error
    {
    public:
      std::string reason;

      std::string desc;

      std::string origin;

      int severity;

      Error();

      Error(const char * _reason, const char * _desc, const char * _origin, int _severity = ERR);

      Error(const std::string & _reason, const std::string & _desc, const std::string & _origin, int _severity = ERR);

      Error(const Error & error);

      virtual ~Error();

      Error & operator=(const Error & error);

    };
  typedef std::vector<hkl::Error> ErrorList;
  class HKLException
    {
    public:
      hkl::ErrorList errors;

      HKLException();

      HKLException(const char * _reason, const char * _desc, const char * _origin, int _severity = ERR);

      HKLException(const std::string & _reason, const std::string & _desc, const std::string & _origin, int _severity = ERR);

      HKLException(const HKLException & _src);

      ~HKLException();

      HKLException & operator=(const HKLException & _src);

      void push_error(char * _reason, char * _desc, char * _origin, int _severity = ERR);

      void push_error(std::string _reason, std::string _desc, std::string _origin, int _severity = ERR);

      void push_error(const hkl::Error & _error);

    };

} // namespace hkl
#endif
