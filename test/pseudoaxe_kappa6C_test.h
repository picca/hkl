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
#ifndef _PSEUDOAXE_KAPPA6C_TEST_H_
#define _PSEUDOAXE_KAPPA6C_TEST_H_

#include <cppunit/extensions/HelperMacros.h>

#include "kappa6C_pseudoaxeengine.h"

class PseudoAxe_Kappa6C_Test : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( PseudoAxe_Kappa6C_Test );

    CPPUNIT_TEST( Omega );
    CPPUNIT_TEST( Chi );
    CPPUNIT_TEST( Phi );
    CPPUNIT_TEST( Psi );
    CPPUNIT_TEST( Tth );
    CPPUNIT_TEST( Q );
    CPPUNIT_TEST( persistanceIO );

    CPPUNIT_TEST_SUITE_END();

    double _alpha;
    hkl::kappa6C::Geometry * _geometry;
    hkl::eulerian6C::Geometry * _geometry_E6C;
    hkl::kappa4C::vertical::Geometry * _geometry_K4C;
    hkl::eulerian4C::vertical::Geometry * _geometry_E4C;
    hkl::SampleList * _samples;

  public:

    void setUp(void);
    void tearDown(void);

    void Omega(void);
    void Chi(void);
    void Phi(void);
    void Psi(void);
    void Tth(void);
    void Q(void);
    void persistanceIO(void);
  };

#endif //_PSEUDOAXE_KAPPA6C_TEST_H_
