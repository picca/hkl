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
#ifndef _DIFFRACTOMETER_TEST_H_
#define _DIIFRACTOMETER_TEST_H_

#include <cppunit/extensions/HelperMacros.h>
#include <iostream>
#include <vector>
#include "diffractometer.h"
#include "constants.h"

using std::vector;

class diffractometerTest : public CppUnit::TestFixture
  {
    CPPUNIT_TEST_SUITE( diffractometerTest );

    CPPUNIT_TEST( GetSetAxe );
//  CPPUNIT_TEST( CrystalPart );
    CPPUNIT_TEST( GetSetLattice );
//  CPPUNIT_TEST( GetReciprocalLattice );
    CPPUNIT_TEST( AddReflection );
//  CPPUNIT_TEST( DelReflection );
//  CPPUNIT_TEST( GetReflection );
    CPPUNIT_TEST( ModePart );
//  CPPUNIT_TEST( ComputeU );
//  CPPUNIT_TEST( ComputeHKL );
//  CPPUNIT_TEST( ComputeAngles );
//  CPPUNIT_TEST( LPS );

    CPPUNIT_TEST_SUITE_END();

    double m_epsilon;
    double m_degToRad;
    Source m_source;
    AngleConfiguration m_aC;

  public:

    void setUp();
    void tearDown();

    void GetSetAxe();
//  void CrystalPart();
    void GetSetLattice();
//  void GetReciprocalLattice();
    void AddReflection();
//  void DelReflection();
//  void GetReflection();
    void ModePart();
//  void ComputeU();
//  void ComputeHKL();
//  void ComputeAngles();
//  void LPS();
  };

#endif //_DIFFRACTOMETER_TEST_H_
