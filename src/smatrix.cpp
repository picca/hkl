#include "svecmat.h"
#include <iostream.h>

// Default constructor.
smatrix::smatrix()
{
  m_mat11 = 1.;
  m_mat12 = 0.;
  m_mat13 = 0.;
  m_mat21 = 0.;
  m_mat22 = 1.;
  m_mat23 = 0.;
  m_mat31 = 0.;
  m_mat32 = 0.;
  m_mat33 = 1.;
}

// Constructor to allocate a 3D matrix and populate it with data.
smatrix::smatrix(double el11, double el12, double el13,
                  double el21, double el22, double el23,
                  double el31, double el32, double el33)
{
  m_mat11 = el11;
  m_mat12 = el12;
  m_mat13 = el13;
  m_mat21 = el21;
  m_mat22 = el22;
  m_mat23 = el23;
  m_mat31 = el31;
  m_mat32 = el32;
  m_mat33 = el33;
}

// Copy constructor.
smatrix::smatrix(const smatrix& M)
{
  m_mat11 = M.m_mat11;
  m_mat12 = M.m_mat12;
  m_mat13 = M.m_mat13;
  m_mat21 = M.m_mat21;
  m_mat22 = M.m_mat22;
  m_mat23 = M.m_mat23;
  m_mat31 = M.m_mat31;
  m_mat32 = M.m_mat32;
  m_mat33 = M.m_mat33;
}

smatrix::set(const smatrix& M)
{
  m_mat11 = M.m_mat11;
  m_mat12 = M.m_mat12;
  m_mat13 = M.m_mat13;
  m_mat21 = M.m_mat21;
  m_mat22 = M.m_mat22;
  m_mat23 = M.m_mat23;
  m_mat31 = M.m_mat31;
  m_mat32 = M.m_mat32;
  m_mat33 = M.m_mat33;
}

// Set all the fields.
void smatrix::set(double el11, double el12, double el13,
                  double el21, double el22, double el23,
                  double el31, double el32, double el33)
{
  m_mat11 = el11;
  m_mat12 = el12;
  m_mat13 = el13;
  m_mat21 = el21;
  m_mat22 = el22;
  m_mat23 = el23;
  m_mat31 = el31;
  m_mat32 = el32;
  m_mat33 = el33;
}

// Transposition.
void smatrix::transpose()
{
  smatrix M(*this);

  m_mat11 = M.m_mat11;
  m_mat12 = M.m_mat21;
  m_mat13 = M.m_mat31;
  m_mat21 = M.m_mat12;
  m_mat22 = M.m_mat22;
  m_mat23 = M.m_mat32;
  m_mat31 = M.m_mat13;
  m_mat32 = M.m_mat23;
  m_mat33 = M.m_mat33;
}

void smatrix::multiplyOnTheRight(const smatrix& M2)
{
  smatrix M1(*this);

  m_mat11 = M1.m_mat11 * M2.m_mat11 + M1.m_mat12 * M2.m_mat21 + M1.m_mat13 * M2.m_mat31;
  m_mat12 = M1.m_mat11 * M2.m_mat12 + M1.m_mat12 * M2.m_mat22 + M1.m_mat13 * M2.m_mat32;
  m_mat13 = M1.m_mat11 * M2.m_mat13 + M1.m_mat12 * M2.m_mat23 + M1.m_mat13 * M2.m_mat33;
  m_mat21 = M1.m_mat21 * M2.m_mat11 + M1.m_mat22 * M2.m_mat21 + M1.m_mat23 * M2.m_mat31;
  m_mat22 = M1.m_mat21 * M2.m_mat12 + M1.m_mat22 * M2.m_mat22 + M1.m_mat23 * M2.m_mat32;
  m_mat23 = M1.m_mat21 * M2.m_mat13 + M1.m_mat22 * M2.m_mat23 + M1.m_mat23 * M2.m_mat33;
  m_mat31 = M1.m_mat31 * M2.m_mat11 + M1.m_mat32 * M2.m_mat21 + M1.m_mat33 * M2.m_mat31;
  m_mat32 = M1.m_mat31 * M2.m_mat12 + M1.m_mat32 * M2.m_mat22 + M1.m_mat33 * M2.m_mat32;
  m_mat33 = M1.m_mat31 * M2.m_mat13 + M1.m_mat32 * M2.m_mat23 + M1.m_mat33 * M2.m_mat33;
}

void smatrix::multiplyOnTheLeft(const smatrix& M2)
{
  smatrix M1(*this);

  m_mat11 = M2.m_mat11 * M1.m_mat11 + M2.m_mat12 * M1.m_mat21 + M2.m_mat13 * M1.m_mat31;
  m_mat12 = M2.m_mat11 * M1.m_mat12 + M2.m_mat12 * M1.m_mat22 + M2.m_mat13 * M1.m_mat32;
  m_mat13 = M2.m_mat11 * M1.m_mat13 + M2.m_mat12 * M1.m_mat23 + M2.m_mat13 * M1.m_mat33;
  m_mat21 = M2.m_mat21 * M1.m_mat11 + M2.m_mat22 * M1.m_mat21 + M2.m_mat23 * M1.m_mat31;
  m_mat22 = M2.m_mat21 * M1.m_mat12 + M2.m_mat22 * M1.m_mat22 + M2.m_mat23 * M1.m_mat32;
  m_mat23 = M2.m_mat21 * M1.m_mat13 + M2.m_mat22 * M1.m_mat23 + M2.m_mat23 * M1.m_mat33;
  m_mat31 = M2.m_mat31 * M1.m_mat11 + M2.m_mat32 * M1.m_mat21 + M2.m_mat33 * M1.m_mat31;
  m_mat32 = M2.m_mat31 * M1.m_mat12 + M2.m_mat32 * M1.m_mat22 + M2.m_mat33 * M1.m_mat32;
  m_mat33 = M2.m_mat31 * M1.m_mat13 + M2.m_mat32 * M1.m_mat23 + M2.m_mat33 * M1.m_mat33;
}

double smatrix::get(int i, int j) const
{
  if (i==1 && j==1)
    return m_mat11;
  if (i==2 && j==1)
    return m_mat21;
  if (i==3 && j==1)
    return m_mat31;

  if (i==1 && j==2)
    return m_mat12;
  if (i==2 && j==2)
    return m_mat22;
  if (i==3 && j==2)
    return m_mat32;

  if (i==1 && j==3)
    return m_mat13;
  if (i==2 && j==3)
    return m_mat23;
  if (i==3 && j==3)
    return m_mat33;
}

void smatrix::testMultiplication(smatrix& M2)
{
  smatrix M1(*this);

  M1.transpose();
  M2.transpose();
  M1.multiplyOnTheRight(M2);
  M1.transpose();

  M2.transpose();
  smatrix M3(*this);
  M3.multiplyOnTheLeft(M2);

  cout << endl << "M1" << endl;
  M1.printOnScreen();

  cout << endl << "M3" << endl;
  M3.printOnScreen();
}

void smatrix::printOnScreen() const
{
  cout << endl;
  cout << m_mat11 << '\t' << m_mat12 << '\t' << m_mat13 << endl;
  cout << m_mat21 << '\t' << m_mat22 << '\t' << m_mat23 << endl;
  cout << m_mat31 << '\t' << m_mat32 << '\t' << m_mat33 << endl;
}
