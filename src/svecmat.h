// File svecmat.h

#ifndef VECMAT
#define VECMAT

// Classes vector and matrix in a three dimensionnal space.

class smatrix;

class svector
{
private:
  // The double precision numbers modelling the 3D vector.
  double m_v1;
  double m_v2;
  double m_v3;

public:
  // Default constructor
  svector();

  // This constructor creates a 3D vector and populates it.
  svector(const double el1, const double el2, const double el3);

  // Copy constructor.
  svector(const svector&);

  // Scalar product.
  double scalar(const svector&) const;

  // Vectorial product : Z = this * Y
  void vectorialProduct(const svector& Y, svector& Z) const;

  // Creation of a axis system with unit vectors.
  // M = (vector1, vector2, vector3) where
  // vector1 = this / ||this||
  // vector2 = U / || U ||
  // vector3 = vector1 * vector2
  void axisSystem(const svector U, smatrix& M) const;

  double norm2() const;

  double norminf() const;

  // Compute a colinear unit vector and store its length.
  // unitVector = this / ||this|| = this / length
  void unitVector(svector& _unitVector, double& length) const;

  // Multiplication by a matrix on its right and left.
  // v = v.M
  void multiplyOnTheRight(const smatrix&);
  // v = M.v
  void multiplyOnTheLeft(const smatrix&);

  // Printing.
  void printOnScreen() const;
};



class smatrix
{
  friend svector;

  // The double precision numbers modelling the 3D matrix.
private:
  double m_mat11;
  double m_mat12;
  double m_mat13;
  double m_mat21;
  double m_mat22;
  double m_mat23;
  double m_mat31;
  double m_mat32;
  double m_mat33;

public:
  // Default constructor
  smatrix();

  // This constructor creates a 3*3 matrix and populates it.
  smatrix( double el11, double el12, double el13,
           double el21, double el22, double el23,
           double el31, double el32, double el33);

  // Copy constructor.
  smatrix(const smatrix&);

  // Copy a matrix.
  set(const smatrix&);

  // Give the fields a new value.
  void set( double el11, double el12, double el13,
            double el21, double el22, double el23,
            double el31, double el32, double el33);

  // Transposition.
  void transpose();

  // Multiplication by another matrix and a vector on its right and left.
  // M1 = M1 * M2
  void multiplyOnTheRight(const smatrix& M2);
  // M1 = M2 * M1
  void multiplyOnTheLeft(const smatrix& M2);

  // Print and test.
  void printOnScreen() const;

  void testMultiplication(smatrix& M2);
};


#endif
