// File svecmat.h

#ifndef VECMAT
#define VECMAT

// Classes vector and matrix

#include <standard.h>

class smatrix;

class svector
{
  friend smatrix;

private:
  // These two fields define the array of double precision
  // numbers modelling the vector and its size.
  int m_size;
  double *m_vec;

  int range(int);

public:
  // This constructor creates a 3D vector and populates it.
  svector(const double el1, const double el2, const double el3);

  ~svector()
  {delete m_vec;}

  double& operator[](int i)
  {return m_vec[range(i)];}

  // Unary plus.
  svector& operator+()
  {return *this;}

  // Redefine basic operations assignment, addition and subtraction
  // to another vector, multiplication and division by a double.
  svector& operator=(const svector&);
  svector& operator+=(const svector&);
  svector& operator-=(const svector&);
  svector& operator*=(const double);
  svector& operator/=(const double);

  int getSize() const
  {return m_size;}

  friend svector operator-(const svector&); // Unary minus.
  friend svector operator+(const svector&, const svector&);
  friend svector operator-(const svector&, const svector&);
  friend svector operator*(const svector&, double);
  friend svector operator*(double, const svector&);
  friend svector operator/(const svector&, double);

  // Scalar product, infinite norm and euclidian norm.
  friend double scalar(const svector&, const svector&);
  friend double norminf(const svector&);
  friend double norminf(const smatrix&);
  friend double norm(const svector&);
  friend double norm(const smatrix&);

  // v = M.u, v = u.M, M = u.v, M = M1.M2
  friend svector operator*(const smatrix&, const svector&);
  friend svector operator*(const svector&, const smatrix&);
  friend smatrix operator*(const svector&, const svector&);
  friend smatrix operator*(const smatrix&, const smatrix&);
};



class smatrix
{
private:
  int m_numrows;
  int m_numcols;
  svector **m_mat;
  
  // Row index check.
  int range(int);

public:
  // This constructor creates a 3*3 matrix and populates it.
  smatrix(const double el11, const double el12, const double el13,
          const double el21, const double el22, const double el23,
          const double el31, const double el32, const double el33);

  // Destructor.
  ~smatrix();

  svector& operator[](int i)
  {return *m_mat[range(i)];}

  // Unary plus.
  smatrix operator+()
  {return *this;}

  // Redefine basic operations assignment, addition and
  // subtraction, multiplication and division by a double.
  smatrix& operator=(const matrix&);
  smatrix& operator+=(const matrix&);
  smatrix& operator-=(const matrix&);
  smatrix& operator*=(double);
  smatrix& operator/=(double);

  int getNumRows() const;
  int getNumCols() const;

  smatrix transpose() const;
  void transpose();

  friend smatrix operator-(const smatrix&); // Unary minus.
  friend smatrix operator+(const smatrix&, const smatrix&);
  friend smatrix operator-(const smatrix&, const smatrix&);

  // M = M1.M2, M = (real).M1, M = M1.(real), M = M1/(real),
  // v = M.u, v = u.M, M = u.v
  friend smatrix operator*(const smatrix&, const smatrix&);
  friend smatrix operator*(double, const smatrix&);
  friend smatrix operator*(const smatrix&, double);
  friend smatrix operator/(const smatrix&, double);
  friend smatrix operator*(const smatrix&, const svector&);
  friend smatrix operator*(const svector&, const smatrix&);
  friend smatrix operator*(const svector&, const svector&);

  // Norms.
  friend double norm(const smatrix&);
  friend double norminf(const smatrix&);
};

// Inline functions.
inline int svector::range(int i)
{
  if ((i >= 0) && (i< m_size))
    return i;
  else 
    return (-1);
}

inline int smatrix::range(int i)
{
  if ((i >= 0) && (i < m_NumRows))
    return i;
  else
    return (-1);
}

inline svector operator*(double d, const svector& v)
{
  return d * v;
}

inline smatrix operator*(double d, const smatrix& m)
{
  return d * m;
}

#endif
