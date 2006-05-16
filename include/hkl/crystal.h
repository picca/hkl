#ifndef _CRYSTAL_H_
#define _CRYSTAL_H_

#include "config.h"

#include <iostream>
#include <vector>
#include <map>

#include "mymap.h"
#include "svecmat.h"
#include "mystring.h"
#include "reflection.h"
#include "fitparameter.h"
#include "fitparameterlist.h"
#include "HKLException.h"

#define DEFAULT_CRYSTAL_NAME "Crystal"

using namespace std;

namespace hkl {

  /**
   * @brief Class which store the crystals parameters
   *
   * Class crystal to store direct and reciprocal lattice 
   * parameters and the matrix to move from the reciprocal
   * lattice to the cristal cartesian system.
   * References :
   *
   * William R. Busing and Henri A. Levy "Angle calculation 
   * for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
   * <A HREF="http://journals.iucr.org/index.html"> Acta
   * Cryst.</A>, <B>22</B>, 457-464.
   *
   * A.J.C. Wilson "X-Ray Optics, The Diffraction of X-Rays
   * By Finite and Imperfect Crystals"
   * (1962) John Wiley & Sons Inc., 14-17.
   */
  class Crystal : public FitParameterList, public Object
  {
  
    public:
  
      /**
       * @brief default constructor
       */
      Crystal(void);
      
      /**
       * @brief default constructor
       * @param name The name of the #Crystal
       */
      Crystal(MyString const & name);
      
      /**
       * @brief Copy constructor
       * @param crystal The #Crystal we want to copy.
       *
       * This constructor creates a new #Crystal from the crystal #Crystal.
       */
      Crystal(Crystal const & crystal);
  
      smatrix const & get_B(void) const {return m_B;} //!< get the m_B %smatrix
      smatrix const & get_U(void) const {return m_U;} //!< get the m_U %smatrix
      ReflectionList const & get_reflectionList(void) const {return m_reflectionList;} //!< get the reflectionList
      
      void set_B(smatrix const & m) {m_B = m;} //!< set the B matrix;
      void set_U(smatrix const & m) {m_U = m;} //!< set the U matrix;
      void set_reflectionList(ReflectionList const & reflectionList) {m_reflectionList = reflectionList;} //!< set the reflectionList
     
      /**
       * @brief Return All the Crystal Lattice parameters
       * @param[out] a
       * @param[out] b
       * @param[out] c
       * @param[out] alpha
       * @param[out] beta
       * @param[out] gamma
       */
      void getLattice(double * a, double * b, double * c,
                      double * alpha, double * beta, double * gamma) const;
      
      void setLattice(double const & a, double const & b, double const & c,
                      double const & alpha, double const & beta, double const & gamma); //!< set the crystal parameters
      
      /**
       * @brief Compute the reciprocal lattice of the Crystal
       * @param[out] a_star
       * @param[out] b_star
       * @param[out] c_star
       * @param[out] alpha_star
       * @param[out] beta_star
       * @param[out] gamma_star
       */
      void getReciprocalLattice(double * a_star, double * b_star, double * c_star,
                                double * alpha_star, double * beta_star, double * gamma_star) const;
  
      /**
       * @brief Add a reflection to the reflection liste
       * @param reflection the %Reflection to add.
       * @return the index of the added %Reflection.
       */
      unsigned int addReflection(Reflection const & reflection) throw (HKLException);
      
      /**
       * @brief Delete the index reflection from the reflection list
       * @param index the index of the reflection to delete.
       */
      void delReflection(unsigned int const & index) throw (HKLException);
      
      /**
       * @brief Modification of the ith reflections
       * @param index of the ith reflection to modify
       * @param reflection replace the ith reflection with that one.
       */
      void setReflection(unsigned int const & index, Reflection const & reflection) throw (HKLException);
      
      /**
       * @brief Get a constant reference on a reflection.
       * @param index of the reflection.
       * @return A constant reference on the #m_reflectionList.
       */
      Reflection & getReflection(unsigned int const & index) throw (HKLException);
      
      /**
       * @brief Get a constant reference on a reflection.
       * @param index of the reflection.
       * @return A constant reference on the #m_reflectionList.
       */
      Reflection const & getReflection(unsigned int const & index) const throw (HKLException);
      
      /**
       * @brief Return true or false if the crystal contain at least nb_reflections independant reflections.
       * @param nb_reflections the minimim number of independant reflections.
       * @return true if crystal contain at least nb_reflections independant reflections. false otherwise.
       *
       * We comptabilize Colinear reflections as one unique reflection available for computation.
       * (ex (1,0,0) == (2,0,0)).
       */
      bool isEnoughReflections(unsigned int nb_reflections) const;
  
      /**
       * @brief Compute the orientation matrix from two basic non-parallel reflections.
       *
       * Compute the orientation matrix from two basic non-parallel reflections.
       */
      void computeU(void) throw (HKLException);
  
      /**
       * @brief Compute the leastSquare of the crystal.
       * @return the variance.
       */
      double fitness(void) throw (HKLException);
      
      /**
       * @brief Randomize the crystal
       */
      void randomize(void);
  
      /**
       * @brief overload of the == operator for the cristal class
       * @param C The crystal we want to compare.
       */
      bool operator == (Crystal const & C) const;
     
      /**
       * @brief Print the state of the current crystal on a ostream.
       * @param flux the ostream to write into.
       * @return the flux modified.
       */
      ostream & printToStream(ostream & flux) const;
      
      /**
       * \brief Save the Crystal into a stream.
       * \param flux the stream to save the Crystal into.
       * \return The stream with the Crystal.
       */
      ostream & toStream(ostream & flux) const;
    
      /**
       * \brief Restore a Crystal from a stream.
       * \param flux The stream containing the Crystal.
       */
      istream & fromStream(istream & flux);
      
    protected:
      smatrix m_B; //!< The crystal matrix.
      smatrix m_U; //!< The Orientation matrix
      ReflectionList m_reflectionList; //!< the reflection list associated with this crystal
  
      /**
       * @brief The main function to compute the matrix B.
       *
       * This method computes the B matrix from the cristal parameters.
       * It is the transformation matrix from the reciprocal base to an orthonormal
       * one.
       * @f[
       *  B = \left(
       *    \begin{matrix}
       *      b_1 & b_2\cos\beta_3  & b_3\cos\beta_2 \\
       *      0   & b_2\sin\beta_3  & -b_3\sin\beta_2\cos\alpha_1 \\
       *      0   & 0               & 1/a_3
       *    \end{matrix}
       *  \right)
       * @f]
       */
      void _computeB(void);
      
      /*!
       * \brief The main function to compute the U matrix.
       *
       * This method compute U using the 3 eulerian angles.
       * in fact euler_x, euler_y and euler_z parameters.
       */
      void _computeU(void);
      
      /**
       * @brief Return the index of the next usable reflection for calculation
       * @param from The iterator of the reflection from which the search start.
       * @return The iterator of the next reflection valid for calculus.
       */
      ReflectionList::iterator & _getNextReflectionIteratorForCalculation(ReflectionList::iterator & from) throw (HKLException);
      
  };

} // namespace hkl

  /**
   * @brief Surcharge de l'operateur << pour la class cristal
   * @param flux 
   * @param C 
   * @return 
   */
ostream & operator << (ostream & flux, hkl::Crystal const & C);

#endif // _CRYSTAL_H_
