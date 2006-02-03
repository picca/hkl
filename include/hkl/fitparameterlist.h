#ifndef _FITPARAMETERLIST_H_
#define _FITPARAMETERLIST_H_

#include <math.h>
#include <iostream>

#include "myvector.h"
#include "fitparameter.h"
#include "HKLException.h"

using namespace std;

namespace hkl {

/**
 * @brief A class design to describe a FitParameterList for the simplex methode
 */
class FitParameterList : public MyVector<FitParameter>
{
  public:
    /*!
     * @brief The default constructor
     */
    FitParameterList(void);

    /*!
     * @brief The Copy constructor
     * @param fitParameterList a %FitParameterList to copy from
     */
    FitParameterList(FitParameterList const & fitParameterList);
    
    /*!
     * @brief The default destructor
     */
    virtual ~FitParameterList(void);
    
    /*!
     * @brief Add a %FitParameterList to an other one
     * @param fitParameterList The %FitParameterList to add.
     * @return The modified %FitParameterList.
     */
    FitParameterList & operator +=(FitParameterList const & fitParameterList);
   
    /*!
     * @brief Substract a %FitParameterList to an other one
     * @param fitParameterList The %FitParameterList to substract.
     * @return The modified %FitParameterList.
     */
    FitParameterList & operator -=(FitParameterList const & fitParameterList);
   
    /*!
     * @brief Multiply a %FitParameterList by a number.
     * @param d The number
     * @return The modified %FitParameterList.
     */
    FitParameterList & operator *=(double const & d);
    
    /*!
     * @brief Divide a %FitParameterList by a number.
     * @param d The number
     * @return The modified %FitParameterList.
     */
    FitParameterList & operator /=(double const & d);
      
    /*!
     * @brief get the number of parameter to fit of the %FitParameterList
     * @return the number of parameter
     */
    unsigned int getNumberOfParameterToFit(void) const;

    /*!
     * @brief Randomize all the fitParameter of the FitParameterList.
     */
    virtual void randomize(void);
  
   /*!
    * @brief Calculation of the fitness.
    * @return the fitness calculated from the fitParameters.
    */
    virtual double fitness(void) throw (HKLException) = 0;
};

} // namespace hkl

/*!
 * @brief Overload of the << operator for the %FitParameterList class
 * @param flux
 * @param fitParameterList
 * @return the modified flux.
 */
ostream & operator<<(ostream & flux, hkl::FitParameterList const & fitParameterList); 

#endif // _FITPARAMETERLIST_H_
