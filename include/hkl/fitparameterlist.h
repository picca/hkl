#ifndef _FITPARAMETERLIST_H_
#define _FITPARAMETERLIST_H_

#include <math.h>

#include "fitparameter.h"

using namespace std;

namespace hkl
  {

  /**
   * @brief A class design to describe a FitParameterList for the simplex methode
   */

  class FitParameterList
    {

    public:
      /**
       * @brief the default destructor.
       */
      virtual ~FitParameterList(void);

      /**
       * @brief compare two FitParameterList.
       * @return true if both are equals.
       */
      bool operator==(FitParameterList const & fitParameterList) const;

      /**
       * @brief Print to a stream the FitParameterList.
       * @param flux the stream to print into.
       * @return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Store a FitParameterList into a stream.
       * @param flux the stream to store into.
       * @return The modified stream.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * @brief Restore a FitParameterList from a stream.
       * @param flux the stream to restore from.
       * @return The modified stream.
       */
      istream & fromStream(istream & flux);

      /**
       * @brief Get the size of the FitParameterList.
       * @return The number of Parameter in the FitParameterList.
       */
      unsigned int size(void) const;

      /**
       * @brief Get on reference on the Parameter named.
       * @param name the name of the parameterin the FitParameterList.
       * @return The named Parameter.
       * @throw HKLException if the Parameter is not present in the FitParameterList.
       */
      FitParameter & operator[](MyString const & name) throw (HKLException);

      /**
       * @brief Get the number of Parameter to fit in the FitParameterList.
       * @return The number of parameter with the fitFlag set to true.
       */
      unsigned int size_to_fit(void) const;

      /**
       * @brief Get an interator on the first element of the FitParameterList.
       * @return The begin iterator of the FitParameterList.
       */
      vector<FitParameter *>::iterator begin(void)
      {
        return _parameters.begin();
      }

      /**
       * @brief Get an interator on the end of the FitParameterList.
       * @return The end iterator of the FitParameterList.
       */
      vector<FitParameter *>::iterator end(void)
      {
        return _parameters.end();
      }

      /**
       * @brief Get a const_interator on the first element of the FitParameterList.
       * @return The begin const_iterator of the FitParameterList.
       */
      vector<FitParameter *>::const_iterator begin(void) const
        {
          return _parameters.begin();
        }

      /**
       * @brief Get a const_interator on the end of the FitParameterList.
       * @return The end const_iterator of the FitParameterList.
       */
      vector<FitParameter *>::const_iterator end(void) const
        {
          return _parameters.end();
        }

      /**
       * @brief check if their is enought data to compute an affinement.
       * @return true if computation is possible.
       */
      virtual bool ready_to_fit(void) const throw (HKLException) = 0;

      /**
       * @brief Randomize all the fitParameter of the FitParameterList.
       */
      virtual void randomize(void) = 0;

      /**
       * @brief Calculation of the fitness.
       * @return the fitness calculated from the fitParameters.
       * @throw HKLException if their is not enought data to perform the fitness calculus.
       */
      virtual double fitness(void) throw (HKLException) = 0;

      /**
       * @brief Calculation of the fitness.
       * @param fitness A double use to store the fitness calculation.
       * @return True if the calculation if valid, false otherwise.
       *
       * this method is use in the simplex Affinement method, and do not throw Exception if the compuationid not valid.
       */
      virtual bool fitness(double & fitness) = 0;

      /**
       * @brief update the fitparameterList.
       *
       * The fitparameters can be changed from the fitParameterList but some
       * other members can depend of these Parameter. So after an update you
       * can be sure that the object is completly coherant.
       */
      virtual void update(void) = 0;

    protected:
      vector<FitParameter *> _parameters; //!< The vector containing pointer on FitParameter.

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
