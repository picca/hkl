#ifndef _FITPARAMETERLIST_H
#define _FITPARAMETERLIST_H


#include <vector>
#include <string>
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl
  {
  class FitParameter;
}

namespace hkl
  {

  class FitParameterList
    {
    protected:
      std::vector<hkl::FitParameter *> _parameters;


    public:
      typedef std::vector<FitParameter *>::iterator iterator;

      typedef std::vector<FitParameter *>::const_iterator const_iterator;

      virtual ~FitParameterList();

      /**
       * @brief Get the size of the FitParameterList.
       * @return the number of element in the FitParameterList.
       */
      unsigned int size() const;

      /**
       * @brief Get the number of Parameter to fit in the FitParameterList.
       * @return The number of Parameter with the fitFlag set to true.
       */
      unsigned int size_to_fit() const;

      /**
       * @return the std::string * named
       * @param name The name of the std::string we are looking for in the FitParameterList.
       * @return A std::string pointer.
       * @throw HKLException if the std::string is not present n the FitParameterList.
       */
      hkl::FitParameter * operator[](const std::string & name) throw(hkl::HKLException);

      /**
       * @brief Get an iterator on the first element of the FitParameterList.
       * @return The iterator.
       */
      iterator begin();

      /**
       * @brief Get an iterator on the end of the FitParameterList.
       * @return The iterator.
       */
      iterator end();

      /**
       * @brief Get an const_iterator on the first element of the FitParameterList.
       * @return The const_iterator.
       */
      const_iterator begin() const;

      /**
       * @brief Get an const_iterator on the end of the FitParameterList.
       * @return The const_iterator.
       */
      const_iterator end() const;

      /**
       * @brief check if their is enought data to compute an affinement.
       * @return true if computation is possible.
       */
      virtual bool ready_to_fit() const = 0;

      /**
       * @brief Randomize all the FitParameters in the FitParameterList.
       */
      virtual void randomize() = 0;

      /**
       * @brief Calculation of the fitness.
       * @return the fitness calculated from the fitParameters.
       * @throw HKLException if their is not enought data to perform the fitness calculus.
       */
      virtual double fitness() throw(hkl::HKLException) = 0;

      /**
       * @brief Calculation of the fitness.
       * @param fitness A double use to store the fitness calculation.
       * @return True if the calculation if valid, false otherwise.
       *
       * this method is use in the simplex Affinement method, and do not throw Exception if the compuation is not valid.
       */
      virtual bool fitness(double & fitness) = 0;

      /**
       * @brief update the fitparameterList.
       *
       * The fitparameters can be changed from the fitParameterList but some
       * other members can depend of these Parameter. So after an update you
       * can be sure that the object is completly coherant.
       */
      virtual void update() = 0;

      /*!
       * \brief Are two FitParameterList equals ?
       * \param fitParameterList the FitParameterList to compare with.
       */

      bool operator==(const FitParameterList & fitParameterList) const;

      /*!
       * \brief print the FitParameterList into a flux
       * \param flux The stream to print into.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /*!
       * \brief Save the FitParameterList into a stream.
       * \param flux the stream to save the FitParameterList into.
       * \return The stream with the FitParameterList.
       */
      std::ostream & toStream(std::ostream & flux) const;

      /*!
       * \brief Restore a FitParameterList from a stream.
       * \param flux The stream containing the FitParameterList to restore.
       * @todo call update_observers or not ?
       */
      std::istream & fromStream(std::istream & flux);

    };

} // namespace hkl
/*!
 * @brief Overload of the << operator for the %FitParameterList class
 * @param flux
 * @param fitParameterList
 * @return the modified flux.
 */
std::ostream &
operator<<(std::ostream & flux, hkl::FitParameterList const & fitParameterList);
#endif
