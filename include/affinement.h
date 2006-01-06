#ifndef _AFFINEMENT_H_
#define _AFFINEMENT_H_

#include "config.h"

#include <string>
#include <iostream>
#include <valarray>

#ifdef VCPP6
  #include "float.h"
  #define isnan _isnan
#endif

#include "crystal.h"
#include "HKLException.h"
#include "fitparameterlist.h"

using namespace std;

namespace hkl {

  /**
   * This class defines how to affine crystal parameters.
   */
  class Affinement : public Object
  {
    public:
      /**
       * @brief the default destructor
       */
      virtual ~Affinement(void);

      /**
       * @brief fit the parameter of an objects 
       * @param fitParameterList %fitParameterList object to fit.
       *
       * this function modify the object.
       */
      virtual void fit(FitParameterList & fitParameterList) = 0;

      /**
       * @brief Get the max number of iteration.
       * @return The maximun number of iteration.
       */
      unsigned int const get_nb_max_iteration(void) const {return m_nb_max_iteration;}

      /**
       * @brief Set the maximun number of iteration
       * @param nb_max_iteration The maximum number of iteration to set.
       */
      void set_nb_max_iteration(unsigned int nb_max_iteration) {m_nb_max_iteration = nb_max_iteration;}

      /**
       * @brief Get the number of iteration effectively computed.
       * @return The number of iteration effectively computed.
       */
      unsigned int const get_nb_iteration(void) const {return m_nb_iteration;}

      /**
       * @brief Set the number of iteration effectively computed.
       * @param nb_iteration The number of iteration effectively computed.
       */
      void set_nb_iteration(unsigned int nb_iteration) {m_nb_iteration = nb_iteration;}

      /**
       * @brief Get the fitness after computation.
       * @return The fitness after computation.
       */
      double const get_fitness(void) const {return m_fitness;}

      /**
       * @brief Set the fitness after computation.
       * @return The fitness after computation.
       */
      void set_fitness(double fitness) {m_fitness = fitness;}

      /**
       * \brief Save the Affinement into a stream.
       * \param flux the stream to save the Affinement into.
       * \return The stream with the Affinement.
       */
      ostream & toStream(ostream & flux) const;
    
      /**
       * \brief Restore a Affinement from a stream.
       * \param flux The stream containing the Affinement.
       */
      istream & fromStream(istream & flux);
      
    private:
      unsigned int m_nb_max_iteration; //!< Max number of iterration
      unsigned int m_nb_iteration; //!< the effectively computed iterations.
      double m_fitness; //!< fitness of the fit

    protected:
      /**
       * @brief the default constructor
       */
      Affinement(std::string name);

      /**
       * @brief the copy contructor
       */
      Affinement(Affinement & affinement);
  };

  namespace affinement {
    
    /**
     * This class defines how to affine via the simplex method.
     */
    class Simplex : public Affinement
    {
      public:

        /**
         * @brief the default constructor
         */
        Simplex(void);

        /**
         * @brief the default destructor
         */
        virtual ~Simplex(void);

        /**
         * @brief fit the data using the simplex method.
         * @param fitParameterList the %FitParameterList to fit.
         *
         * This function modify the vertex.
         */
        void fit(FitParameterList & fitParameterList);

      private:
        /**
         * @brief Update the fitparameters of the %Vertex from a vector of double
         * @param fitParameterList.
         * @param parameterList The parameterList to update.
         */
        void _updateParameterListFromVertex(FitParameterList const & fitParameterList, std::valarray<double> & parameterList);
        
        /**
         * @brief Update the fitparameters of the %Vertex from a vector of double
         * @param fitParameterList the %FitParameterList to update.
         * @param parameterList
         */
        void _updateVertexFromParameterList(FitParameterList & fitParameterList, std::valarray<double> const & parameterList);
    };
  } // namespace affinement

#ifdef VCPP6
  typedef MyStarVector<Affinement*> AffinementList;
#else
  typedef MyVector<Affinement*> AffinementList;
#endif

} // namespace hkl

#endif // _AFFINEMENT_H_
