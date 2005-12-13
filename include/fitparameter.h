#ifndef _FITPARAMETER_H
#define _FITPARAMETER_H

#include <math.h>
#include <string>
#include <iostream>
#include <cstdlib>

#include "range.h"

using namespace std;

namespace hkl {
  
  /**
   * @brief A class design to describe a fitted parameter
   */
  class FitParameter : public Range
  {
    public:
  
      /**
       * @brief default constructor
       */
      FitParameter(void);
      
      /**
       * @brief Constructor
       * @param name of the %FitParameter.
       * @param value of the %FitParameter.
       * @param min the minimum of the %FitParameter.
       * @param max the maximum of the %FitParameter.
       * @param flagFit is a fit parameter or not
       * @param precision to fullfill for the fit.
       */
      FitParameter(string const & name, double value, double min, double max, bool flagFit, double precision);
  
      /**
       * @brief Copy constructor
       * @param fitParameter #FitParameter to copy from
       */
      FitParameter(FitParameter const & fitParameter);
      
      /**
       * @brief The default destructor
       */
      virtual ~FitParameter(void);
  
    
      /**
       * @brief Get the flag of the #FitParameter
       * @return the flag
       */
      bool get_flagFit(void) const {return m_flagFit;}
  
      /**
       * @brief Get the precision of the #FitParameter.
       * @return The precision.
       */
      double get_precision(void) const {return m_precision;}
  
      /**
       * @brief Get the chi2 after a fit.
       * @return the chi2 of the affinement.
       */
      double get_chi2(void) const {return m_chi2;}
      
      /**
       * @brief Set the flag of the #FitParameter.
       * @param flagFit to set. 
       */
      void set_flagFit(bool flagFit) {m_flagFit = flagFit;}
  
      /*
       * @brief Set the Axe sens of rotation
       * @param i +1 direct rotation or -1 for non-direct rotation
       */
      void set_precision(double precision) {m_precision = precision;}
     
      /*
       * @brief Set the Axe sens of rotation
       * @param i +1 direct rotation or -1 for non-direct rotation
       */
      void set_chi2(double chi2) {m_chi2 = chi2;}
     
      /**
       * @brief Are two #FitParameter equals ?
       * @param fitParameter the #FitParameter to compare with
       * @return The comparison of the two #FitParameter.
       */
      bool operator ==(FitParameter const & fitParameter) const;
     
      /**
       * @brief print the #FitParameter into a flux
       * @param flux The stream to print into.
       * @return The flux modified.
       */
      ostream & printToStream(ostream & flux) const;
  
      /**
       * @brief shuffle the parameter
       */
      void randomize(void);
      
      /**
       * \brief Save the FitParameter into a stream.
       * \param flux the stream to save the FitParameter into.
       * \return The stream with the FitParameter.
       */
      ostream & toStream(ostream & flux) const;
    
      /**
       * \brief Restore a FitParameter from a stream.
       * \param flux The stream containing the FitParameter.
       */
      istream & fromStream(istream & flux);
      
    private:
      bool m_flagFit; //< the flag of the #FitParameter.
      double m_precision; //< the precision fo rthe fit.
      double m_chi2; //< the chi2 obtained during the calculation.
    };

} // namespace hkl

/**
 * @brief Overload of the << operator for the Axe class
 * @param flux
 * @param fitParameter
 * @return The flux modified.
 */
ostream & operator<<(ostream & flux, hkl::FitParameter const & fitParameter); 

#endif // _AXE_H
