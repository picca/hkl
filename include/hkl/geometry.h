#ifndef _GEOMETRY_H_
#define _GEOMETRY_H_

#include "config.h"

#include <iostream>
#include <sstream>
#include <vector>

#include "axe.h"
#include "source.h"
#include "svecmat.h"
#include "mystring.h"
#include "constants.h"
#include "quaternion.h"
#include "HKLException.h"
#include "objectwithparameters.h"

using namespace std;

namespace hkl {

    /*!
     * \brief Store the current geometry according to the type of diffractometer.
     * 
     * This class will be derived depending of the geometry of the diffractometers.
     * this class is an "abstraite" class.
     * 
     * \todo Remplacer axeMap par un unsorted_map (GCC 4.0) et rajouter un champ type d'axe pour accelerer les calcules.
     */
    class Geometry : public ObjectWithParameters
    { 
    public:

      Geometry(void); //!< The default constructor.

      Geometry(Geometry const & geometry); //!< The copy constructor.

      virtual ~Geometry(void); //!< The destructor.

      /*!
       * \brief Assignation of the Geometry.
       * \param geometry The Geometry to assign.
       */
      Geometry & operator=(Geometry const & geometry);

      /*!
       * \brief Are two Geometry equals.
       * \param geometry The Geometry to be compare.
       */
      bool operator==(Geometry const & geometry) const;

      Source const & get_source(void) const {return m_source;} //!< Get the Source

      Source & get_source(void) {return m_source;} //!< Get the Source

      vector<Axe *> const & get_samples(void) const {return m_samples;} //!< Get the samples names.

      vector<Axe *> const & get_detectors(void) const {return m_detectors;} //!< Get the detectors names.

      /*!
       * \brief Get the Axe named.
       * \param name the name of the Axe we are looking for.
       * \throw HKLException if the axe do not exist.
       * \return a reference on the axe with the right name.
       */
      Axe & get_axe(MyString const & name) throw (HKLException);

      /*!
       * \brief Get the Axe named.
       * \param name the name of the axe we are looking for.
       * \throw HKLException if the Axe do not exist.
       * \return A reference on the axe with the right name.
       */
      Axe const & get_axe(MyString const & name) const throw (HKLException);

      /*!
       * \brief Return a vector of MyString with the name of all axes.
       * \return A list of all axes
       */
      vector<MyString> const getAxesNames(void) const;

      /*!
       * \brief  Add a new Axe into the m_samples vector
       * \param A the Axe
       * \throw HKLException Axe already present in the sample list or the detector list.
       */
      void addSampleAxe(Axe & A) throw (HKLException);

      /*!
       * \brief  Add a new Axe into the m_detectors vector
       * \param A the Axe
       * \throw HKLException Axe exist already in the detector list or in the sample list.
       */
      void addDetectorAxe(Axe & A) throw (HKLException);

      /*!
       * \brief return the Rotatio matrix of the sample
       * \return the quaternion corresponding to the state of the sample.
       */
      Quaternion getSampleQuaternion(void) const;

      /*!
       * \brief return the Rotatio matrix of the sample.
       * \return The rotation matrix
       *
       * This method compute the rotation matrix by applying each Axe transformation from the m_samples svector.
       * So we can describe every diffractometer if we put the Axe in the right position into this svector
       */
      smatrix getSampleRotationMatrix(void) const;

      /*!
       * \brief return the diffraction vector calculated from the detectors angles
       * \return the Q svector
       */
      svector getQ(void) const;

      /*!
       * \brief return the diffraction vector calculated from the detectors angles
       * \return the Q svector
       */
      svector getKf(void) const;

      /*!
       * \brief return the diffraction vector calculated from the detectors angles
       * \return the HKLphi svector
       */
      svector getHKLphi(void) const;

      /*!
       * \brief compute the distance between two Geometry
       *\param geometry The Geometry to compute the distance from.
       *\return The distance between both Geometry 
       */
      double getDistance(Geometry const & geometry) throw (HKLException);

      /** 
       * @brief Compute hkl for an UB matrix.
       * 
       * @param[out] h return the h parameter.
       * @param[out] k return the k parameter.
       * @param[out] l return the l parameter.
       * @param UB The UB matrix of a crystal.
       */
      void computeHKL(double & h, double & k, double & l, smatrix const & UB) throw (HKLException);

      /** 
       * @brief Set the geometry from an other one.
       * 
       * @param geometry The Geometry to set from.
       * @param strict true or false if the geometry conversion is strict or not.
       * @throw HKLException dependig of the geometry. 
       * @todo voir comment rendre cette fonction purement virtuelle = 0.
       */
      virtual void setFromGeometry(Geometry const & geometry, bool const & strict) throw (HKLException);

      /*!
       * \brief put the angleConfiguration into a stream
       * \param flux
       */
      ostream & printToStream(ostream & flux) const;

      /*!
       * \brief Save the Geometry into a stream.
       * \param flux the stream to save the Geometry into.
       * \return The stream with the Geometry.
       */
      virtual ostream & toStream(ostream & flux) const;

      /*!
       * \brief Restore an Geometry from a stream.
       * \param flux The stream containing the Geometry.
       */
      virtual istream & fromStream(istream & flux);

    protected:
      Source m_source; //!< the source use with the Geometry.
      AxeMap m_axes; //!< A map of all the axes.
      AxeVector m_samples; //!< The sample vector.
      AxeVector m_detectors; //!< the detector vector.

    };

} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << pour la class angleconfiguration
 * \param flux 
 * \param geometry
 *
 * This function use the printToStream virtual function to print on screen
 * or in an ostream. Because the operator<< can not be declare as virtual
 * we need to use this hake to virtualize not the operator<< but the function
 * called by it printToStream
 */
ostream & operator<<(ostream & flux, hkl::Geometry const & geometry);

#endif
