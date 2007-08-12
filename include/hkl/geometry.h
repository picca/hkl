#ifndef _GEOMETRY_H
#define _GEOMETRY_H


#include <vector>

#include "hklobject.h"
#include "source.h"
#include "axe.h"
#include <string>

#include "HKLException.h"
#include "quaternion.h"
#include "svector.h"
#include <iostream>
using namespace std;

namespace hkl { class Holder; } 
namespace hkl { class Axe; } 

namespace hkl {

typedef std::vector<int> _vector_int_;
typedef std::vector<hkl::_vector_int_> _vector_vector_int_;
class Geometry : public hkl::HKLObject {
  protected:
    hkl::Source _source;


  private:
    hkl::AxeList _axes;


  protected:
    std::vector<hkl::Holder *> _holders;


  public:
    /**
     * @brief Create a new Geometry. 
     * @param name The name of the Geometry.
     * @param description The description of the Geometry.
     * @param nb_holder The number of holder of the Geometry.
     */
    Geometry(const std::string & name, const std::string & description, unsigned int nb_holder);

    virtual ~Geometry();

    Geometry(const Geometry & geometry);

    /**
     * @brief Get the Source store in the Diffractometer.
     * @return A constant reference on the Source.
     */
    inline const hkl::Source get_source() const;

    /**
     * @brief Get the Source store in the Diffractometer.
     * @return A reference on the Source.
     */
    inline hkl::Source & get_source();

    inline const hkl::AxeList get_axes() const;

    /**
     * @brief Get the Axe named.
     * @param name the name of the Axe we are looking for.
     * @return An hkl::Axe pointer.
     * @throw hkl::HKLException if the hkl::Axe does not exist.
     */
    Axe * get_axe(const std::string & name) throw(hkl::HKLException);

    /**
     * @brief Get the Axe named.
     * @param name the name of the Axe we are looking for.
     * @return An hkl::Axe pointer.
     * @throw hkl::HKLException if the hkl::Axe does not exist.
     */
    Axe * get_axe(const std::string & name) const throw(hkl::HKLException);

    /**
     * \brief  Add a new Axe into the m_samples vector
     * \param A the Axe
     * \throw HKLException Axe already present in the sample list or the detector list.
     */
    Axe * add_sample_axe(Axe * axe) throw(hkl::HKLException);

    /**
     * @brief Add a new Axe into the _detectors_holders vector
     * @param axe the Axe
     * @param idx the index of the detector_holder.
     * \throw an HKLException if the axe is already in the detector list or in the sample list or if the detector_holder is not present.
     */
    Axe * add_detector_axe(Axe * axe) throw(hkl::HKLException);

    /*!
     * \brief return the Rotatio matrix of the sample
     * \return the quaternion corresponding to the state of the sample.
     */
    hkl::Quaternion getSampleQuaternion() const;

    /*!
     * \brief return the Rotatio matrix of the sample.
     * \return The rotation matrix
     *
     * This method compute the rotation matrix by applying each Axe transformation from the m_samples svector.
     * So we can describe every diffractometer if we put the Axe in the right position into this svector
     */
    hkl::smatrix getSampleRotationMatrix() const;

    /*!
     * \brief return the diffraction vector calculated from the detectors angles
     * \return the Q svector
     */
    hkl::svector getQ() const;

    /*!
     * \brief return the diffraction vector calculated from the detectors angles
     * \return the Q svector
     */
    hkl::svector getKf() const;

    /**
     * @brief compute the distance between two Geometry
     * @param geometry The Geometry to compute the distance from.
     * @return The distance between both Geometry
     */
    double get_distance(const Geometry & geometry) const throw(hkl::HKLException);

    /**
     * @brief Compute hkl for an UB matrix. 
     * @param[out] h return the h parameter.
     * @param[out] k return the k parameter.
     * @param[out] l return the l parameter.
     * @param UB The UB matrix of a crystal.
     */
    void computeHKL(double & h, double & k, double & l, const hkl::smatrix & UB) throw(hkl::HKLException);

    /**
     * @brief Set the geometry from an other one. 
     * @param geometry The Geometry to set from.
     * @param strict true or false if the geometry conversion is strict or not.
     * @throw HKLException dependig of the geometry. 
     * @todo voir comment rendre cette fonction purement virtuelle = 0.
     */
    virtual void setFromGeometry(const Geometry & geometry, bool strict) throw(hkl::HKLException);

    /**
     * @brief print the Geometry into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    ostream & printToStream(ostream & flux) const;

    /**
     * @brief print on a stream the content of the Geometry
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the Geometry from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    istream & fromStream(istream & flux);

};
/**
 * @brief Get the Source store in the Diffractometer.
 * @return A constant reference on the Source.
 */
inline const hkl::Source Geometry::get_source() const 
{
  return _source;
}

/**
 * @brief Get the Source store in the Diffractometer.
 * @return A reference on the Source.
 */
inline hkl::Source & Geometry::get_source() 
{
  // Bouml preserved body begin 00029082
  return _source;
  // Bouml preserved body end 00029082
}

inline const hkl::AxeList Geometry::get_axes() const 
{
  return _axes;
}


} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << pour la class Geometry
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
