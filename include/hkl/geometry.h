#ifndef _GEOMETRY_H
#define _GEOMETRY_H

#include <vector>
#include <string>
#include <ostream>
#include <istream>

#include "hklobject.h"
#include "source.h"
#include "holder.h"
#include "axe.h"
#include "HKLException.h"
#include "svector.h"
#include "smatrix.h"
#include "quaternion.h"

namespace hkl { class Axe; }

namespace hkl
  {

  class Geometry : public hkl::HKLObject
    {
    public:
      hkl_source source;

    protected:
      hkl::HolderList _holders;


    public:
      /**
       * @brief Create a new Geometry.
       * @param name The name of the Geometry.
       * @param description The description of the Geometry.
       */
      Geometry(const std::string & name, const std::string & description);

      virtual ~Geometry();

      Geometry(const Geometry & geometry);

      /**
       * @brief Get a constant reference on the axes stores in all holders.
       * @return the AxeList
       */
      inline hkl::AxeList const & get_axes() const;

      /**
       * @brief Get a constant reference on the axes stores in all holders.
       * @return the AxeList
       */
      inline hkl::AxeList & axes();

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

      /*!
       * \brief return the Rotatio matrix of the sample
       * \return the quaternion corresponding to the state of the sample.
       */
      void get_sample_quaternion(hkl_quaternion * q) const;

      /*!
       * \brief return the Rotatio matrix of the sample
       * \return the quaternion corresponding to the state of the sample.
       */
      void get_sample_quaternion_consign(hkl_quaternion * q) const;

      /*!
       * \brief return the Rotatio matrix of the sample.
       * \return The rotation matrix
       *
       * This method compute the rotation matrix by applying each Axe transformation from the m_samples svector.
       * So we can describe every diffractometer if we put the Axe in the right position into this svector
       */
      void get_sample_rotation_matrix(hkl_smatrix * m) const;

      /*!
       * \brief return the Rotatio matrix of the sample.
       * \return The rotation matrix
       *
       * This method compute the rotation matrix by applying each Axe transformation from the m_samples svector.
       * So we can describe every diffractometer if we put the Axe in the right position into this svector
       */
      void get_sample_rotation_matrix_consign(hkl_smatrix * m) const;

      /*!
       * \brief return the diffraction vector calculated from the detectors angles
       * \return the Q svector
       */
      void get_Q(hkl_svector * v) const;

      /*!
       * \brief return the diffraction vector calculated from the detectors angles
       * \return the Q svector
       */
      void get_Q_consign(hkl_svector * v) const;

      /*!
       * \brief return the diffraction vector calculated from the detectors angles
       * \return the Q svector
       */
      void get_kf(hkl_svector * v) const;

      /*!
       * \brief return the diffraction vector calculated from the detectors angles
       * \return the Q svector
       */
      void get_kf_consign(hkl_svector * v) const;

      /**
       * @brief compute the distance between two Geometry
       * @param geometry The Geometry to compute the distance from.
       * @return The distance between both Geometry
       */
      double get_distance(const Geometry & geometry) const throw(hkl::HKLException);

      /**
       * @brief compute the distance between two Geometry
       * @param geometry The Geometry to compute the distance from.
       * @return The distance between both Geometry
       */
      double get_distance_consign(const Geometry & geometry) const throw(hkl::HKLException);

      /**
       * @brief Compute hkl for an UB matrix.
       * @param[out] h return the h parameter.
       * @param[out] k return the k parameter.
       * @param[out] l return the l parameter.
       * @param UB The UB matrix of a crystal.
       */
      void compute_HKL(double & h, double & k, double & l, hkl_smatrix const * UB) throw(hkl::HKLException);

      /**
       * @brief Compute hkl for an UB matrix.
       * @param[out] h return the h parameter.
       * @param[out] k return the k parameter.
       * @param[out] l return the l parameter.
       * @param UB The UB matrix of a crystal.
       */
      void compute_HKL_consign(double & h, double & k, double & l, hkl_smatrix const * UB) throw(hkl::HKLException);

      /**
       * @brief Set the geometry from an other one.
       * @param geometry The Geometry to set from.
       * @param strict true or false if the geometry conversion is strict or not.
       * @throw HKLException dependig of the geometry.
       * @todo voir comment rendre cette fonction purement virtuelle = 0.
       */
      virtual void setFromGeometry(const Geometry & geometry, bool strict) throw(hkl::HKLException);

      /**
       * @brief Are two Geometry equals ?
       * @param geometry the Geometry to compare with.
       * @return true if both are equals flase otherwise.
       */
      bool operator==(const Geometry & geometry) const;

      /**
       * @brief print the Geometry into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

    };

  /**
   * @brief Get a constant reference on the axes stores in all holders.
   * @return the AxeList
   */
  inline hkl::AxeList const & Geometry::get_axes() const
    {
      // Bouml preserved body begin 0003CF82
      return _holders.axes();
      // Bouml preserved body end 0003CF82
    }

  /**
   * @brief Get a constant reference on the axes stores in all holders.
   * @return the AxeList
   */
  inline hkl::AxeList & Geometry::axes()
  {
    // Bouml preserved body begin 0003D002
    return _holders.axes();
    // Bouml preserved body end 0003D002
  }


} // namespace hkl

/**
 * \brief Surcharge de l'operateur << pour la class Geometry
 * \param flux
 * \param geometry
 *
 * This function use the printToStream virtual function to print on screen
 * or in an ostream. Because the operator<< can not be declare as virtual
 * we need to use this hake to virtualize not the operator<< but the function
 * called by it printToStream
 */
std::ostream &
operator<<(std::ostream & flux, hkl::Geometry const & geometry);
#endif
