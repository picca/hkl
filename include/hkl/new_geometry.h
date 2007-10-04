#ifndef _NEW_GEOMETRY_H
#define _NEW_GEOMETRY_H

#include "source.h"
#include "new_holder.h"
#include "new_axe.h"
#include "svector.h"
#include "smatrix.h"
#include "quaternion.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
  {
#endif

  struct hkl_axe_config
  {
    struct hkl_interval range;
    double current;
    double consign;
  }

  typedef hkl_axis_config hkl_axe_config[];

  struct hkl_axe
  {
    const char * name;
    struct hkl_svector axe;
    struct hkl_axe_config config;
  }

  typedef hkl_axes hkl_axe[];

  struct hkl_holder
  {
    struct hkl_axes * axes;
    unsigned int nb;
    unsigned int idx[];

    int dirty;
    struct mutable hkl_quaternion _q;
  }

  struct hkl_holders
  {
    unsigned int nb;
    unsigned int alloc;
    hkl_holder * holders[];
  }

  struct hkl_geometry
    {
      hkl_geometry_config config;
      hkl_holders holders;
    }

  struct hkl_geometry_config
    {
      hkl_source source;
      hkl_axis_config * axis_config
    }

  hkl_holder * hkl_geometry_add_new_holder(struct hkl_geometry * geometry)
  {
    hkl_holder * holder = NULL;
    
    holder = malloc(sizeof(hkl_holder));
    if (holder)
    {
      holder.axes = geometry->holders.axes;
      geometry->holders.nb++;

    return holder;
  }

  external int hkl_holder_add_new_rotation_axe(struct hkl_holder * holder, hkl_svector const * axe);

  external hkl_geometry_config * hkl_geometry_get_new_config(struct hkl_geometry const * geom);

  external int hkl_geometry_set_config(struct hkl_geometry * geom, struct hkl_geometry_config const * config);

  external void hkl_geometry_get_Q(struct hkl_geometry const * geometry, struct hkl_svector * Q);

  external void hkl_geometry_get_Q_consign(struct hkl_geometry const * geometry, struct hkl_svector * Q);

  external hkl_geometry_free(struct hkl_geometry * geom);

  static void hkl_geometry_set_twoC_vertical(struct hkl_geometry * geometry)
  {
    double current = 0;
    double consign = 0;
    static source = {HKL_SOURCE_DEFAULT_WAVE_LENGTH, {{1, 0, 0}}}
    static struct hkl_axe axe_omega = {"omega", {{0, -1, 0}}, {{-M_PI, M_PI}, current, consign}};
    static struct hkl_axe axe_tth = {"tth", {{0, -1, 0}}, {{-M_PI, M_PI}, current, consign}};
    struct hkl_holder *holder;

    geometry->source = source;

    holder = hkl_geometry_add_new_holder(geometry);
    hkl_holder_add_new_rotation_axe(holder, axe_omega);
    holder = hkl_geometry_add_new_holder(geometry);
    hkl_holder_add_new_rotation_axe(holder, axe_tth);
  }

  static hkl_geometry * hkl_geometry_create_eulerian4C_vertical(void)
  {
    double current = 0;
    double consign = 0;
    static source = {HKL_SOURCE_DEFAULT_WAVE_LENGTH, {{1, 0, 0}}}
    static struct hkl_axe axe_omega = {"omega", {{0, -1, 0}}, {{-M_PI, M_PI}, current, consign}};
    static struct hkl_axe axe_chi = {"chi", {{1, 0, 0}}, {{-M_PI, M_PI}, current, consign };
    static struct hkl_axe axe_phi = {"phi", {{0, -1, 0}}, {{-M_PI, M_PI}, current, consign };
    static struct hkl_axe axe_tth = {"tth", {{0, -1, 0}}, {{-M_PI, M_PI}, current, consign };
    struct hkl_holder *holder;

    geometry->source = source;

    holder = hkl_geometry_add_new_holder(geometry);
    hkl_holder_add_new_rotation_axe(holder, axe_omega);
    hkl_holder_add_new_rotation_axe(holder, axe_chi);
    hkl_holder_add_new_rotation_axe(holder, axe_phi);
    holder = hkl_geometry_add_new_holder(geometry);
    hkl_holder_add_new_rotation_axe(holder, axe_tth);
  }

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

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif /* _NEW_GEOMETRY_H */
