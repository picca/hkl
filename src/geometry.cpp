
#include "geometry.h"

/**
 * @brief compute the Q vector
 * @param v the vector containing Q
 * @param qi The qi vector of the source
 * @param qr The Rotation quaternion (becarefull this method transpose qr)
 */

inline static void hkl_geometry_compute_Q(hkl_svector * v, hkl_quaternion const * qi, hkl_quaternion * qr)
{
  hkl_quaternion q;

  q = *qr;
  hkl_quaternion_times_quaternion(&q, qi);
  hkl_quaternion_conjugate(qr);
  hkl_quaternion_times_quaternion(&q, qr);
  hkl_quaternion_minus_quaternion(&q, qi);

  // copy the vector part of the quaternion in the vector
  memcpy(v->data, &q.data[1], sizeof(v->data));
}

inline static void hkl_geometry_compute_kf(hkl_svector * v, hkl_quaternion const * qi, hkl_quaternion * qr)
{
  hkl_quaternion q;

  q = *qr;
  hkl_quaternion_times_quaternion(&q, qi);
  hkl_quaternion_conjugate(qr);
  hkl_quaternion_times_quaternion(&q, qr);

  // copy the vector part of the quaternion in the vector
  memcpy(v->data, &q.data[1], sizeof(v->data));
}

namespace hkl
  {

  /**
   * @brief Create a new Geometry.
   * @param name The name of the Geometry.
   * @param description The description of the Geometry.
   */
  Geometry::Geometry(const std::string & name, const std::string & description) :
      HKLObject(name, description)
  {
    // set a default source for all diffractometers
    static hkl_svector direction0 = {{1, 0, 0}};

    source.wave_length = HKL_SOURCE_DEFAULT_WAVE_LENGTH;
    source.direction = direction0;
  }

  Geometry::~Geometry()
  {
  }

  Geometry::Geometry(const hkl::Geometry & geometry) :
      HKLObject(geometry),
      source(geometry.source),
      _holders(geometry._holders)
  {
  }

  /**
   * @brief Get the Axe named.
   * @param name the name of the Axe we are looking for.
   * @return An hkl::Axe pointer.
   * @throw hkl::HKLException if the hkl::Axe does not exist.
   */
  Axe * Geometry::get_axe(const std::string & name) throw(hkl::HKLException)
  {
    return _holders.axes()[name];
  }

  /**
   * @brief Get the Axe named.
   * @param name the name of the Axe we are looking for.
   * @return An hkl::Axe pointer.
   * @throw hkl::HKLException if the hkl::Axe does not exist.
   */
  Axe * Geometry::get_axe(const std::string & name) const throw(hkl::HKLException)
  {
    return _holders.axes()[name];
  }

  /*!
   * \brief return the Rotatio matrix of the sample
   * \return the quaternion corresponding to the state of the sample.
   */
  void Geometry::get_sample_quaternion(hkl_quaternion * q) const
    {
      q->data[0] = 1;
      q->data[1] = q->data[2] = q->data[3] = 0;

      _holders[0]->apply(q);
    }

  /*!
   * \brief return the Rotatio matrix of the sample
   * \return the quaternion corresponding to the state of the sample.
   */
  void Geometry::get_sample_quaternion_consign(hkl_quaternion * q) const
    {
      q->data[0] = 1;
      q->data[1] = q->data[2] = q->data[3] = 0;

      _holders[0]->apply_consign(q);
    }

  /*!
   * \brief return the Rotatio matrix of the sample.
   * \return The rotation matrix
   *
   * This method compute the rotation matrix by applying each Axe transformation from the m_samples svector.
   * So we can describe every diffractometer if we put the Axe in the right position into this svector
   */
  void Geometry::get_sample_rotation_matrix(hkl_smatrix * m) const
    {
      hkl_quaternion q;

      this->get_sample_quaternion(&q);
      ::hkl_quaternion_to_smatrix(&q, m);
    }

  /*!
   * \brief return the Rotatio matrix of the sample.
   * \return The rotation matrix
   *
   * This method compute the rotation matrix by applying each Axe transformation from the m_samples svector.
   * So we can describe every diffractometer if we put the Axe in the right position into this svector
   */
  void Geometry::get_sample_rotation_matrix_consign(hkl_smatrix * m) const
    {
      hkl_quaternion q;

      this->get_sample_quaternion_consign(&q);
      ::hkl_quaternion_to_smatrix(&q, m);
    }

  /*!
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q svector
   */
  void Geometry::get_Q(hkl_svector * Q) const
    {
      hkl_svector ki;
      hkl_quaternion qr = {{1, 0, 0, 0}};

      //get ki
      ::hkl_source_get_ki(&source, &ki);

      // compute the qr quaternion of the detector holder.
      _holders[1]->apply(&qr);

      // compute Q
      *Q = ki;
      ::hkl_svector_rotated_quaternion(Q, &qr);
      ::hkl_svector_minus_svector(Q, &ki);
    }

  /*!
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q svector
   */
  void Geometry::get_Q_consign(hkl_svector * Q) const
    {
      hkl_svector ki;
      hkl_quaternion qr = {{1, 0, 0, 0}};

      //get ki
      ::hkl_source_get_ki(&source, &ki);

      // compute the qr quaternion of the detector holder.
      _holders[1]->apply_consign(&qr);

      // compute Q
      *Q = ki;
      ::hkl_svector_rotated_quaternion(Q, &qr);
      ::hkl_svector_minus_svector(Q, &ki);
    }

  /*!
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q svector
   */
  void Geometry::get_kf(hkl_svector * kf) const
    {
      hkl_quaternion qr = {{1, 0, 0, 0}};

      // compute the qr quaternion of the detector holder.
      _holders[1]->apply(&qr);

      // compute kf
      ::hkl_source_get_ki(&source, kf);
      ::hkl_svector_rotated_quaternion(kf, &qr);
    }

  /*!
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q svector
   */
  void Geometry::get_kf_consign(hkl_svector * kf) const
    {
      hkl_quaternion qr = {{1, 0, 0, 0}};

      // compute the qr quaternion of the detector holder.
      _holders[1]->apply_consign(&qr);

      // compute kf
      ::hkl_source_get_ki(&source, kf);
      ::hkl_svector_rotated_quaternion(kf, &qr);
    }

  /**
   * @brief compute the distance between two Geometry
   * @param geometry The hkl::Geometry to compute the distance from.
   * @return The distance between both Geometry
   */
  double Geometry::get_distance(const hkl::Geometry & geometry) const throw(hkl::HKLException)
  {
    return _holders.axes().get_distance(geometry._holders.axes());
  }

  /**
   * @brief compute the distance between two Geometry
   * @param geometry The hkl::Geometry to compute the distance from.
   * @return The distance between both Geometry
   */
  double Geometry::get_distance_consign(const hkl::Geometry & geometry) const throw(hkl::HKLException)
  {
    return _holders.axes().get_distance_consign(geometry._holders.axes());
  }

  /**
   * @brief Compute hkl for an UB matrix.
   * @param[out] h return the h parameter.
   * @param[out] k return the k parameter.
   * @param[out] l return the l parameter.
   * @param UB The UB matrix of a crystal.
   */
  void Geometry::compute_HKL(double & h, double & k, double & l, hkl_smatrix const * UB) throw(hkl::HKLException)
  {
    hkl_smatrix R;
    hkl_svector hkl;
    hkl_svector q;

    // R * UB
    this->get_sample_rotation_matrix(&R);
    ::hkl_smatrix_times_smatrix(&R, UB);

    this->get_Q(&q);

    if (::hkl_smatrix_solve(&R, &hkl, &q) < 0)
      HKLEXCEPTION("det(R) is null",
                   "La matrice rotation de la machine n'est pas valide");
    h = hkl.data[0];
    k = hkl.data[1];
    l = hkl.data[2];
  }

  /**
   * @brief Compute hkl for an UB matrix.
   * @param[out] h return the h parameter.
   * @param[out] k return the k parameter.
   * @param[out] l return the l parameter.
   * @param UB The UB matrix of a crystal.
   */
  void Geometry::compute_HKL_consign(double & h, double & k, double & l, hkl_smatrix const * UB) throw(hkl::HKLException)
  {
    hkl_smatrix R;
    hkl_svector hkl;
    hkl_svector q;

    // R * UB
    this->get_sample_rotation_matrix_consign(&R);
    ::hkl_smatrix_times_smatrix(&R, UB);

    this->get_Q_consign(&q);

    if (::hkl_smatrix_solve(&R, &hkl, &q) < 0)
      HKLEXCEPTION("det(R) is null",
                   "La matrice rotation de la machine n'est pas valide");
    h = hkl.data[0];
    k = hkl.data[1];
    l = hkl.data[2];
  }

  /**
   * @brief Set the geometry from an other one.
   * @param geometry The Geometry to set from.
   * @param strict true or false if the geometry conversion is strict or not.
   * @throw HKLException dependig of the geometry.
   * @todo voir comment rendre cette fonction purement virtuelle = 0.
   */
  void Geometry::setFromGeometry(const hkl::Geometry & geometry, bool strict) throw(hkl::HKLException)
  {
  }

  /**
   * @brief Are two Geometry equals ?
   * @param geometry the hkl::Geometry to compare with.
   * @return true if both are equals flase otherwise.
   */
  bool Geometry::operator==(const hkl::Geometry & geometry) const
    {
      return HKLObject::operator==(geometry)
             && ::hkl_source_cmp(&source, &geometry.source)
             && _holders == geometry._holders;
    }

  /**
   * @brief print the Geometry into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Geometry::printToStream(std::ostream & flux) const
    {
      HKLObject::printToStream(flux);
      //flux << std::endl << _source;
      flux << std::endl << _holders.axes();
      return flux;
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
std::ostream & operator<<(std::ostream & flux, hkl::Geometry const & geometry)
{
  return geometry.printToStream(flux);
}
