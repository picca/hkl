
#include "geometry.h"

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
  }

  Geometry::~Geometry()
  {
  }

  Geometry::Geometry(const hkl::Geometry & geometry) :
      HKLObject(geometry),
      _source(geometry._source),
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
  hkl::Quaternion Geometry::get_sample_quaternion() const
    {
      Quaternion q;
      _holders[0]->apply(q);

      return q;
    }

  /*!
   * \brief return the Rotatio matrix of the sample
   * \return the quaternion corresponding to the state of the sample.
   */
  hkl::Quaternion Geometry::get_sample_quaternion_consign() const
    {
      Quaternion q;
      _holders[0]->apply_consign(q);

      return q;
    }

  /*!
   * \brief return the Rotatio matrix of the sample.
   * \return The rotation matrix
   *
   * This method compute the rotation matrix by applying each Axe transformation from the m_samples svector.
   * So we can describe every diffractometer if we put the Axe in the right position into this svector
   */
  hkl::smatrix Geometry::get_sample_rotation_matrix() const
    {
      return this->get_sample_quaternion().asMatrix();
    }

  /*!
   * \brief return the Rotatio matrix of the sample.
   * \return The rotation matrix
   *
   * This method compute the rotation matrix by applying each Axe transformation from the m_samples svector.
   * So we can describe every diffractometer if we put the Axe in the right position into this svector
   */
  hkl::smatrix Geometry::get_sample_rotation_matrix_consign() const
    {
      return this->get_sample_quaternion_consign().asMatrix();
    }

  /*!
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q svector
   */
  hkl::svector Geometry::get_Q() const
    {
      // Attention pour l'instant qf est obtenu a partir de qi
      // il faudrait prendre 1, 0, 0 comme référence.
      Quaternion qr;
      Quaternion const & qi = _source.get_qi();

      _holders[1]->apply(qr);

      Quaternion q(qr);
      q *= qi;
      q *= qr.conjugate();
      q -= qi;

      return svector(q.b(), q.c(), q.d());
    }

  /*!
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q svector
   */
  hkl::svector Geometry::get_Q_consign() const
    {
      // Attention pour l'instant qf est obtenu a partir de qi
      // il faudrait prendre 1, 0, 0 comme référence.
      Quaternion qr;
      Quaternion const & qi = _source.get_qi();

      _holders[1]->apply_consign(qr);

      Quaternion q(qr);
      q *= qi;
      q *= qr.conjugate();
      q -= qi;

      return svector(q.b(), q.c(), q.d());
    }

  /*!
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q svector
   */
  hkl::svector Geometry::get_kf() const
    {
      // Attention pour l'instant qf est obtenu a partir de qi
      // il faudrait prendre 1, 0, 0 comme référence.
      Quaternion qr;
      Quaternion const & qi = _source.get_qi();

      _holders[1]->apply(qr);

      Quaternion q(qr);
      q *= qi;
      q *= (qr.conjugate());

      return svector(q.b(), q.c(), q.d());
    }

  /*!
   * \brief return the diffraction vector calculated from the detectors angles
   * \return the Q svector
   */
  hkl::svector Geometry::get_kf_consign() const
    {
      // Attention pour l'instant qf est obtenu a partir de qi
      // il faudrait prendre 1, 0, 0 comme référence.
      Quaternion qr;
      Quaternion const & qi = _source.get_qi();

      _holders[1]->apply_consign(qr);

      Quaternion q(qr);
      q *= qi;
      q *= (qr.conjugate());

      return svector(q.b(), q.c(), q.d());
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
  void Geometry::compute_HKL(double & h, double & k, double & l, const hkl::smatrix & UB) throw(hkl::HKLException)
  {
    smatrix R = this->get_sample_rotation_matrix() * UB;

    double det;

    det  =  R.get(0,0)*(R.get(1,1)*R.get(2,2)-R.get(2,1)*R.get(1,2));
    det += -R.get(0,1)*(R.get(1,0)*R.get(2,2)-R.get(2,0)*R.get(1,2));
    det +=  R.get(0,2)*(R.get(1,0)*R.get(2,1)-R.get(2,0)*R.get(1,1));

    if (fabs(det) < constant::math::epsilon)
      HKLEXCEPTION("det(R) is null",
                   "La matrice rotation de la machine n'est pas valide");
    else
      {

        svector q = this->get_Q();

        double sum;

        sum =   q.x() * (R.get(1,1)*R.get(2,2)-R.get(1,2)*R.get(2,1));
        sum += -q.y() * (R.get(0,1)*R.get(2,2)-R.get(0,2)*R.get(2,1));
        sum +=  q.z() * (R.get(0,1)*R.get(1,2)-R.get(0,2)*R.get(1,1));
        h = sum / det;

        sum =  -q.x() * (R.get(1,0)*R.get(2,2)-R.get(1,2)*R.get(2,0));
        sum +=  q.y() * (R.get(0,0)*R.get(2,2)-R.get(0,2)*R.get(2,0));
        sum += -q.z() * (R.get(0,0)*R.get(1,2)-R.get(0,2)*R.get(1,0));
        k = sum / det;

        sum =   q.x() * (R.get(1,0)*R.get(2,1)-R.get(1,1)*R.get(2,0));
        sum += -q.y() * (R.get(0,0)*R.get(2,1)-R.get(0,1)*R.get(2,0));
        sum +=  q.z() * (R.get(0,0)*R.get(1,1)-R.get(0,1)*R.get(1,0));
        l = sum / det;
      }
  }

  /**
   * @brief Compute hkl for an UB matrix.
   * @param[out] h return the h parameter.
   * @param[out] k return the k parameter.
   * @param[out] l return the l parameter.
   * @param UB The UB matrix of a crystal.
   */
  void Geometry::compute_HKL_consign(double & h, double & k, double & l, const hkl::smatrix & UB) throw(hkl::HKLException)
  {
    smatrix R = this->get_sample_rotation_matrix_consign() * UB;

    double det;

    det  =  R.get(0,0)*(R.get(1,1)*R.get(2,2)-R.get(2,1)*R.get(1,2));
    det += -R.get(0,1)*(R.get(1,0)*R.get(2,2)-R.get(2,0)*R.get(1,2));
    det +=  R.get(0,2)*(R.get(1,0)*R.get(2,1)-R.get(2,0)*R.get(1,1));

    if (fabs(det) < constant::math::epsilon)
      HKLEXCEPTION("det(R) is null",
                   "La matrice rotation de la machine n'est pas valide");
    else
      {

        svector q = this->get_Q_consign();

        double sum;

        sum =   q.x() * (R.get(1,1)*R.get(2,2)-R.get(1,2)*R.get(2,1));
        sum += -q.y() * (R.get(0,1)*R.get(2,2)-R.get(0,2)*R.get(2,1));
        sum +=  q.z() * (R.get(0,1)*R.get(1,2)-R.get(0,2)*R.get(1,1));
        h = sum / det;

        sum =  -q.x() * (R.get(1,0)*R.get(2,2)-R.get(1,2)*R.get(2,0));
        sum +=  q.y() * (R.get(0,0)*R.get(2,2)-R.get(0,2)*R.get(2,0));
        sum += -q.z() * (R.get(0,0)*R.get(1,2)-R.get(0,2)*R.get(1,0));
        k = sum / det;

        sum =   q.x() * (R.get(1,0)*R.get(2,1)-R.get(1,1)*R.get(2,0));
        sum += -q.y() * (R.get(0,0)*R.get(2,1)-R.get(0,1)*R.get(2,0));
        sum +=  q.z() * (R.get(0,0)*R.get(1,1)-R.get(0,1)*R.get(1,0));
        l = sum / det;
      }
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
             && _source == geometry._source
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
      flux << std::endl << _source;
      flux << std::endl << _holders.axes();
      return flux;
    }

  /**
   * @brief print on a stream the content of the Geometry
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & Geometry::toStream(std::ostream & flux) const
    {
      HKLObject::toStream(flux);
      _source.toStream(flux);
      _holders.toStream(flux);
      return flux;
    }

  /**
   * @brief restore the content of the Geometry from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & Geometry::fromStream(std::istream & flux)
  {
    HKLObject::fromStream(flux);
    _source.fromStream(flux);
    _holders.fromStream(flux);
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
