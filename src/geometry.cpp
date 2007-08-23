
#include "geometry.h"

namespace hkl {

/**
 * @brief Create a new Geometry. 
 * @param name The name of the Geometry.
 * @param description The description of the Geometry.
 */
Geometry::Geometry(const std::string & name, const std::string & description) :
  HKLObject(name, description)
{
  // Bouml preserved body begin 00028E02
  // Bouml preserved body end 00028E02
}

Geometry::~Geometry() 
{
  // Bouml preserved body begin 00034102
  // Bouml preserved body end 00034102
}

Geometry::Geometry(const hkl::Geometry & geometry) :
  HKLObject(geometry),
  _source(geometry._source),
  _holders(geometry._holders)
{
  // Bouml preserved body begin 00028F02
  // Bouml preserved body end 00028F02
}

/**
 * @brief Get the Axe named.
 * @param name the name of the Axe we are looking for.
 * @return An hkl::Axe pointer.
 * @throw hkl::HKLException if the hkl::Axe does not exist.
 */
Axe * Geometry::get_axe(const std::string & name) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00029282
  return _holders.axes()[name];
  // Bouml preserved body end 00029282
}

/**
 * @brief Get the Axe named.
 * @param name the name of the Axe we are looking for.
 * @return An hkl::Axe pointer.
 * @throw hkl::HKLException if the hkl::Axe does not exist.
 */
Axe * Geometry::get_axe(const std::string & name) const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003A702
  return _holders.axes()[name];
  // Bouml preserved body end 0003A702
}

/*!
 * \brief return the Rotatio matrix of the sample
 * \return the quaternion corresponding to the state of the sample.
 */
hkl::Quaternion Geometry::getSampleQuaternion() const 
{
  // Bouml preserved body begin 00029502
    Quaternion q;
    _holders[0]->apply(q);

    return q;
  // Bouml preserved body end 00029502
}

/*!
 * \brief return the Rotatio matrix of the sample.
 * \return The rotation matrix
 *
 * This method compute the rotation matrix by applying each Axe transformation from the m_samples svector.
 * So we can describe every diffractometer if we put the Axe in the right position into this svector
 */
hkl::smatrix Geometry::getSampleRotationMatrix() const 
{
  // Bouml preserved body begin 00029582
      return getSampleQuaternion().asMatrix();
  // Bouml preserved body end 00029582
}

/*!
 * \brief return the diffraction vector calculated from the detectors angles
 * \return the Q svector
 */
hkl::svector Geometry::getQ() const 
{
  // Bouml preserved body begin 00029602
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
  // Bouml preserved body end 00029602
}

/*!
 * \brief return the diffraction vector calculated from the detectors angles
 * \return the Q svector
 */
hkl::svector Geometry::getKf() const 
{
  // Bouml preserved body begin 00029682
      // Attention pour l'instant qf est obtenu a partir de qi
      // il faudrait prendre 1, 0, 0 comme référence.
      Quaternion qr;
      Quaternion const & qi = _source.get_qi();
     
      _holders[1]->apply(qr);
      
      Quaternion q(qr);
      q *= qi;
      q *= (qr.conjugate());
      
      return svector(q.b(), q.c(), q.d());
  // Bouml preserved body end 00029682
}

/**
 * @brief compute the distance between two Geometry
 * @param geometry The hkl::Geometry to compute the distance from.
 * @return The distance between both Geometry
 */
double Geometry::get_distance(const hkl::Geometry & geometry) const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00029782
  return _holders.axes().get_distance(geometry._holders.axes());
  // Bouml preserved body end 00029782
}

/**
 * @brief Compute hkl for an UB matrix. 
 * @param[out] h return the h parameter.
 * @param[out] k return the k parameter.
 * @param[out] l return the l parameter.
 * @param UB The UB matrix of a crystal.
 */
void Geometry::computeHKL(double & h, double & k, double & l, const hkl::smatrix & UB) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00029802
      smatrix R = getSampleRotationMatrix() * UB;
      
      double det;
      
      det  =  R.get(0,0)*(R.get(1,1)*R.get(2,2)-R.get(2,1)*R.get(1,2));
      det += -R.get(0,1)*(R.get(1,0)*R.get(2,2)-R.get(2,0)*R.get(1,2));
      det +=  R.get(0,2)*(R.get(1,0)*R.get(2,1)-R.get(2,0)*R.get(1,1));
      
      if (fabs(det) < constant::math::epsilon)
        HKLEXCEPTION("det(R) is null",
                     "La matrice rotation de la machine n'est pas valide");
      else
        {
      
          svector q = getQ();
      
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
  // Bouml preserved body end 00029802
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
  // Bouml preserved body begin 00029882
  // Bouml preserved body end 00029882
}

/**
 * @brief print the Geometry into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & Geometry::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00029982
  /*
      int nb_axes = _sample.size();
      
      flux.precision(3);
      flux << "  Source: " << _source.get_waveLength()
      << ", " << _source.get_direction() << endl;
      //samples
      flux << "  Samples: (" << nb_axes << ")" << endl;
      AxeList::const_iterator it = _sample.begin();
      AxeList::const_iterator end = _sample.end();
      while(it != end)
        {
          Axe const & axe = **it;
          flux.width(12);
          flux << axe.get_name();
          flux << ": " << axe.get_axe();
          flux << "(" << showpos << axe.get_direction() << ")";
          flux.unsetf(ios_base::showpos);
          flux << "  " << axe.get_current().get_value()*constant::math::radToDeg;
          flux << endl;
          ++it;
        }
      
      //detector
      nb_axes = _detector.size();
      flux << "  Detectors: (" << nb_axes << ")" << endl;
      it = _detector.begin();
      end = _detector.end();
      while(it != end)
        {
          Axe const & axe = **it;
          flux.width(12);
          flux << axe.get_name();
          flux << ": " << axe.get_axe();
          flux << "(" << showpos << axe.get_direction() << ")";
          flux.unsetf(ios_base::showpos);
          flux << "  " << axe.get_current().get_value()*constant::math::radToDeg;
          flux << endl;
          ++it;
        }
      
      return flux;
  */
  return flux;
  // Bouml preserved body end 00029982
}

/**
 * @brief print on a stream the content of the Geometry
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Geometry::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00029A02
      HKLObject::toStream(flux);
      _source.toStream(flux);
      _holders.toStream(flux);
      return flux;
  // Bouml preserved body end 00029A02
}

/**
 * @brief restore the content of the Geometry from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Geometry::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00029A82
      HKLObject::fromStream(flux);
      _source.fromStream(flux);
      _holders.fromStream(flux);
      return flux;
  // Bouml preserved body end 00029A82
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
