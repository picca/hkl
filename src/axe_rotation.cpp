
#include "axe_rotation.h"
#include "value.h"

namespace hkl {

namespace axe {

/**
 * @brief constructor
 * @param name The name of the Rotation.
 * @param description The description of the Rotation.
 * @param min The minimum of the Rotation.
 * @param current The current position of the Rotation.
 * @param max The maximum value of the Rotation.
 * @param axe The hkl::svector rotation axe coordinates.
 */
Rotation::Rotation(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max, const hkl::svector & axe) throw(hkl::HKLException) :
  hkl::Axe(name, description , min, current, max)  
{
  // Bouml preserved body begin 00025682
      if (!(axe == svector ()))
	{
	  _axe = axe.normalize ();

	  double angle;
	  double s_angle;
	  // update the read_quaternion
	    angle = _current.get_value () / 2.;
	    s_angle = sin (angle);
	    _quaternion.set (cos (angle), s_angle * _axe.x (),
			     s_angle * _axe.y (), s_angle * _axe.z ());
	}
      else
	  HKLEXCEPTION ("Can not create an Axe with a null axe vector.",
			"Please set a correct axe for this axe.");
  // Bouml preserved body end 00025682
}

Rotation::~Rotation() 
{
  // Bouml preserved body begin 0003AB02
  // Bouml preserved body end 0003AB02
}

Axe * Rotation::clone() const 
{
  // Bouml preserved body begin 0003B002
    return new hkl::axe::Rotation(*this);
  // Bouml preserved body end 0003B002
}

/**
 * @brief Set the read part of the Rotation 
 */
void Rotation::set_current(const hkl::Value & value) 
{
  // Bouml preserved body begin 00039D02
      // update the read_quaternion
      double angle = value.get_value () / 2.;
      double s_angle = sin (angle);
      _quaternion.set (cos (angle), s_angle * _axe.x (), s_angle * _axe.y (),
		       s_angle * _axe.z ());
      // call the Axe::set_current to updates all related PseudoAxes
      Axe::set_current (value);
  // Bouml preserved body end 00039D02
}

/**
 * \brief Are two Rotation equals ?
 * \param rotation the Rotation to compare with.
 * \return The comparison of the two Rotation.
 */
bool Rotation::operator==(const hkl::axe::Rotation & rotation) const 
{
  // Bouml preserved body begin 00025F82
      return Axe::operator== (rotation)
	&& _axe == rotation._axe && _quaternion == rotation._quaternion;
  // Bouml preserved body end 00025F82
}

/**
 * @brief Compute the read distance between two Rotation.
 * @param rotation The hkl::Axe to compute the distance from. 
 * @return The distance between the two Rotation.
 */
double Rotation::get_distance(const hkl::Axe & rotation) const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00039E82
    if (rotation.get_type() == AXE_ROTATION)
      {
        double v1 = fmod (_current.get_value(), 2 * constant::math::pi);
        double v2 = fmod (rotation.get_current().get_value(), 2 * constant::math::pi);

        return acos (cos (v1 - v2));
      }
    else
        HKLEXCEPTION("Cannot compute the distance between 2 different type of Axes", "Check the constitancy of the geometry.");
  // Bouml preserved body end 00039E82
}

/**
 * @brief Applie to a hkl::Quaternion, the Rotation.
 * @return The modified hkl::Quaternion
 */
hkl::Quaternion & Rotation::apply(hkl::Quaternion & q) 
{
  // Bouml preserved body begin 0003BF02
      q *= _quaternion;
      return q;
  // Bouml preserved body end 0003BF02
}

/*!
 * \brief print the Rotation into a flux
 * \param flux The stream to print into.
 */
std::ostream & Rotation::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00026202
      flux << " Rotation : ";
      Axe::printToStream (flux);
      flux << " " << _axe;
      return flux;
  // Bouml preserved body end 00026202
}

/*!
 * \brief Save the Rotation into a stream.
 * \param flux the stream to save the Rotation into.
 * \return The stream with the Rotation.
 */
std::ostream & Rotation::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00026282
      Axe::toStream (flux);
      _axe.toStream (flux);
      _quaternion.toStream (flux);

      return flux;
  // Bouml preserved body end 00026282
}

/*!
 * \brief Restore a Rotation from a stream.
 * \param flux The stream containing the Rotation to restore.
 * @todo call update_observers or not ?
 */
std::istream & Rotation::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00026302
      Axe::fromStream (flux);
      _axe.fromStream (flux);
      _quaternion.fromStream (flux);

      return flux;
  // Bouml preserved body end 00026302
}


} // namespace hkl::axe

} // namespace hkl
