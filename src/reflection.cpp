
#include "reflection.h"

namespace hkl {

/**
 * @brief Create a Reflection.
 * 
 * @param geometry The hkl::Geometry of the reflection
 * @param hkl The hkl scattering vactor.
 * @param flag if the reflection must be use during calculation.
 * @throw HKLException if the geometry is not valid.
 */

Reflection::Reflection(const hkl::Geometry & geometry, const hkl::svector & hkl, bool flag) :
  _geometry(geometry),
  _hkl(hkl),
  _flag(flag) 
{
  // Bouml preserved body begin 0002CD02
  // Bouml preserved body end 0002CD02
}

Reflection::~Reflection() 
{
  // Bouml preserved body begin 00034482
  // Bouml preserved body end 00034482
}

/**
 * @brief Set the hkl scattering vector store in the Reflection.
 * @param hkl The scattering vector in the crystal coordinates to store in the Reflection.
 */

void Reflection::set_hkl(const hkl::svector & value) 
{
  _hkl = value;
}

/**
 * @brief Get a constant reference on the flag store in the Reflection.
 * @return The flag of the Reflection.
 *
 * the flag is true when we use the reflection in the affinement, false otherwise.
 */

const bool & Reflection::flag() const 
{
  // Bouml preserved body begin 0002D002
      return _flag;
  // Bouml preserved body end 0002D002
}

/**
 * @brief Get a constant reference on the flag store in the Reflection.
 * @return The flag of the Reflection.
 *
 * the flag is true when we use the reflection in the affinement, false otherwise.
 */

bool & Reflection::flag() 
{
  // Bouml preserved body begin 0002D082
      return _flag;
  // Bouml preserved body end 0002D082
}

/**
 * @brief compute the theoretical angle beetween two hkl vectors.
 * @param hkl The second scattering vector to compare with the Reflection internal hkl.
 * @return the angle between the two hkl.
 * @todo Maybe move this in the Sample and add a computeAngle(Reflection const & reflection)
 * @todo add the mathematical formula.
 */

hkl::Value Reflection::computeAngle(const hkl::svector & hkl) const 
{
  // Bouml preserved body begin 0002D102
      return Value(_hkl.angle(hkl));
  // Bouml preserved body end 0002D102
}

/**
 * @brief Check if two reflections are colinear.
 * @param reflection The reflection to compare with.
 * @return true if the reflections are colinear, false otherwise.
 * @todo Add the mathematical formula.
 */

bool Reflection::isColinear(const hkl::Reflection & reflection) const 
{
  // Bouml preserved body begin 0002D182
      if ((_hkl.vectorialProduct(reflection._hkl)).norm2() < constant::math::epsilon)
        return true;
      else
        return false;
  // Bouml preserved body end 0002D182
}

/**
 * \brief Are two Reflection equals ?
 * \param reflection the hkl::Reflection to compare with.
 * \return true if both are equals flase otherwise.
 */
bool Reflection::operator==(const hkl::Reflection & reflection) const 
{
  // Bouml preserved body begin 0002D202
      return _geometry == reflection._geometry
             && _hkl == reflection._hkl
             && _flag == reflection._flag
             && _hkl_phi == reflection._hkl_phi;
  // Bouml preserved body end 0002D202
}

/**
 * @brief print the Reflection into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & Reflection::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 0002D282
      flux << _hkl;
      vector<string> axesNames = _geometry.getAxesNames();
      
      unsigned int nb_axes = axesNames.size();
      unsigned int i;
      for(i=0; i<nb_axes; i++)
        {
          flux.width(9);
          flux << _geometry.get_axe(axesNames[i]).get_current().get_value() * hkl::constant::math::radToDeg;
        }
      flux << " |";
      flux.width(9);
      flux << _geometry.get_source().get_waveLength().get_value();
      flux << " | " << "(" << _flag << ") hkl_phi : " << _hkl_phi;
      
      return flux;
  // Bouml preserved body end 0002D282
}

/**
 * @brief print on a stream the content of the Reflection
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & Reflection::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 0002D302
      _geometry.toStream(flux);
      _hkl.toStream(flux);
      _hkl_phi.toStream(flux);
      flux << " " << _flag;
      
      return flux;
  // Bouml preserved body end 0002D302
}

/**
 * @brief restore the content of the Reflection from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & Reflection::fromStream(istream & flux) 
{
  // Bouml preserved body begin 0002D382
      _geometry.fromStream(flux);
      _hkl.fromStream(flux);
      _hkl_phi.fromStream(flux);
      flux >> _flag;
      
      return flux;
  // Bouml preserved body end 0002D382
}


} // namespace hkl
