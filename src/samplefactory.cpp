
#include "samplefactory.h"
#include "geometry.h"

#include "sample_monocrystal.h"
namespace hkl {

/**
 * @brief The default constructor.
 * @param geometry the Geometry use to fill the Reflection._geometry.
 */

SampleFactory::SampleFactory(hkl::Geometry & geometry) :
  _geometry(geometry) 
{
  // Bouml preserved body begin 0002EE02
  // Bouml preserved body end 0002EE02
}

vector<SampleType> SampleFactory::types() const 
{
  // Bouml preserved body begin 0002EF02
      vector<SampleType> types;
      types.push_back(SAMPLE_MONOCRYSTAL);
      
      return types;
  // Bouml preserved body end 0002EF02
}

/**
 * @brief Create a new reflection.
 * @return The created Reflection.
 */

hkl::Sample * SampleFactory::create(const std::string & name, hkl::SampleType type) const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002EE82
      Sample * sample;
      
      switch (type)
        {
        case SAMPLE_MONOCRYSTAL :
          sample = new sample::MonoCrystal(_geometry, name);
          break;
        default :
          HKLEXCEPTION("Unknown sample Type.", "Please use a correct type.");
        }
      return sample;
  // Bouml preserved body end 0002EE82
}


} // namespace hkl
