#include "samplefactory.h"
#include "sample_monocrystal.h"

using namespace std;

namespace hkl
  {

  SampleFactory::SampleFactory(Geometry & geometry) :
      _geometry(geometry)
  {}

  SampleFactory::~SampleFactory(void)
  {}

  vector<SampleType>
  SampleFactory::types(void) const
    {
      vector<SampleType> types;
      types.push_back(SAMPLE_MONOCRYSTAL);

      return types;
    }

  Sample *
  SampleFactory::create(MyString const & name, SampleType type) const
    {
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
    }
} // namespace hkl
