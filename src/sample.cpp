#include "sample.h"

using namespace std;

namespace hkl
  {

  Sample::Sample(Geometry & geometry, MyString const & name) :
      Object(name, "no description"),
      FitParameterList(),
      _geometry(geometry)
  {
    _parameters.push_back(&_lattice.a());
    _parameters.push_back(&_lattice.b());
    _parameters.push_back(&_lattice.c());
    _parameters.push_back(&_lattice.alpha());
    _parameters.push_back(&_lattice.beta());
    _parameters.push_back(&_lattice.gamma());
  }

  Sample::Sample(Sample const & sample) :
      Object(sample),
      FitParameterList(),
      _geometry(sample._geometry),
      _lattice(sample._lattice)
  {
    _parameters.push_back(&_lattice.a());
    _parameters.push_back(&_lattice.b());
    _parameters.push_back(&_lattice.c());
    _parameters.push_back(&_lattice.alpha());
    _parameters.push_back(&_lattice.beta());
    _parameters.push_back(&_lattice.gamma());
  }

  Sample::~Sample(void)
  {}

  bool
  Sample::operator == (Sample const & sample) const
    {
      return Object::operator==(sample)
             && _lattice == sample._lattice
             && *_reflections == *sample._reflections;
    }

  ostream &
  Sample::printToStream(ostream & flux) const
    {
      unsigned int i;
      unsigned int j;

      // Parameters
      flux << "\"" << get_name() << "\"" << endl;
      flux.width(9);
      flux << "  Parameters:";
      flux.width(9);
      flux << "value";
      flux.width(9);
      flux << "min";
      flux.width(9);
      flux << "max";
      flux << endl;
      for(i=0;i<3;i++)
        {
          FitParameter const & p = *_parameters[i];
          flux.precision(3);
          flux.width(9);
          flux << p.get_name() << "(" << p.get_flagFit() << "):";
          flux.width(9);
          flux << p.get_current().get_value();
          flux.width(9);
          flux << p.get_min().get_value();
          flux.width(9);
          flux << p.get_max().get_value();
          flux << endl;
        }
      for(i=3;i<6;i++)
        {
          FitParameter const & p = *_parameters[i];
          flux.precision(3);
          flux.width(9);
          flux << p.get_name() << "(" << p.get_flagFit() << "):";
          flux.width(9);
          flux << p.get_current().get_value()*constant::math::radToDeg;
          flux.width(9);
          flux << p.get_min().get_value()*constant::math::radToDeg;
          flux.width(9);
          flux << p.get_max().get_value()*constant::math::radToDeg;
          flux << endl;
        }

      //Reflections
      if (_reflections->size())
        {
          flux << endl << "  Reflections:" << endl
          << "  n";
          flux.width(9);
          flux << "h";
          flux.width(9);
          flux << "k";
          flux.width(9);
          flux << "l";
          flux << "  ";
          vector<string> axesNames = (*_reflections)[0]->get_geometry().getAxesNames();
          unsigned int n = axesNames.size();
          for(i=0;i<n;i++)
            {
              flux.width(9);
              flux << axesNames[i];
            }
          flux << "  ";
          flux.width(9);
          flux << "lambda";
          flux << endl;
          vector<Reflection *>::const_iterator iter = _reflections->begin();
          vector<Reflection *>::const_iterator end = _reflections->end();
          n = 1;
          while(iter != end)
            {
              flux << "  " << n << **iter << endl;
              ++iter;
              ++n;
            }
        }
      else
        flux << endl << "  No reflection" << endl;
      return flux;
    }

  ostream &
  Sample::toStream(ostream & flux) const
    {
      Object::toStream(flux);
      _lattice.toStream(flux);
      _reflections->toStream(flux);

      return flux;
    }

  istream &
  Sample::fromStream(istream & flux)
  {
    Object::fromStream(flux);
    _lattice.fromStream(flux);
    _reflections->fromStream(flux);

    return flux;
  }

} // namespace hkl
