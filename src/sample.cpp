
#include "sample.h"
#include "geometry.h"

namespace hkl
  {

  /**
   * @brief The default constructor
   * @param geometry The geometry use to create the Reflections.
   * @param name The name of the Reflections.
   */

  Sample::Sample(hkl::Geometry & geometry, const std::string & name) :
      FitParameterList(),
      Object(name, "no description"),
      _geometry(geometry)
  {
    _parameters.push_back(&_lattice.a());
    _parameters.push_back(&_lattice.b());
    _parameters.push_back(&_lattice.c());
    _parameters.push_back(&_lattice.alpha());
    _parameters.push_back(&_lattice.beta());
    _parameters.push_back(&_lattice.gamma());
  }

  Sample::~Sample()
  {
  }

  Sample::Sample(const hkl::Sample & source) :
      FitParameterList(),
      Object(source),
      _geometry(source._geometry),
      _lattice(source._lattice)
  {
    _parameters.push_back(&_lattice.a());
    _parameters.push_back(&_lattice.b());
    _parameters.push_back(&_lattice.c());
    _parameters.push_back(&_lattice.alpha());
    _parameters.push_back(&_lattice.beta());
    _parameters.push_back(&_lattice.gamma());
  }

  /**
   * @brief Get the Lattice of the Sample.
   *
   * @return A reference on the Lattice.
   */

  hkl::Lattice & Sample::lattice()
  {
    return _lattice;
  }

  /**
   * @brief Get the reflections associated with the Sample.
   *
   * @return A reference on thoses reflections.
   */

  hkl::ReflectionList & Sample::reflections()
  {
    return *_reflections;
  }

  /**
   * \brief Are two Sample equals ?
   * \param sample the hkl::Sample to compare with.
   * \return true if both are equals flase otherwise.
   */
  bool Sample::operator==(const hkl::Sample & sample) const
    {
      return Object::operator==(sample)
             && _lattice == sample._lattice
             && *_reflections == *sample._reflections;
    }

  /**
   * @brief print the Sample into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Sample::printToStream(std::ostream & flux) const
    {
      unsigned int i;

      // Parameters
      flux << "\"" << get_name() << "\"" << std::endl;
      flux.width(9);
      flux << "  Parameters:";
      flux.width(9);
      flux << "value";
      flux.width(9);
      flux << "min";
      flux.width(9);
      flux << "max";
      flux << std::endl;
      for (i=0;i<3;i++)
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
          flux << std::endl;
        }
      for (i=3;i<6;i++)
        {
          FitParameter const & p = *_parameters[i];
          flux.precision(3);
          flux.width(9);
          flux << p.get_name() << "(" << p.get_flagFit() << "):";
          flux.width(9);
          flux << p.get_current().get_value() * HKL_RADTODEG;
          flux.width(9);
          flux << p.get_min().get_value() * HKL_RADTODEG;
          flux.width(9);
          flux << p.get_max().get_value() * HKL_RADTODEG;
          flux << std::endl;
        }

      //Reflections
      if (_reflections->size())
        {
          flux << std::endl << "  Reflections:" << std::endl
          << "  n";
          flux.width(9);
          flux << "h";
          flux.width(9);
          flux << "k";
          flux.width(9);
          flux << "l";
          flux << "  ";
          hkl::AxeList const & axeList = (*_reflections)[0]->get_geometry().get_axes();
          unsigned int n = axeList.size();
          for (i=0;i<n;i++)
            {
              flux.width(9);
              flux << axeList[i]->get_name();
            }
          flux << "  ";
          flux.width(9);
          flux << "lambda";
          flux << std::endl;
          std::vector<Reflection *>::const_iterator iter = _reflections->begin();
          std::vector<Reflection *>::const_iterator end = _reflections->end();
          n = 1;
          while (iter != end)
            {
              flux << "  " << n << **iter << std::endl;
              ++iter;
              ++n;
            }
        }
      else
        flux << std::endl << "  No reflection" << std::endl;
      return flux;
    }

  /**
   * @brief print on a stream the content of the Sample
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  std::ostream & Sample::toStream(std::ostream & flux) const
    {
      Object::toStream(flux);
      _lattice.toStream(flux);
      _reflections->toStream(flux);

      return flux;
    }

  /**
   * @brief restore the content of the Sample from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
   */
  std::istream & Sample::fromStream(std::istream & flux)
  {
    Object::fromStream(flux);
    _lattice.fromStream(flux);
    _reflections->fromStream(flux);

    return flux;
  }


} // namespace hkl
