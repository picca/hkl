#include "source.h"

namespace hkl {

    Source::Source(void)
      {
        m_waveLength = 0.;
        m_direction = svector(0., 0., 0.);
        m_qi = Quaternion(0., 0., 0., 0.);
      }

    Source::Source(Source const & source) :
      m_waveLength(source.m_waveLength),
      m_direction(source.m_direction),
      m_qi(source.m_qi)
    {}

    Source::Source(double const & waveLength, svector const & direction)
      {
        m_waveLength = waveLength;
        m_direction = direction.normalize();

        double k = constant::physic::tau / m_waveLength;
        svector ki(m_direction);
        ki *= k;
        m_qi = Quaternion(0., ki[0], ki[1], ki[2]);
      }

    bool
    Source::operator ==(Source const & source) const
      {
        return fabs(m_waveLength - source.m_waveLength) < constant::math::epsilon_0
        && m_direction == source.m_direction
        && m_qi == source.m_qi;
      }

      bool
      Source::isValid(void) const throw (HKLException)
      {
        if (m_waveLength < constant::math::epsilon_0)
            HKLEXCEPTION("The source wave length is null.", "Please set a non-null wave length.");
        if (m_direction == svector())
            HKLEXCEPTION("The source is set with a null direction.", "Please set a non-null direction.");
        return true;
      }

      void
      Source::setWaveLength(double waveLength) throw (HKLException)
        {
          if (fabs(waveLength) < constant::math::epsilon_0)
              HKLEXCEPTION("Cannot set a source with a null wave length",
                           "Please set a non-null wave length");
          else
            {
              m_waveLength = waveLength;

              if (!(m_direction == svector()))
                {
                  double k = constant::physic::tau / m_waveLength;
                  svector ki(m_direction);
                  ki *= k;
                  m_qi = Quaternion(0., ki[0], ki[1], ki[2]);
                }
            }
        }

      void
      Source::setDirection(svector const & direction) throw (HKLException)
        {
          if (direction == svector())
              HKLEXCEPTION("Cannot set a source with a null direction.", "Please set a non-null direction.");
          else
            {
              m_direction = direction.normalize();
              svector ki(m_direction);

              if (m_waveLength > constant::math::epsilon_1)
                {
                  double k = constant::physic::tau / m_waveLength;
                  ki *= k;
                }
              m_qi = Quaternion(0., ki[0], ki[1], ki[2]);
            }
        }

      svector
      Source::getKi(void) const
        {
          double k = constant::physic::tau / m_waveLength;

          svector ki(m_direction);
          ki *= k;

          return ki;
        }

      void
      Source::setKi(svector const & ki) throw (HKLException)
        {
          if (ki == svector())
              HKLEXCEPTION("Cannot set a source with a null Ki", "Please use a non-null Ki.");
          else
            {
              m_waveLength = constant::physic::tau / ki.norm2();
              m_direction = ki.normalize();

              m_qi = Quaternion(0., ki[0], ki[1], ki[2]);
            }
        }

      ostream &
      Source::toStream(ostream & flux) const
        {
          flux << setprecision(constant::math::precision) << " " << m_waveLength << endl;
          m_direction.toStream(flux);
          m_qi.toStream(flux);

          return flux;    
        }

      istream &
      Source::fromStream(istream & flux)
        {
          flux >> setprecision(constant::math::precision) >> m_waveLength;
          m_direction.fromStream(flux);
          m_qi.fromStream(flux);

          return flux;
        }

} // namespace hkl

ostream &
operator << (ostream& flux, hkl::Source const & source)
{
    flux << "Source: "
    << "Wave length = " << source.get_waveLength() << ", "
    << "Direction = " << source.get_direction() << ", "
    << "Qi = " << source.get_qi();

    return flux;
}
