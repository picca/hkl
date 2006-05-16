#include "reflection.h"

namespace hkl {

    Reflection::Reflection(void) {}

    Reflection::Reflection(Reflection const & reflection)
      {
        m_geometry = reflection.m_geometry;
        m_relevance = reflection.m_relevance;
        m_h = reflection.m_h;
        m_k = reflection.m_k;
        m_l = reflection.m_l;
        m_flag = reflection.m_flag;
        m_hkl_phi = reflection.m_hkl_phi;
      }

    Reflection::Reflection(Geometry const & geometry,
                           double const & h,
                           double const & k,
                           double const & l,
                           int const & relevance,
                           bool const & flag)
      {
        m_geometry = geometry;
        m_relevance = relevance;
        m_h = h;
        m_k = k;
        m_l = l;
        m_flag = flag;

        // do not forgot to update m_hkl_phi
        m_hkl_phi = m_geometry.getSampleRotationMatrix().transpose() * m_geometry.getQ();
      }

    Reflection::~Reflection(void) {}

    bool
    Reflection::operator == (Reflection const & reflection) const
      {
        return m_geometry == reflection.m_geometry
        && m_h == reflection.m_h
        && m_k == reflection.m_k
        && m_l == reflection.m_l
        && m_relevance == reflection.m_relevance
        && m_flag == reflection.m_flag
        && m_hkl_phi == reflection.m_hkl_phi;
      }


    void
    Reflection::set_geometry(Geometry const & geometry)
      {
        m_geometry = geometry;

        // do not forgot to update m_hkl_phi
        m_hkl_phi = m_geometry.getSampleRotationMatrix().transpose() * m_geometry.getQ();
      }

    MyString
    Reflection::getStrRelevance(void) const
      {
        return m_strRelevance[m_relevance];
      }

    bool
    Reflection::toggle(void)
      {
        m_flag = !m_flag;

        return m_flag;
      }

    svector
    Reflection::getHKL(void) const
      {
        return svector(m_h, m_k, m_l);
      }

    double
    Reflection::computeAngle(double const & h, double const & k, double const & l) const
      {
        double dot_product = h * m_h + k * m_k + l * m_l;
        double length1 = sqrt(m_h*m_h + m_k*m_k + m_l*m_l);
        double length2 = sqrt(h*h + k*k + l*l);
        double cosine = dot_product / (length1*length2);

        return acos(cosine);
      }

    bool
    Reflection::isColinear(Reflection const & reflection) const
      {
        svector v1(m_h, m_k, m_l);
        svector v2(reflection.m_h, reflection.m_k, reflection.m_l);
        if ((v1.vectorialProduct(v2)).norm2() < constant::math::epsilon_1)
            return true;
        else
            return false;
      }

    ostream &
    Reflection::toStream(ostream & flux) const
      {
        m_geometry.toStream(flux);
        flux << setprecision(constant::math::precision);
        flux << " " << m_h;
        flux << " " << m_k;
        flux << " " << m_l;
        flux << " " << m_relevance;
        flux << " " << m_flag;
        m_hkl_phi.toStream(flux);

        return flux;
      }

    istream &
    Reflection::fromStream(istream & flux)
      {
        m_geometry.fromStream(flux);
        flux >> setprecision(constant::math::precision);
        flux >> m_h >> m_k >> m_l >> m_relevance >> m_flag;
        m_hkl_phi.fromStream(flux);

        return flux;
      }

    MyString
    Reflection::m_strRelevance[] = {"notVerySignificant", "Significant", "VerySignificant", "Best"};

} // namespace hkl

ostream &
operator << (ostream & flux, hkl::Reflection const & reflection)
{
    flux.precision(3);
    flux.width(9);
    //flux << showpos;
    flux.width(9);flux << reflection.get_h();
    flux.width(9);flux << reflection.get_k();
    flux.width(9);flux << reflection.get_l();
    flux << " |";

    hkl::Geometry const & geometry = reflection.get_geometry();
    vector<hkl::MyString> axesNames = geometry.getAxesNames();

    unsigned int nb_axes = axesNames.size();
    unsigned int i;
    for(i=0; i<nb_axes; i++)
      {
        flux.width(9);
        flux << geometry.get_axe(axesNames[i]).get_value() * hkl::constant::math::radToDeg;
      }
    flux << " |";
    flux.width(9);
    flux << geometry.get_source().get_waveLength();
    flux << " | " << reflection.getStrRelevance()
    << "(" << reflection.get_flag() << ") ";

    return flux;
}
