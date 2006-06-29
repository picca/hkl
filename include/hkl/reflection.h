#ifndef _REFLECTION_H_
#define _REFLECTION_H_

#include <math.h>
#include <iomanip>
#include <iostream>
#include <constants.h>

#include "source.h"
#include "svecmat.h"
#include "mystring.h"
#include "geometry.h"

using namespace std;

namespace hkl {

    /*!
     * \brief The class reflection defines a configuration where a diffraction occurs. It
     * 
     * is defined by a set of angles, the 3 integers associated to the reciprocal
     * lattice and its relevance to make sure we only take into account significant
     * reflections.
     */

    template<class T>
    class Reflection
      {
      public:

        Reflection(void); //!< The default constructor.
        Reflection(Reflection const & reflection);
        Reflection(T const & geometry,
                   double const & h,
                   double const & k,
                   double const & l,
                   int const & relevance,
                   bool const & flag);
        virtual ~Reflection(void);
        bool operator == (Reflection const & r) const;
        T & get_geometry(void);
        T const & get_geometry(void) const;
        double const & get_h(void) const;
        double const & get_k(void) const;
        double const & get_l(void) const;
        int const & get_relevance(void) const;
        bool const & get_flag(void) const;
        svector const & get_hkl_phi(void) const;
        void set_geometry(T const & geometry);
        void set_h(double const & h);
        void set_k(double const & k);
        void set_l(double const & l);
        void set_relevance(int const & relevance);
        void set_flag(bool const & flag);
        bool toggle(void);
        svector getHKL(void) const;
        MyString getStrRelevance(void) const;
        double computeAngle(double const & h2, double const & k2, double const & l2) const;
        bool isColinear(Reflection const & reflection) const;
        ostream & toStream(ostream & flux) const;
        istream & fromStream(istream & flux);

      private:
        T m_geometry; //!< The corresponding Geometry.
        double m_h; //!< The first of the three numbers (h,k,l).
        double m_k; //!< The second of the three numbers (h,k,l).
        double m_l; //!< The third of the three numbers (h,k,l).
        int m_relevance; //!< Its associated relevance. 
        bool m_flag; //!< is the reflection use for calculation.
        svector m_hkl_phi; //!< juste utilisé pour accélérer les calcules de fitness des cristaux.
      };

    enum Relevance
      {
        notVerySignificant = 0, //!< not very significant reflection
        Significant, //!< significant reflection
        VerySignificant, //!< very significant reflection
        Best //!< Best reflection
      };

    /**
     * The default constructor.
     */
    template<class T>
    Reflection<T>::Reflection(void)
      {}

    /*!
     * \brief copy constructor
     * \param reflection The Reflection to copy from.
     */
    template<class T>
    Reflection<T>::Reflection(Reflection const & reflection) :
      m_geometry(reflection.m_geometry),
      m_h(reflection.m_h),
      m_k(reflection.m_k),
      m_l(reflection.m_l),
      m_relevance(reflection.m_relevance),
      m_flag(reflection.m_flag),
      m_hkl_phi(reflection.m_hkl_phi)
    {}

    /*!
     * \brief Constructor from parameters
     * \param geometry The Geometry storing the configuration of the diffractometer for this reflection.
     * \param h The h number of the reflection.
     * \param k The k number of the reflection.
     * \param l The l number of the reflection.
     * \param relevance The Relevance of this reflection.
     * \param flag true if the reflection is use during calculation.
     */
    template<class T>
    Reflection<T>::Reflection(T const & geometry,
                              double const & h,
                              double const & k,
                              double const & l,
                              int const & relevance,
                              bool const & flag) :
      m_geometry(geometry),
      m_h(h),
      m_k(k),
      m_l(l),
      m_relevance(relevance),
      m_flag(flag)
    {
      // do not forgot to update m_hkl_phi
      m_hkl_phi = m_geometry.getSampleRotationMatrix().transpose() * m_geometry.getQ();
    }

    /**
     * The default destructor
     */
    template<class T>
    Reflection<T>::~Reflection(void)
      {}

    /*!
     * \brief overload of the == operator for the reflection class.
     * \param r The reflection we want to compare with.
     * \return true if both reflections are equals, false otherwise.
     */
    template<class T>
    bool
    Reflection<T>::operator == (Reflection const & reflection) const
      {
        return m_geometry == reflection.m_geometry
        && m_h == reflection.m_h
        && m_k == reflection.m_k
        && m_l == reflection.m_l
        && m_relevance == reflection.m_relevance
        && m_flag == reflection.m_flag
        && m_hkl_phi == reflection.m_hkl_phi;
      }

    template<class T>
    T &
    Reflection<T>::get_geometry(void) {return m_geometry;} //!< get the angle configuration

    template<class T>
    T const &
    Reflection<T>::get_geometry(void) const {return m_geometry;} //!< get the angle configuration

    template<class T>
    double const &
    Reflection<T>::get_h(void) const {return m_h;} //!< get the h parameter of the reflection

    template<class T>
    double const &
    Reflection<T>::get_k(void) const {return m_k;} //!< get the k parameter of the reflection

    template<class T>
    double const &
    Reflection<T>::get_l(void) const {return m_l;} //!< get the l parameter of the reflection

    template<class T>
    int const &
    Reflection<T>::get_relevance(void) const {return m_relevance;} //!< get the relevance parameter of the reflection

    template<class T>
    bool const &
    Reflection<T>::get_flag(void) const {return m_flag;} //!< is the reflection use during the U calculation

    template<class T>
    svector const &
    Reflection<T>::get_hkl_phi(void) const {return m_hkl_phi;} //!< Get the hkl_phi of the reflection

    //!< set angleConfiguration
    template<class T>
    void
    Reflection<T>::set_geometry(T const & geometry)
      {
        m_geometry = geometry;

        // do not forgot to update m_hkl_phi
        m_hkl_phi = m_geometry.getSampleRotationMatrix().transpose() * m_geometry.getQ();
      }

    template<class T>
    void
    Reflection<T>::set_h(double const & h) {m_h = h;} //!< set h

    template<class T>
    void
    Reflection<T>::set_k(double const & k) {m_k = k;} //!< set k

    template<class T>
    void
    Reflection<T>::set_l(double const & l) {m_l = l;} //!< set l

    template<class T>
    void
    Reflection<T>::set_relevance(int const & relevance) {m_relevance = relevance;} //!< set relevance

    template<class T>
    void
    Reflection<T>::set_flag(bool const & flag) {m_flag = flag;} //!< set flag

    /**
     * toggle the reflection flag.
     */
    template<class T>
    bool
    Reflection<T>::toggle(void)
      {
        m_flag = !m_flag;

        return m_flag;
      }

    /**
     * return hkl as a svector.
     */
    template<class T>
    svector
    Reflection<T>::getHKL(void) const
      {
        return svector(m_h, m_k, m_l);
      }

    /**
     * get the relevance parameter of the reflection as a string
     */
    template<class T>
    MyString
    Reflection<T>::getStrRelevance(void) const
      {
        static const MyString strRelevance[] = {"notVerySignificant", "Significant", "VerySignificant", "Best"};
        return strRelevance[m_relevance];
      }

    /*!
     * \brief compute the angle between two reflections
     * \param h2 the h parameters of the second reflection
     * \param k2 the k parameters of the second reflection
     * \param l2 the l parameters of the second reflection
     *
     * Compute the angle between two reflections to get an idea about their level
     * of relevance (return the absolute value). As an example it can detect if
     * (m_h, m_k, m_l) and (h2, k2, l2) are parallel.
     */
    template<class T>
    double
    Reflection<T>::computeAngle(double const & h, double const & k, double const & l) const
      {
        double dot_product = h * m_h + k * m_k + l * m_l;
        double length1 = sqrt(m_h*m_h + m_k*m_k + m_l*m_l);
        double length2 = sqrt(h*h + k*k + l*l);
        double cosine = dot_product / (length1*length2);

        return acos(cosine);
      }

    /*!
     * \brief true if two reflections are colinear
     * \param reflection The reflection to compare with.
     * \return true if colinear, false otherwise.
     */
    template<class T>
    bool
    Reflection<T>::isColinear(Reflection const & reflection) const
      {
        svector v1(m_h, m_k, m_l);
        svector v2(reflection.m_h, reflection.m_k, reflection.m_l);
        if ((v1.vectorialProduct(v2)).norm2() < constant::math::epsilon_1)
            return true;
        else
            return false;
      }

    /*!
     * \brief Save the Reflection into a stream.
     * \param flux the stream to save the Reflection into.
     * \return The stream with the Reflection.
     */
    template<class T>
    ostream &
    Reflection<T>::toStream(ostream & flux) const
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

    /*!
     * \brief Restore a Reflection from a stream.
     * \param flux The stream containing the Reflection.
     */
    template<class T>
    istream &
    Reflection<T>::fromStream(istream & flux)
      {
        m_geometry.fromStream(flux);
        flux >> setprecision(constant::math::precision);
        flux >> m_h >> m_k >> m_l >> m_relevance >> m_flag;
        m_hkl_phi.fromStream(flux);

        return flux;
      }

} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << pour la class reflection
 * \param flux The flux to print into
 * \param r
 */
template<class T>
ostream &
operator << (ostream & flux, hkl::Reflection<T> const & reflection)
{
    flux.precision(3);
    flux.width(9);
    //flux << showpos;
    flux.width(9);flux << reflection.get_h();
    flux.width(9);flux << reflection.get_k();
    flux.width(9);flux << reflection.get_l();
    flux << " |";

    T const & geometry = reflection.get_geometry();
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
#endif // _REFLECTION_H_
