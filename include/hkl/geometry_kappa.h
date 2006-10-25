#ifndef _GEOMETRY_KAPPA_H_
#define _GEOMETRY_KAPPA_H_

#include "geometry.h"

namespace hkl {
    namespace geometry {

        /**
         * \brief An Geometry for a the kappa 6 circle soleil generic diffractometer.
         */
        class Kappa : public Geometry
        {
        public:

          /**
           * \brief The destructor
           */
          virtual ~Kappa(void);

          /** 
           * @brief Return the m_alpha parameter of the Kappa geometry.
           * 
           * @return The m_alpha parameter of the Kappa geometry.
           */
          double const & get_alpha(void) const {return _alpha;}

          /*!
           * \brief Assignation of the Geometry.
           * \param geometry The Geometry to assign.
           */
          Kappa & operator=(Kappa const & geometry);

          /** 
           * @brief Get the validity of the geometry.
           * 
           * @return True if the source is set correctly and if alpha is non-null.
           */
          bool isValid(void) const throw (HKLException);

          /*!
           * \brief put the angleConfiguration into a stream
           * \param flux
           */
          ostream & printToStream(ostream & flux) const;

          /*!
           * \brief Save the Geometry into a stream.
           * \param flux the stream to save the Geometry into.
           * \return The stream with the Geometry.
           */
          ostream & toStream(ostream & flux) const;

          /*!
           * \brief Restore an Geometry from a stream.
           * \param flux The stream containing the Geometry.
           */
          istream & fromStream(istream & flux);

        protected:
          double _alpha; //!< The alpha angle of the Kappa geometry.

          /**
           * \brief The default constructor -- Private to be an abstract class.
           * @param name The name of the Kappa Geometry.
           * @param description The description of the Kappa Geometry.
           * @param alpha The alpha parameter of the Kappa geometry
           */
          Kappa(MyString const & name, MyString const & descrition, double alpha);
        };

    } // namespace geometry
} // namespace hkl

#endif // _GEOMETRY_KAPPA_H_
