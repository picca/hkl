#ifndef _AXE_H
#define _AXE_H

#include "portability.h"

#include "mymap.h"
#include "svecmat.h"
#include "myvector.h"
#include "quaternion.h"
#include "fitparameter.h"

using namespace std;

namespace hkl
{

/*!
 * \brief A class design to describe a rotation axe
 */
class Axe : public FitParameter
{
public:

    /**
     * @brief constructor
     * @param name The name of the Axe.
     * @param description The description of the Axe.
     * @param min The minimum of the axe.
     * @param current The current position of the axe.
     * @param max The maximum value of the axe.
     * @param axe The axe coordinates.
     * @param direction +1 or -1 if the axe is a direct one or not.
     */
    Axe(MyString const & name, MyString const & description,
        double min, double current, double max,
        svector const & axe, int direction) throw (HKLException);

    /**
     * @brief The copy constructor
     * @param axe The Axe to copy from.
     */
    Axe(Axe const & axe);

    /**
     * @brief Return the axe coordinates
     * @return The axe coordinates as a 3 elements vector.
     */
    svector const & get_axe(void) const
    {
        return _axe;
    }

    /*!
     * \brief Return the axe rotation direction
     * \return +1 if the sens of rotation is direct
     * -1 otherwise
     */
    int const & get_direction(void) const
    {
        return _direction;
    }

    /*!
     * \brief Are two Axes equals ?
     * \param axe The axe to compare with
     */
    bool operator ==(Axe const & axe) const;

    /*!
     * @brief Get the Axe as a Quaternion.
     * @return The Quaternion corresponding to the Axe.
     */
    Quaternion asQuaternion(void) const;

    /**
     * Compute the distance between two Axes.
     * 
     * @param axe The Axe to compute the distance from.
     * 
     * @return The distance between the two axes.
     */
    double getDistance(Axe const & axe) const;

    /*!
     * \brief print the Axe into a flux
     * \param flux The stream to print into.
     */
    ostream & printToStream(ostream & flux) const;

    /*!
     * \brief Save the Axe into a stream.
     * \param flux the stream to save the Axe into.
     * \return The stream with the Axe.
     */
    ostream & toStream(ostream & flux) const;

    /*!
     * \brief Restore an Axe from a stream.
     * \param flux The stream containing the Axe.
     */
    istream & fromStream(istream & flux);

private:
    svector _axe; //!< the coordinates of the axe
    int _direction; //!< rotation diraction of the axe
};

#ifdef MSVC6
typedef MyMap<Axe> AxeMap;
typedef MyStarVector<Axe *> AxeVector;
#else
typedef MyMap<Axe> AxeMap;
typedef MyVector<Axe *> AxeVector;
#endif
} // namespace hkl

/**
 * \brief Overload of the << operator for the Axe class
 */
inline ostream &
operator<<(ostream & flux, hkl::Axe const & axe)
{
    return axe.printToStream(flux);
}


#endif // _AXE_H
