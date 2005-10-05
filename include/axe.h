#ifndef _AXE_H
#define _AXE_H

//#include "config.h"

#include <math.h>
#include <string>
#include <vector>
#include <iostream>
#include <iomanip>

#include "mymap.h"
#include "range.h"
#include "quaternion.h"
#include "svecmat.h"

namespace hkl {

/**
 * \brief A class design to describe a rotation axe
 */
class Axe : public Range
{
  public:
    /**
     * \brief constructor
     * \param name The name of the Axe
     * \param axe A 3 element vector describing the axe coordinates
     * \param direction +1 or -1 if the axe is a direct one or not.
     */
    Axe(std::string const & name, svector const & axe, int direction);

    /**
     * \brief Copy constructor
     * \param axe An axe to copy from
     */
    Axe(Axe const & axe);
    
    /**
     * \brief The default destructor
     */
    virtual ~Axe(void);

  
    /**
     * \brief Return the axe coordinates
     * \return The axe coordinates as a 3 elements vector.
     */
    svector const & get_axe(void) const {return m_axe;}

    /**
     * \brief Return the axe rotation direction
     * \return +1 if the sens of rotation is direct
     * -1 otherwise
     */
    int const & get_direction(void) const {return m_direction;}

    /**
     * \brief Set the coordinates of the rotation axe.
     * \param axe A 3 elements vector.
     */
    void set_axe(svector const & axe) {m_axe = axe;}

    /**
     * \brief Set the Axe sens of rotation
     * \param i +1 direct rotation or -1 for non-direct rotation
     */
    void set_direction(int i);
   
    /**
     * \brief Are two Axes equals ?
     * \param axe The axe to compare with
     */
    bool operator ==(Axe const & axe) const;
  
    /**
     * @brief Get the %Axe as a %Quaternion.
     * @return The %Quaternion corresponding to the %Axe.
     */
    Quaternion asQuaternion(void) const;
    
    /**
     * \brief print the Axe into a flux
     * \param flux The stream to print into.
     */
    std::ostream & printToStream(std::ostream & flux) const;
    
  private:
    svector m_axe; //< the coordinates of the axe
    int m_direction; //< rotation diraction of the axe
};

typedef MyMap<Axe> AxeMap;

} // namespace hkl

/**
 * \brief Overload of the << operator for the Axe class
 */
std::ostream & operator<<(std::ostream & flux, hkl::Axe const & axe); 


#endif // _AXE_H
