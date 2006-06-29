#ifndef _AXE_H
#define _AXE_H

#include "config.h"

#include <math.h>
#include <vector>
#include <iostream>
#include <iomanip>

#include "mymap.h"
#include "range.h"
#include "svecmat.h"
#include "mystring.h"
#include "quaternion.h"

using namespace std;

namespace hkl {
  
  /*!
   * \brief A class design to describe a rotation axe
   */
 class Axe : public Range
  {
  public:
      
    Axe(void); //!< Default constructor
    
    /*!
     * \brief constructor
     * \param name The name of the Axe
     * \param axe A 3 element vector describing the axe coordinates.
     * \param direction +1 or -1 if the axe is a direct one or not.
     */
    Axe(MyString const & name, svector const & axe, int direction);
  

    /*! 
     * \brief constructor
     * \param name The name of the Axe
     * \param axe A 3 element vector describing the axe coordinates. 
     * \param direction +1 or -1 if the axe is a direct one or not.
     * \param position The current position of the Axe.	
     * \return A new Axe
     */
    Axe(MyString const & name, svector const & axe, int direction, double position);

    /*!
     * \brief Copy constructor
     * \param axe An axe to copy from
     */
    Axe(Axe const & axe);
      
    /*!
     * \brief The default destructor
     */
    virtual ~Axe(void);
  
    
    /*!
     * \brief Return the axe coordinates
     * \return The axe coordinates as a 3 elements vector.
     */
    svector const & get_axe(void) const {return m_axe;}
  
    /*!
     * \brief Return the axe rotation direction
     * \return +1 if the sens of rotation is direct
     * -1 otherwise
     */
    int const & get_direction(void) const {return m_direction;}
  
    /*!
     * \brief Set the coordinates of the rotation axe.
     * \param axe A 3 elements vector.
     */
    void set_axe(svector const & axe) {m_axe = axe;}
  
    /*!
     * \brief Set the Axe sens of rotation
     * \param i +1 direct rotation or -1 for non-direct rotation
     */
    void set_direction(int i);
     
    /*!
     * \brief Are two Axes equals ?
     * \param axe The axe to compare with
     */
    bool operator ==(Axe const & axe) const;
    
    /*!
     * @brief Get the %Axe as a %Quaternion.
     * @return The %Quaternion corresponding to the %Axe.
     */
    Quaternion asQuaternion(void) const;

    /** 
     * Compute the distance between two Axes.
     * 
     * @param axe The axe to compute the distance from.
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
    svector m_axe; //!< the coordinates of the axe
    int m_direction; //!< rotation diraction of the axe
  };
 
#ifdef MSVC6
  typedef MyStarMap<Axe *> AxeMap; //!< \typedef A MyMap containing Axes.
  typedef MyStarVector<Axe *> AxeVector; //!< \typedef A MyMap containing Axes.
#else
  typedef MyMap<Axe *> AxeMap; //!< \typedef A MyMap containing Axes.
  typedef MyVector<Axe *> AxeVector; //!< \typedef A MyMap containing Axes.
#endif
} // namespace hkl

/**
 * \brief Overload of the << operator for the Axe class
 */
ostream & operator<<(ostream & flux, hkl::Axe const & axe); 


#endif // _AXE_H
