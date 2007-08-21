#ifndef _AXE_H
#define _AXE_H


#include "fitparameter.h"
#include "observer.h"
#include <string>

#include "value.h"
#include "HKLException.h"
#include <iostream>
using namespace std;
#include <vector>
#include <vector>


namespace hkl { class Quaternion; } 

namespace hkl {

enum AxeType {
  AXE_ROTATION
};
class Axe : public hkl::FitParameter, public hkl::Observable {
  public:
    /**
     * @brief constructor
     * @param name The name of the Axe.
     * @param description The description of the Axe.
     * @param min The minimum part of the Axe.
     * @param current The current hkl::Value of the Axe.
     * @param max The maximum hkl::Value of the Axe.
     */
    Axe(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max) throw(hkl::HKLException);

    virtual ~Axe();

    virtual hkl::AxeType get_type() const = 0;

    inline virtual void set_current(const hkl::Value & value);

    inline virtual void set_min(const hkl::Value & value);

    inline virtual void set_max(const hkl::Value & value);

    virtual Axe * clone() const = 0;

    /**
     * @brief Applie to a hkl::Quaternion, the Axe.
     * @return The modified hkl::Quaternion
     */
    virtual hkl::Quaternion & apply(hkl::Quaternion & q) = 0;

    /**
     * @brief Compute the read distance between two Axe.
     * @param axe The Axe to compute the distance from. 
     * @return The distance between the two Axe.
     */
    virtual double get_distance(const Axe & axe) const throw(hkl::HKLException) = 0;

    /**
     * @brief print the Axe into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    virtual ostream & printToStream(ostream & flux) const;

    /**
     * @brief print on a stream the content of the Axe
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    virtual ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the Axe from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    virtual istream & fromStream(istream & flux);

};
inline void Axe::set_current(const hkl::Value & value) 
{
  // Bouml preserved body begin 0003E702
  Range::set_current(value);
  this->set_changed();
  this->update_observers();
  // Bouml preserved body end 0003E702
}

inline void Axe::set_min(const hkl::Value & value) 
{
  // Bouml preserved body begin 0003E782
  Range::set_range(value, _max);
  this->set_changed();
  this->update_observers();
  // Bouml preserved body end 0003E782
}

inline void Axe::set_max(const hkl::Value & value) 
{
  // Bouml preserved body begin 0003E802
  Range::set_range(_min, value);
  this->set_changed();
  this->update_observers();
  // Bouml preserved body end 0003E802
}

/**
 * @todo use a factory to create the different axes.
 */
class AxeList {
  protected:
    std::vector<hkl::Axe *> _axes;


  public:
    typedef vector<hkl::Axe *>::iterator iterator;

    typedef vector<hkl::Axe *>::const_iterator const_iterator;

    /**
     * @brief Add an hkl::Axe to the AxeList.
     * @param axe The added hkl::Axe.
     */
    void push_back(hkl::Axe * axe);

    /**
     * @brief Check if an axe with the name has_axe is already in the AxeList
     * @param name The std::string with the name of the axe to check for.
     * @return true if the axe is already present in the Axe
     */
    bool has_axe(const std::string & name) const;

    /**
     * @return The number of axe in the AxeList.
     */
    unsigned int size() const;

    /**
     * @brief compute the distance between two AxeList.
     * @param @{p0} The @{t0} to compare with.
     * @return the distance.
     */
    double get_distance(const AxeList & axeList) const;

    /**
     * @brief Return the axe named.
     * @param name of the returned Reflection.
     * @throw HKLException if the Axe is not in the AxeList. 
     * @return The axe.
     */
    Axe * operator[](const std::string & name) throw(hkl::HKLException);

    /**
     * @brief Return the axe named.
     * @param name of the returned Reflection.
     * @throw HKLException if the Axe is not in the AxeList. 
     * @return The axe.
     */
    Axe * operator[](const std::string & name) const throw(hkl::HKLException);

    /**
     * @brief Return the axe named.
     * @param idx of the returned Reflection.
     * @throw HKLException if the Axe is not in the AxeList. 
     * @return The axe.
     */
    Axe * operator[](const unsigned int & idx) throw(hkl::HKLException);

    /**
     * @brief Return the axe named.
     * @param idx of the returned Reflection.
     * @throw HKLException if the Axe is not in the AxeList. 
     * @return The axe.
     */
    Axe * operator[](const unsigned int & idx) const throw(hkl::HKLException);

    /**
     * @brief Get an iterator on the first element of AxeList.
     * @return The iterator.
     */
    iterator begin();

    /**
     * @brief Get an iterator on the end of AxeList.
     * @return The iterator.
     */
    iterator end();

    /**
     * @brief Get an const_iterator on the first element of AxeList.
     * @return The iterator.
     */
    const_iterator begin() const;

    /**
     * @brief Get an const_iterator on the end of AxeList.
     * @return The iterator.
     */
    const_iterator end() const;

    void clear();

    /**
     * @brief Are two AxeList equals ?
     * @param axeList the AxeList to compare with.
     * @return true if both are equals false otherwise.
     */
    bool operator==(const AxeList & axeList) const;

    /**
     * @brief print the AxeList into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    ostream & printToStream(ostream & flux) const;

    /**
     * @brief print on a stream the content of the AxeList
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the AxeList from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    istream & fromStream(istream & flux);

};

} // namespace hkl

/**
 * \brief Overload of the << operator for the Axe class
 */
inline ostream &
operator<<(ostream & flux, hkl::Axe const & axe)
{
  return axe.printToStream(flux);
}

/**
 * \brief Overload of the << operator for the AxeList class
 */
inline ostream &
operator<<(ostream & flux, hkl::AxeList const & axeList)
{
  return axeList.printToStream(flux);
}
#endif
