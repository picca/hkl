
#include "axe.h"
#include "quaternion.h"

namespace hkl {

/**
 * @brief constructor
 * @param name The name of the Axe.
 * @param description The description of the Axe.
 * @param min The minimum part of the Axe.
 * @param current The current hkl::Value of the Axe.
 * @param max The maximum hkl::Value of the Axe.
 */
Axe::Axe(const std::string & name, const std::string & description, const hkl::Value & min, const hkl::Value & current, const hkl::Value & max) throw(hkl::HKLException) :
  hkl::ObjectReadOnly(name, description),
  _min(min),
  _current(current),
  _max(max)
{
  // Bouml preserved body begin 00039282
  // Bouml preserved body end 00039282
}

Axe::~Axe() 
{
  // Bouml preserved body begin 00039882
  // Bouml preserved body end 00039882
}

Axe::Axe(const hkl::Axe & source) :
  ObjectReadOnly(source),
  Observable(source),
  _min(source._min),
  _current(source._current),
  _max(source._max)
{
  // Bouml preserved body begin 00039902
  // Bouml preserved body end 00039902
}

Axe * Axe::clone() const 
{
  // Bouml preserved body begin 00039802
    return new hkl::Axe(*this);
  // Bouml preserved body end 00039802
}

/**
 * @brief Applie to a hkl::Quaternion, the Axe.
 * @return The modified hkl::Quaternion
 */
hkl::Quaternion & Axe::apply(hkl::Quaternion & q) 
{
  // Bouml preserved body begin 0003BE82
  return q;
  // Bouml preserved body end 0003BE82
}

/**
 * @brief Compute the read distance between two Axe.
 * @param axe The hkl::Axe to compute the distance from. 
 * @return The distance between the two Axe.
 */
double Axe::get_distance(const hkl::Axe & axe) const 
{
  // Bouml preserved body begin 0003C002
  return _current.get_value() - axe._current.get_value();
  // Bouml preserved body end 0003C002
}

/**
 * @brief print the Axe into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & Axe::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00039A82
      flux << "\"" << this->get_name() << "\" : "
      << _current << " [" << _min << " : " << _max << "]";
      return flux;
  // Bouml preserved body end 00039A82
}

/**
 * @brief print on a stream the content of the Axe
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & Axe::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00039A02
    ObjectReadOnly::toStream(flux);
    _min.toStream(flux);
    _current.toStream(flux);
    _max.toStream(flux);
  
    return flux;
  // Bouml preserved body end 00039A02
}

/**
 * @brief restore the content of the Axe from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & Axe::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00039982
    ObjectReadOnly::fromStream(flux);
    _min.fromStream(flux);
    _current.fromStream(flux);
    _max.fromStream(flux);
  
    return flux;
  // Bouml preserved body end 00039982
}

AxeList::AxeList() 
{
  // Bouml preserved body begin 0003A982
  // Bouml preserved body end 0003A982
}

/**
 * @brief The default destructor.
 */
AxeList::~AxeList() 
{
  // Bouml preserved body begin 0003A002
    hkl::AxeList::iterator iter = _axes.begin();
    hkl::AxeList::iterator end = _axes.end();
    while(iter != end)
    {
      delete *iter;
      ++iter;
    }
  // Bouml preserved body end 0003A002
}

/**
 * @brief The copy constructor.
 * @param factory The factory to copy from.
 */
AxeList::AxeList(const hkl::AxeList & source) 
{
  // Bouml preserved body begin 0003A082
    hkl::AxeList::const_iterator iter = source._axes.begin();
    hkl::AxeList::const_iterator end = source._axes.end();
    while(iter != end)
    {
      _axes.push_back((*iter)->clone());
      ++iter;
    }
  // Bouml preserved body end 0003A082
}

/**
 * @brief Make a deep copy of a AxeList.
 * @return A pointer on the copied AxeList.
 */
hkl::AxeList * AxeList::clone() const 
{
  // Bouml preserved body begin 0003A102
    return new AxeList(*this);
  // Bouml preserved body end 0003A102
}

/**
 * @brief Add an Axe to the AxeList.
 * @param axe The added Axe.
 * @throw an exception if an axe with the same name is in the AxeList
 */
void AxeList::push_back(Axe * axe) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003A182
      try
      {
        // check if the Axe is in the AxeList.
        this->operator[](axe->get_name());
        // if no exception we can add the axe.
        _axes.push_back(axe);
      }
      catch(hkl::HKLException &)
      {
        throw;
      }
  // Bouml preserved body end 0003A182
}

/**
 * @return The number of axe in the AxeList.
 */
unsigned int AxeList::size() const 
{
  // Bouml preserved body begin 0003A282
      return _axes.size();
  // Bouml preserved body end 0003A282
}

/**
 * @brief Return the axe named.
 * @param name of the returned Reflection.
 * @throw HKLException if the Axe is not in the AxeList. 
 * @return The axe.
 */
Axe * AxeList::operator[](const std::string & name) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003A382
      hkl::AxeList::iterator iter = _axes.begin();
      hkl::AxeList::iterator end = _axes.end();
      while(iter != end)
        {
          if ((*iter)->get_name() == name)
              return *iter;
          ++iter;
        }
  
      std::ostringstream reason;
      std::ostringstream description;
  
      reason << "Cannot find the hkl::Axe named : " << name << " in the hkl::AxeList";
      description << "Available axes are :";
      iter = _axes.begin();
      while(iter != end)
        {
          description << " \"" << (*iter)->get_name() << "\"";
          ++iter;
        }
  
      HKLEXCEPTION(reason.str(), description.str());
  // Bouml preserved body end 0003A382
}

/**
 * @brief Return the axe named.
 * @param name of the returned Reflection.
 * @throw HKLException if the Axe is not in the AxeList. 
 * @return The axe.
 */
Axe * AxeList::operator[](const std::string & name) const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003B202
      hkl::AxeList::const_iterator iter = _axes.begin();
      hkl::AxeList::const_iterator end = _axes.end();
      while(iter != end)
        {
          if ((*iter)->get_name() == name)
              return *iter;
          ++iter;
        }
  
      std::ostringstream reason;
      std::ostringstream description;
  
      reason << "Cannot find the hkl::Axe named : " << name << " in the hkl::AxeList";
      description << "Available axes are :";
      iter = _axes.begin();
      while(iter != end)
        {
          description << " \"" << (*iter)->get_name() << "\"";
          ++iter;
        }
  
      HKLEXCEPTION(reason.str(), description.str());
  // Bouml preserved body end 0003B202
}

/**
 * @brief Return the axe named.
 * @param idx of the returned Reflection.
 * @throw HKLException if the Axe is not in the AxeList. 
 * @return The axe.
 */
Axe * AxeList::operator[](const unsigned int & idx) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003B082
  return _axes[idx];
  // Bouml preserved body end 0003B082
}

/**
 * @brief Return the axe named.
 * @param idx of the returned Reflection.
 * @throw HKLException if the Axe is not in the AxeList. 
 * @return The axe.
 */
Axe * AxeList::operator[](const unsigned int & idx) const throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0003B102
  return _axes[idx];
  // Bouml preserved body end 0003B102
}

/**
 * @brief Get an iterator on the first element of AxeList.
 * @return The iterator.
 */
hkl::AxeList::iterator AxeList::begin() 
{
  // Bouml preserved body begin 0003A402
        return _axes.begin();
  // Bouml preserved body end 0003A402
}

/**
 * @brief Get an iterator on the end of AxeList.
 * @return The iterator.
 */
hkl::AxeList::iterator AxeList::end() 
{
  // Bouml preserved body begin 0003A482
        return _axes.end();
  // Bouml preserved body end 0003A482
}

/**
 * @brief Get an const_iterator on the first element of AxeList.
 * @return The iterator.
 */
hkl::AxeList::const_iterator AxeList::begin() const 
{
  // Bouml preserved body begin 0003A802
          return _axes.begin();
  // Bouml preserved body end 0003A802
}

/**
 * @brief Get an const_iterator on the end of AxeList.
 * @return The iterator.
 */
hkl::AxeList::const_iterator AxeList::end() const 
{
  // Bouml preserved body begin 0003A882
          return _axes.end();
  // Bouml preserved body end 0003A882
}

/**
 * @brief Are two AxeList equals ?
 * @param axeList the hkl::AxeList to compare with.
 * @return true if both are equals false otherwise.
 */
bool AxeList::operator==(const hkl::AxeList & axeList) const 
{
  // Bouml preserved body begin 0003A502
      if (_axes.size() != axeList._axes.size())
          return false;
      else
        {
          hkl::AxeList::const_iterator iter = _axes.begin();
          hkl::AxeList::const_iterator end = _axes.end();
          hkl::AxeList::const_iterator iter2 = axeList._axes.begin();
          while(iter != end)
            {
              if (!(**iter == **iter2))
                  return false;
              ++iter;
              ++iter2;
            }
        }
      return true;
  // Bouml preserved body end 0003A502
}

/**
 * @brief print the AxeList into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
ostream & AxeList::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 0003A582
      hkl::AxeList::const_iterator iter = _axes.begin();
      hkl::AxeList::const_iterator end = _axes.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          flux << endl;
          ++iter;
        }
      return flux;
  // Bouml preserved body end 0003A582
}

/**
 * @brief print on a stream the content of the AxeList
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & AxeList::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 0003A602
      unsigned int nb_axes = _axes.size();
      flux << nb_axes << endl;
  
      hkl::AxeList::const_iterator iter = _axes.begin();
      hkl::AxeList::const_iterator end = _axes.end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
  
      return flux;
  // Bouml preserved body end 0003A602
}

/**
 * @brief restore the content of the AxeList from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & AxeList::fromStream(istream & flux) 
{
  // Bouml preserved body begin 0003A682
      // check that both samples have the same size.
      unsigned int nb_axes;
      flux >> nb_axes;
      if (nb_axes != _axes.size())
          HKLEXCEPTION("Can not restore this AxeList", "Not the same number of Sample.");
      else
        {
          iterator iter = _axes.begin();
          iterator end = _axes.end();
          while(iter != end)
            {
              (*iter)->fromStream(flux);
              ++iter;
            }
        }
      return flux;
  // Bouml preserved body end 0003A682
}


} // namespace hkl
