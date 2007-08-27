
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
  hkl::FitParameter(name, description, min, current, max, true, hkl::constant::math::epsilon)
{
  // Bouml preserved body begin 00039282
  // Bouml preserved body end 00039282
}

Axe::~Axe() 
{
  // Bouml preserved body begin 00039882
  // Bouml preserved body end 00039882
}

/**
 * @brief print the Axe into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
std::ostream & Axe::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00039A82
    flux << "\"" << this->get_name() << "\" : "
    << _current << ", " << _consign << " [" << _min << " : " << _max << "]";
    return flux;
  // Bouml preserved body end 00039A82
}

/**
 * @brief print on a stream the content of the Axe
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & Axe::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00039A02
    FitParameter::toStream(flux);
  
    return flux;
  // Bouml preserved body end 00039A02
}

/**
 * @brief restore the content of the Axe from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & Axe::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00039982
    FitParameter::fromStream(flux);
  
    return flux;
  // Bouml preserved body end 00039982
}

/**
 * @brief Add an hkl::Axe to the AxeList.
 * @param axe The added hkl::Axe.
 */
void AxeList::push_back(hkl::Axe * axe) 
{
  // Bouml preserved body begin 0003A182
  _axes.push_back(axe);
  // Bouml preserved body end 0003A182
}

/**
 * @brief Check if an axe with the name has_axe is already in the AxeList
 * @param name The std::string with the name of the axe to check for.
 * @return true if the axe is already present in the Axe
 */
bool AxeList::has_axe(const std::string & name) const 
{
  // Bouml preserved body begin 0003E182
  for(unsigned int i=0; i<_axes.size(); i++)
  {
    if (_axes[i]->get_name() == name)
      return true;
  }
  return false;
  // Bouml preserved body end 0003E182
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
 * @brief compute the distance between two AxeList.
 * @param axeList The hkl::AxeList to compare with.
 * @return the distance.
 */
double AxeList::get_distance(const hkl::AxeList & axeList) const 
{
  // Bouml preserved body begin 0003CC82
  double distance = 0;
  std::vector<hkl::Axe *>::const_iterator iter1 = _axes.begin();
  std::vector<hkl::Axe *>::const_iterator end = _axes.end();
  std::vector<hkl::Axe *>::const_iterator iter2 = axeList.begin();
  while(iter1 != end)
  {
    distance += (*iter1)->get_distance(**iter2);
    ++iter1;
    ++iter2;
  }
  return distance;
  // Bouml preserved body end 0003CC82
}

/**
 * @brief compute the distance between two AxeList.
 * @param axeList The hkl::AxeList to compare with.
 * @return the distance.
 */
double AxeList::get_distance_consign(const hkl::AxeList & axeList) const 
{
  // Bouml preserved body begin 00040002
    double distance = 0;
    std::vector<hkl::Axe *>::const_iterator iter1 = _axes.begin();
    std::vector<hkl::Axe *>::const_iterator end = _axes.end();
    std::vector<hkl::Axe *>::const_iterator iter2 = axeList.begin();
    while(iter1 != end)
    {
      distance += (*iter1)->get_distance_consign(**iter2);
      ++iter1;
      ++iter2;
    }
    return distance;
  // Bouml preserved body end 00040002
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

void AxeList::clear() 
{
  // Bouml preserved body begin 0003DD02
  _axes.clear();
  // Bouml preserved body end 0003DD02
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
std::ostream & AxeList::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0003A582
      hkl::AxeList::const_iterator iter = _axes.begin();
      hkl::AxeList::const_iterator end = _axes.end();
      while(iter != end)
        {
          flux << **iter << std::endl;
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
std::ostream & AxeList::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 0003A602
      unsigned int nb_axes = _axes.size();
      flux << nb_axes << std::endl;
  
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
std::istream & AxeList::fromStream(std::istream & flux) 
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
