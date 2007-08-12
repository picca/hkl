
#include "fitparameterlist.h"
#include "fitparameter.h"

namespace hkl {

FitParameterList::~FitParameterList() 
{
  // Bouml preserved body begin 00034082
  // Bouml preserved body end 00034082
}

/**
 * @brief Get the size of the FitParameterList.
 * @return the number of element in the FitParameterList.
 */
unsigned int FitParameterList::size() const 
{
  // Bouml preserved body begin 00027182
      return _parameters.size();
  // Bouml preserved body end 00027182
}

/**
 * @brief Get the number of Parameter to fit in the FitParameterList.
 * @return The number of Parameter with the fitFlag set to true.
 */
unsigned int FitParameterList::size_to_fit() const 
{
  // Bouml preserved body begin 00027682
      vector<FitParameter *>::const_iterator iter = _parameters.begin();
      vector<FitParameter *>::const_iterator end = _parameters.end();
      
      unsigned int n = 0;
      while(iter != end)
        {
          if ((*iter)->get_flagFit())
            n++;
          ++iter;
        }
      return n;
  // Bouml preserved body end 00027682
}

/**
 * @return the std::string * named
 * @param name The name of the std::string we are looking for in the FitParameterList.
 * @return A std::string pointer.
 * @throw HKLException if the std::string is not present n the FitParameterList.
 */
hkl::FitParameter * FitParameterList::operator[](const std::string & name) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00027202
      vector<FitParameter *>::iterator iter = _parameters.begin();
      vector<FitParameter *>::iterator end = _parameters.end();
      
      while(iter != end)
        {
          if (name == (*iter)->get_name())
            return *iter;
          ++iter;
        }
      HKLEXCEPTION("can not find the parameter","");
  // Bouml preserved body end 00027202
}

/**
 * @brief Get an iterator on the first element of the FitParameterList.
 * @return The iterator.
 */
hkl::FitParameterList::iterator FitParameterList::begin() 
{
  // Bouml preserved body begin 00027282
      return _parameters.begin();
  // Bouml preserved body end 00027282
}

/**
 * @brief Get an iterator on the end of the FitParameterList.
 * @return The iterator.
 */
hkl::FitParameterList::iterator FitParameterList::end() 
{
  // Bouml preserved body begin 00027302
      return _parameters.end();
  // Bouml preserved body end 00027302
}

/**
 * @brief Get an const_iterator on the first element of the FitParameterList.
 * @return The const_iterator.
 */
hkl::FitParameterList::const_iterator FitParameterList::begin() const 
{
  // Bouml preserved body begin 00027382
      return _parameters.begin();
  // Bouml preserved body end 00027382
}

/**
 * @brief Get an const_iterator on the end of the FitParameterList.
 * @return The const_iterator.
 */
hkl::FitParameterList::const_iterator FitParameterList::end() const 
{
  // Bouml preserved body begin 00027402
      return _parameters.end();
  // Bouml preserved body end 00027402
}

/*!
 * \brief Are two FitParameterList equals ?
 * \param fitParameterList the hkl::FitParameterList to compare with.
 */

bool FitParameterList::operator==(const hkl::FitParameterList & fitParameterList) const 
{
  // Bouml preserved body begin 00027982
      if (_parameters.size() != fitParameterList._parameters.size())
        return false;
      else
        {
          vector<FitParameter *>::const_iterator iter = _parameters.begin();
          vector<FitParameter *>::const_iterator iter2 = fitParameterList._parameters.begin();
          vector<FitParameter *>::const_iterator end = _parameters.end();
          while(iter != end)
            {
              if ( !(**iter == **iter2) )
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
  // Bouml preserved body end 00027982
}

/*!
 * \brief print the FitParameterList into a flux
 * \param flux The stream to print into.
 */
ostream & FitParameterList::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00027502
      vector<FitParameter *>::const_iterator iter = _parameters.begin();
      vector<FitParameter *>::const_iterator end = _parameters.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00027502
}

/*!
 * \brief Save the FitParameterList into a stream.
 * \param flux the stream to save the FitParameterList into.
 * \return The stream with the FitParameterList.
 */
ostream & FitParameterList::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00027582
      vector<FitParameter *>::const_iterator iter = _parameters.begin();
      vector<FitParameter *>::const_iterator end = _parameters.end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00027582
}

/*!
 * \brief Restore a FitParameterList from a stream.
 * \param flux The stream containing the FitParameterList to restore.
 * @todo call update_observers or not ?
 */
istream & FitParameterList::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00027602
      vector<FitParameter *>::iterator iter = _parameters.begin();
      vector<FitParameter *>::iterator end = _parameters.end();
      while(iter != end)
        {
          (*iter)->fromStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00027602
}


} // namespace hkl
