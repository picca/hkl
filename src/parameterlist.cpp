
#include "parameterlist.h"
#include "parameter.h"

namespace hkl {

/**
 * @brief Add a hkl::Parameter to the ParameterList.
 * @param parameter The hkl::Parameter to add.
 */
bool ParameterList::add(hkl::Parameter * parameter) 
{
  // Bouml preserved body begin 00026A82
      vector<Parameter *>::iterator iter = _parameters.begin();
      vector<Parameter *>::iterator end = _parameters.end();
      while(iter != end)
        {
          if ((*iter)->get_name() == parameter->get_name())
            return false;
          ++iter;
        }
      _parameters.push_back(parameter);
      return true;
  // Bouml preserved body end 00026A82
}

/**
 * @brief Get the size of the ParameterList.
 * @return the number of element in the ParameterList.
 */
unsigned int ParameterList::size() const 
{
  // Bouml preserved body begin 00026B02
      return _parameters.size();
  // Bouml preserved body end 00026B02
}

vector<string> ParameterList::get_names() const 
{
  // Bouml preserved body begin 00038082
      std::vector<std::string> names;
      
      const_iterator iter = _parameters.begin();
      const_iterator end = _parameters.end();
      while(iter != end)
      {
        names.push_back( (*iter)->get_name() );
        ++iter;
      }
      
      return names;
  // Bouml preserved body end 00038082
}

/**
 * @return the std::string * named
 * @param name The name of the std::string we are looking for in the ParameterList.
 * @return A std::string pointer.
 * @throw HKLException if the std::string is not present n the ParameterList.
 */
hkl::Parameter * ParameterList::operator[](const std::string & name) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00026B82
      vector<Parameter *>::iterator iter = _parameters.begin();
      vector<Parameter *>::iterator end = _parameters.end();
      while(iter != end)
        {
          if ( (*iter)->get_name() == name )
            return *iter;
          ++iter;
        }
      HKLEXCEPTION("Cannot find this parameter", "Check the name of the parameter.");
  // Bouml preserved body end 00026B82
}

/**
 * @brief Get an iterator on the first element of the ParameterList.
 * @return The iterator.
 */
hkl::ParameterList::iterator ParameterList::begin() 
{
  // Bouml preserved body begin 00026C02
      return _parameters.begin();
  // Bouml preserved body end 00026C02
}

/**
 * @brief Get an iterator on the end of the ParameterList.
 * @return The iterator.
 */
hkl::ParameterList::iterator ParameterList::end() 
{
  // Bouml preserved body begin 00026C82
      return _parameters.end();
  // Bouml preserved body end 00026C82
}

/**
 * @brief Get an const_iterator on the first element of the ParameterList.
 * @return The const_iterator.
 */
hkl::ParameterList::const_iterator ParameterList::begin() const 
{
  // Bouml preserved body begin 00026D02
      return _parameters.begin();
  // Bouml preserved body end 00026D02
}

/**
 * @brief Get an const_iterator on the end of the ParameterList.
 * @return The const_iterator.
 */
hkl::ParameterList::const_iterator ParameterList::end() const 
{
  // Bouml preserved body begin 00026D82
      return _parameters.end();
  // Bouml preserved body end 00026D82
}

/*!
 * \brief Are two ParameterList equals ?
 * \param parameterList the hkl::ParameterList to compare with.
 */

bool ParameterList::operator==(const hkl::ParameterList & parameterList) const 
{
  // Bouml preserved body begin 00026882
      if (_parameters.size() != parameterList._parameters.size())
        return false;
      else
        {
          vector<Parameter *>::const_iterator iter = _parameters.begin();
          vector<Parameter *>::const_iterator iter2 = parameterList._parameters.begin();
          vector<Parameter *>::const_iterator end = _parameters.end();
          while(iter != end)
            {
              if ( !(**iter == **iter2) )
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
  // Bouml preserved body end 00026882
}

/*!
 * \brief print the ParameterList into a flux
 * \param flux The stream to print into.
 */
ostream & ParameterList::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00026902
      vector<Parameter *>::const_iterator iter = _parameters.begin();
      vector<Parameter *>::const_iterator end = _parameters.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00026902
}

/*!
 * \brief Save the ParameterList into a stream.
 * \param flux the stream to save the ParameterList into.
 * \return The stream with the ParameterList.
 */
ostream & ParameterList::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00026982
      vector<Parameter *>::const_iterator iter = _parameters.begin();
      vector<Parameter *>::const_iterator end = _parameters.end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00026982
}

/*!
 * \brief Restore a ParameterList from a stream.
 * \param flux The stream containing the ParameterList to restore.
 * @todo call update_observers or not ?
 */
istream & ParameterList::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00026A02
      vector<Parameter *>::iterator iter = _parameters.begin();
      vector<Parameter *>::iterator end = _parameters.end();
      while(iter != end)
        {
          (*iter)->fromStream(flux);
          ++iter;
        }
      return flux;
  // Bouml preserved body end 00026A02
}


} // namespace hkl
