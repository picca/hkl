#include "parameterlist.h"

namespace hkl
  {

  ParameterList::~ParameterList(void)
  {}

  bool
  ParameterList::operator==(ParameterList const & parameterList) const
    {
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
    }

  ostream &
  ParameterList::printToStream(ostream & flux) const
    {
      vector<Parameter *>::const_iterator iter = _parameters.begin();
      vector<Parameter *>::const_iterator end = _parameters.end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

  ostream &
  ParameterList::toStream(ostream & flux) const
    {
      vector<Parameter *>::const_iterator iter = _parameters.begin();
      vector<Parameter *>::const_iterator end = _parameters.end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  istream &
  ParameterList::fromStream(istream & flux)
  {
    vector<Parameter *>::iterator iter = _parameters.begin();
    vector<Parameter *>::iterator end = _parameters.end();
    while(iter != end)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }

  bool
  ParameterList::add(Parameter * parameter)
    {
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
    }

  unsigned int
  ParameterList::size(void) const
    {
      return _parameters.size();
    }

} // namespace hkl
