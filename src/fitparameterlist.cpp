#include "fitparameterlist.h"

namespace hkl {

    FitParameterList::FitParameterList(FitParameterList const & fitParameterList)
      {
        vector<FitParameter *>::const_iterator iter = fitParameterList._parameters.begin();
        vector<FitParameter *>::const_iterator end = fitParameterList._parameters.end();
        while(iter != end)
          {
            _parameters.push_back(new FitParameter(**iter));
            ++iter;
          }
      }

    bool
    FitParameterList::operator ==(FitParameterList const & fitParameterList) const
      {
        if (_parameters.size() != fitParameterList._parameters.size())
            return false;
        else
          {
            vector<FitParameter *>::const_iterator iter = _parameters.begin();
            vector<FitParameter *>::const_iterator end = _parameters.end();
            vector<FitParameter *>::const_iterator iter2 = fitParameterList._parameters.begin();
            while(iter != end)
              {
                if (!(**iter == **iter2))
                    return false;
                ++iter;
                ++iter2;
              }
          }
        return true;
      }

    ostream &
    FitParameterList::printToStream(ostream & flux) const
      {
        vector<FitParameter *>::const_iterator iter = _parameters.begin();
        vector<FitParameter *>::const_iterator end = _parameters.end();
        while(iter != end)
          {
            (*iter)->printToStream(flux);
            ++iter;
          }
        return flux;
      }

    ostream &
    FitParameterList::toStream(ostream & flux) const
      {
        vector<FitParameter *>::const_iterator iter = _parameters.begin();
        vector<FitParameter *>::const_iterator end = _parameters.end();
        while(iter != end)
          {
            (*iter)->toStream(flux);
            ++iter;
          }
        return flux;
      }

    istream &
    FitParameterList::fromStream(istream & flux)
      {
        vector<FitParameter *>::iterator iter = _parameters.begin();
        vector<FitParameter *>::iterator end = _parameters.end();
        while(iter != end)
          {
            (*iter)->fromStream(flux);
            ++iter;
          }
        return flux;
      }

    unsigned int
    FitParameterList::size(void) const
      {
        return _parameters.size();
      }

    unsigned int
    FitParameterList::size_to_fit(void) const
      {
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
      }

} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::FitParameterList const & fitParameterList)
{
    return fitParameterList.printToStream(flux);
}
