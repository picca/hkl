#include "fitparameterlist.h"

namespace hkl {

  FitParameterList::FitParameterList()
    : MyVector<FitParameter>()
    {}

  FitParameterList::FitParameterList(FitParameterList const & fitParameterList)
    : MyVector<FitParameter>(fitParameterList)
    {}

  FitParameterList::~FitParameterList()
  {}

  FitParameterList &
    FitParameterList::operator +=(FitParameterList const & fitParameterList)
    {
      FitParameterList::iterator iter = begin();
      FitParameterList::iterator last = end();

      FitParameterList::const_iterator iter2 = fitParameterList.begin();

      while(iter != last){
        *iter += *iter2;
        ++iter;
        ++iter2;
      }
      return *this;
    }

  FitParameterList &
    FitParameterList::operator -=(FitParameterList const & fitParameterList)
    {
      FitParameterList::iterator iter = begin();
      FitParameterList::iterator last = end();

      FitParameterList::const_iterator iter2 = fitParameterList.begin();

      while(iter != last){
        *iter -= *iter2;
        ++iter;
        ++iter2;
      }
      return *this;
    }

  FitParameterList &
    FitParameterList::operator *=(double const & d)
    {
      FitParameterList::iterator iter = begin();
      FitParameterList::iterator last = end();

      while(iter != last){
        *iter *= d;
        ++iter;
      }
      return *this;
    }

  FitParameterList &
    FitParameterList::operator /=(double const & d)
    {
      FitParameterList::iterator iter = begin();
      FitParameterList::iterator last = end();

      while(iter != last){
        *iter /= d;
        ++iter;
      }
      return *this;
    }

  unsigned int
    FitParameterList::getNumberOfParameterToFit() const
    { 
      FitParameterList::const_iterator iter = begin();
      FitParameterList::const_iterator last = end();

      unsigned int n = 0;
      while(iter != last){
        if (iter->get_flagFit())
          n++;
        ++iter;
      }
      return n;
    }

  void
    FitParameterList::randomize(void)
    {
      FitParameterList::iterator iter = begin();
      FitParameterList::iterator last = end();

      while(iter != last){
        iter->randomize();
        ++iter;
      }
    }
} // namespace hkl

  std::ostream &
operator <<(std::ostream & flux, hkl::FitParameterList const & fitParameterList)
{
  return fitParameterList.printToStream(flux);
}
