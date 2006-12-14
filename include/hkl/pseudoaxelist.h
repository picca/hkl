#ifndef _PSEUDOAXELIST_H_
#define _PSEUDOAXELIST_H_

#include "pseudoaxe.h"

using namespace std;

namespace hkl
  {

  class PseudoAxeList
    {
    public:
      ~PseudoAxeList(void);
      void add(PseudoAxe * pseudoAxe) throw (HKLException);
      void erase(vector<PseudoAxe *>::iterator pos) throw (HKLException);
      void clear(void);
      unsigned int size(void) const;
      vector<PseudoAxe *>::iterator begin(void);
      vector<PseudoAxe *>::iterator end(void);
      PseudoAxe * operator[](MyString const & name) throw (HKLException);
      bool operator==(PseudoAxeList const & pseudoAxeList) const;
      ostream & printToStream(ostream & flux) const;
      ostream & toStream(ostream & flux) const;
      istream & fromStream(istream & flux);

    private:
      vector<PseudoAxe *> _pseudoAxes;
    };

} // namespace hkl

static ostream &
operator <<(ostream & flux, hkl::PseudoAxeList const & pseudoAxeList)
{
  return pseudoAxeList.printToStream(flux);
}

#endif // _PSEUDOAXELIST_H_
