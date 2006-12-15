#ifndef _SAMPLELIST_H_
#define _SAMPLELIST_H_

#include "samplefactory.h"

using namespace std;

namespace hkl
  {

  class SampleList
    {
    public:
      SampleList(Geometry & geometry);
      SampleList(SampleList const & sampleList);
      ~SampleList(void);
      vector<SampleType> types(void) const;
      void add(MyString const & name, SampleType type) throw (HKLException);
      void erase(vector<Sample *>::iterator pos) throw (HKLException);
      void clear(void);
      unsigned int size(void) const;
      vector<Sample *>::iterator begin(void);
      vector<Sample *>::iterator end(void);
      Sample * operator[](unsigned int index) throw (HKLException);
      void set_current(unsigned int index) throw (HKLException);
      Sample * current(void) throw (HKLException);
      bool operator==(SampleList const & sampleList) const;
      ostream & printToStream(ostream & flux) const;
      ostream & toStream(ostream & flux) const;
      istream & fromStream(istream & flux);

    private:
      Sample * _current;
      vector<Sample *> _samples;
      Geometry & _geometry;
      SampleFactory * _samplefactory;
    };
} // namespace hkl

static ostream &
operator <<(ostream & flux, hkl::SampleList const & sampleList)
{
  return sampleList.printToStream(flux);
}

#endif // _SAMPLELIST_H_
