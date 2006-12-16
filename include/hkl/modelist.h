#ifndef _MODELIST_H_
#define _MODELIST_H_

#include "mode.h"

using namespace std;

namespace hkl
{

class ModeList
{
public:
    ~ModeList(void);
    void add(Mode * mode) throw (HKLException);
    void erase(vector<Mode *>::iterator pos) throw (HKLException);
    void clear(void);
    unsigned int size(void) const;
    vector<Mode *>::iterator begin(void);
    vector<Mode *>::iterator end(void);
    Mode * operator[](MyString const & name) throw (HKLException);
    bool operator==(ModeList const & modeList) const;
    ostream & printToStream(ostream & flux) const;
    ostream & toStream(ostream & flux) const;
    istream & fromStream(istream & flux);

private:
    vector<Mode *> _modes;
};

} // namespace hkl

static ostream &
operator <<(ostream & flux, hkl::ModeList const & modeList)
{
    return modeList.printToStream(flux);
}

#endif // _MODELIST_H_
