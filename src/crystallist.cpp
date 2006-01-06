#include "crystallist.h"

namespace hkl {

  CrystalList::CrystalList(void)
    : MyMap<Crystal>()
  {
    MyMap<Crystal>::add(Crystal(DEFAULT_CRYSTAL_NAME));
  }

  CrystalList::CrystalList(CrystalList const & crystalList)
    : MyMap<Crystal>(crystalList)
  {}

  CrystalList::~CrystalList(void)
  {}
  
  void
  CrystalList::remove(string const & name) throw (HKLException)
  {
    MyMap<Crystal>::remove(name);
    if (MyMap<Crystal>::size() == 0)
      MyMap<Crystal>::add(Crystal(DEFAULT_CRYSTAL_NAME));
  }
  
  void
  CrystalList::clear(void)
  {
    MyMap<Crystal>::clear();
    MyMap<Crystal>::add(Crystal(DEFAULT_CRYSTAL_NAME));
  }
  
} // namespace hkl
