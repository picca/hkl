#ifndef _HKL_HOLDERLIST_H
#define _HKL_HOLDERLIST_H


#include "axe.h"
#include <vector>

namespace hkl
  {
  class Holder;
}

namespace hkl
  {

  class HolderList
    {
    protected:
      hkl::AxeList _axes;

      std::vector<hkl::Holder> _holders;


    public:
      HolderList();

      virtual ~HolderList();

      HolderList(const HolderList & source);

    };

} // namespace hkl
#endif
