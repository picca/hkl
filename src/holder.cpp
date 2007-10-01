
#include "holder.h"
#include "axe.h"
#include "axe_rotation.h"
#include "svector.h"
#include "quaternion.h"

namespace hkl
  {

  hkl::AxeFactory HolderList::_axeFactory;

  /**
   * @brief Create an empty holderList.
   */
  HolderList::HolderList()
  {
  }

  HolderList::~HolderList()
  {
    unsigned int i;
    // remove all axes
    for (i=0;i<_axes.size();i++)
      delete _axes[i];

    // remove all holders
    for (i=0;i<_holders.size();i++)
      delete _holders[i];
  }

  HolderList::HolderList(const hkl::HolderList & source) :
      _holders(source._holders)
  {
    // need to do a deep copy of the source._axes.
    hkl::AxeList::const_iterator iter = source._axes.begin();
    hkl::AxeList::const_iterator end = source._axes.end();
    while (iter != end)
      {
        _axes.push_back((*iter)->clone());
        ++iter;
      }

    // make a deep copy of the holders and update the axelist due to the deep copy.
    for (unsigned int i=0;i<_holders.size();i++)
      {
        _holders[i] = new hkl::Holder(*_holders[i]);
        _holders[i]->set_holderList(this);
      }
  }

  /**
   * @brief add an holder to the HolderList
   * @return The added Holder
   */
  hkl::Holder * HolderList::add()
  {
    hkl::Holder * holder = new hkl::Holder(this);
    _holders.push_back(holder);

    return holder;
  }

  /**
   * @brief Get the size of the HolderList
   * @return The number of Holder in the HolderList
   */
  unsigned int HolderList::size() const
    {
      return _holders.size();
    }

  /**
   * @brief Are two HolderList equals ?
   * @param holderList the hkl::HolderList to compare with.
   */
  bool HolderList::operator==(const hkl::HolderList & holderList) const
    {
      if (_axes == holderList._axes)
        {
          if (_holders.size() == holderList._holders.size())
            {
              std::vector<hkl::Holder *>::const_iterator iter = _holders.begin();
              std::vector<hkl::Holder *>::const_iterator iter2 = holderList._holders.begin();
              std::vector<hkl::Holder *>::const_iterator end = _holders.end();
              while (iter != end)
                {
                  if ( !(**iter == **iter2) )
                    return false;
                  ++iter;
                  ++iter2;
                }
              return true;
            }
        }
      return false;
    }

  /**
   * @brief print the HolderList into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & HolderList::printToStream(std::ostream & flux) const
    {
      unsigned int i;

      flux << "HolderList with " << _holders.size() << " Holder(s)" << std::endl;
      flux << "AxeList : " << &_axes << std::endl;
      for (i=0;i<_axes.size();++i)
        flux << "  " << _axes[i] << " " << *_axes[i] << std::endl;
      flux << std::endl;
      // now the holders
      for (i=0;i<_holders.size();++i)
        flux << "(" << i << ") " << *_holders[i] << std::endl;
      return flux;
    }

  /**
   * @brief construct an Holder related to an AxeList.
   */
  Holder::Holder(hkl::HolderList * holderList) :
      _holderList(holderList)
  {
  }

  /**
   * @brief Add an axe to the holder.
   * @param name The name of the added Axe.
   * @param axe The hkl::svector representing the axe of rotation.
   * @return The added axe.
   */
  hkl::axe::Rotation * Holder::add_rotation(const std::string & name, hkl_svector const * axe) throw(hkl::HKLException)
  {
    hkl::axe::Rotation * rotation = new hkl::axe::Rotation(name, "rotation", -2*M_PI, 0, 2*M_PI, axe);
    this->add(rotation);

    return rotation;
  }

  /**
   * @brief apply the holder transformation to a hkl::Quaternion.
   * @return The q hkl::Quaternion after the transformation.
   *
   * It computes q * qi in the Holder.
   */
  hkl_quaternion * Holder::apply(hkl_quaternion * q) const
    {
      unsigned int i;

      for (i=0;i<_rows.size();i++)
        _rows[i].axe->apply(q);

      return q;
    }

  /**
   * @brief apply the holder consign transformation to a hkl::Quaternion.
   * @return The q hkl::Quaternion after the transformation.
   *
   * It computes q * qi(consign) in the Holder.
   */
  hkl_quaternion * Holder::apply_consign(hkl_quaternion * q) const
    {
      unsigned int i;

      for (i=0;i<_rows.size();i++)
        _rows[i].axe->apply_consign(q);

      return q;
    }

  /**
   * @brief set the axeList of the Holder.
   * @param holderList The hkl::HolderList to set.
   * @throw HKLException if the hkl::HolderList is not compatible.
   */
  void Holder::set_holderList(hkl::HolderList * holderList) throw(hkl::HKLException)
  {
    _holderList = holderList;
    std::vector<hkl::HolderRow>::iterator iter = _rows.begin();
    std::vector<hkl::HolderRow>::iterator end = _rows.end();
    while (iter != end)
      {
        iter->axe = _holderList->axes()[iter->idx];
        ++iter;
      }
  }

  /**
   * @brief Are two Holder equals ?
   * @param holder the hkl::Holder to compare with.
   */
  bool Holder::operator==(const hkl::Holder & holder) const
    {
      if (_rows.size() == holder._rows.size())
        {
          std::vector<hkl::HolderRow>::const_iterator iter = _rows.begin();
          std::vector<hkl::HolderRow>::const_iterator iter2 = holder._rows.begin();
          std::vector<hkl::HolderRow>::const_iterator end = _rows.end();
          while (iter != end)
            {
              if ( iter->idx != iter2->idx)
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
      return false;
    }

  /**
   * @brief print the Holder into a flux
   * @param flux The stream to print into.
   * @return The modified flux.
   */
  std::ostream & Holder::printToStream(std::ostream & flux) const
    {
      flux << "holder: (" << _rows.size() << ") Axe List related : " << &_holderList->axes() << std::endl;
      std::vector<hkl::HolderRow>::const_iterator iter = _rows.begin();
      std::vector<hkl::HolderRow>::const_iterator end = _rows.end();
      while (iter != end)
        {
          flux << "  (" << iter->axe << ", " << iter->idx << ") "
          << *(iter->axe) << std::endl;
          ++iter;
        }

      return flux;
    }

} // namespace hkl
