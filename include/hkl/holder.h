/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2007 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef _HKL_HOLDER_H
#define _HKL_HOLDER_H

#include <vector>
#include <ostream>
#include <istream>
#include <string>
#include "axefactory.h"
#include "axe.h"
#include "HKLException.h"

namespace hkl
  {
  class Axe;
}
namespace hkl
  {
  namespace axe
    {
    class Rotation;
  }
}
namespace hkl
  {
  class svector;
}
namespace hkl
  {
  class Quaternion;
}

namespace hkl
  {
  class Holder;
}
namespace hkl
  {

  struct HolderRow
    {
      Axe * axe;
      unsigned int idx;
    };

  class HolderList
    {
    protected:
      static hkl::AxeFactory _axeFactory;

      hkl::AxeList _axes;

      std::vector<hkl::Holder *> _holders;


    public:
      /**
       * @brief Create an empty holderList.
       */
      HolderList();

      virtual ~HolderList();

      HolderList(const HolderList & source);

      /**
       * @brief add an holder to the HolderList
       * @return The added Holder
       */
      hkl::Holder * add();

      /**
       * @brief get the axes store in the holderList.
       * @return all axes stored in all holders of the HolderList.
       */
      inline hkl::AxeList & axes();

      /**
       * @brief get the axes store in the holderList.
       * @return all axes stored in all holders of the HolderList.
       */
      inline hkl::AxeList const & axes() const;

      /**
       * @brief Get the size of the HolderList
       * @return The number of Holder in the HolderList
       */
      unsigned int size() const;

      /**
       * @brief get an Holder by its index.
       * @param idx the index of the holder to get.
       */
      inline hkl::Holder const * operator[](unsigned int idx) const;

      /**
       * @brief Are two HolderList equals ?
       * @param holderList the HolderList to compare with.
       */
      bool operator==(const HolderList & holderList) const;

      /**
       * @brief print the HolderList into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      virtual std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the HolderList
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the HolderList from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);

    };
  /**
   * @brief get the axes store in the holderList.
   * @return all axes stored in all holders of the HolderList.
   */
  inline hkl::AxeList & HolderList::axes()
  {
    // Bouml preserved body begin 0003CB02
    return _axes;
    // Bouml preserved body end 0003CB02
  }

  /**
   * @brief get the axes store in the holderList.
   * @return all axes stored in all holders of the HolderList.
   */
  inline hkl::AxeList const & HolderList::axes() const
    {
      // Bouml preserved body begin 0003CB82
      return _axes;
      // Bouml preserved body end 0003CB82
    }

  /**
   * @brief get an Holder by its index.
   * @param idx the index of the holder to get.
   */
  inline hkl::Holder const * HolderList::operator[](unsigned int idx) const
    {
      // Bouml preserved body begin 0003CC02
      return _holders[idx];
      // Bouml preserved body end 0003CC02
    }

  class Holder
    {
    protected:
      std::vector<hkl::HolderRow> _rows;

      hkl::HolderList * _holderList;


    public:
      /**
       * @brief construct an Holder related to an AxeList.
       */
      Holder(hkl::HolderList * holderList);

      /**
       * @brief Add an axe to the holder.
       * @param name The name of the added Axe.
       * @param axe The hkl::svector representing the axe of rotation.
       * @return The added axe.
       */
      hkl::axe::Rotation * add_rotation(const std::string & name, const hkl::svector & axe) throw(hkl::HKLException);

      /**
       * @brief apply the holder transformation to a hkl::Quaternion.
       * @return The q hkl::Quaternion after the transformation.
       *
       * It computes q * qi in the Holder.
       */
      hkl::Quaternion & apply(hkl::Quaternion & q) const;

      /**
       * @brief apply the holder consign transformation to a hkl::Quaternion.
       * @return The q hkl::Quaternion after the transformation.
       *
       * It computes q * qi(consign) in the Holder.
       */
      hkl::Quaternion & apply_consign(hkl::Quaternion & q) const;

      /**
       * @brief set the axeList of the Holder.
       * @param holderList The hkl::HolderList to set.
       * @throw HKLException if the hkl::HolderList is not compatible.
       */
      void set_holderList(hkl::HolderList * holderList) throw(hkl::HKLException);

      /**
       * @brief Are two Holder equals ?
       * @param holder the Holder to compare with.
       */
      bool operator==(const Holder & holder) const;

      /**
       * @brief print the Holder into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the Holder
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the Holder from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);


    protected:
      /**
       * @brief Add an axe to the holder.
       * @param name The name of the added Axe.
       * @param axe The hkl::svector representing the axe of rotation.
       * @return The added axe.
       */
      template<typename T>
      T * add(T * axe) throw(hkl::HKLException)
      {
        std::string const & name = axe->get_name();

        // Is the axe in the axeList ?
        hkl::AxeList::iterator iter = _holderList->axes().begin();
        hkl::AxeList::iterator end = _holderList->axes().end();
        bool found_in_axeList = false;
        unsigned int idx = 0;
        while (iter != end && !found_in_axeList )
          {
            if ( (*iter)->get_name() == name) // same name -> check if axes are compatible
              {
                if ( **iter == *axe) // same axe -> check if axe in the holder ( in _axes)
                  {
                    std::vector<hkl::HolderRow>::iterator it = _rows.begin();
                    std::vector<hkl::HolderRow>::iterator it_end = _rows.end();
                    while (it != it_end)
                      {
                        if ( it->axe->get_name() == name) // yes -> exception
                          {
                            std::ostringstream description;
                            description << "The axe \"" << name << "\" is already present in the holder";
                            // destroy the axe as it will not be added to the axe List
                            delete axe;
                            HKLEXCEPTION("Can not add two times the same axe",
                                         description.str());
                          }
                        else // no -> add it
                          ++it;
                      }
                    // not in the holder -> add it and check for memory leak
                    hkl::HolderRow row = {NULL, idx};
                    if (*iter == axe) // same pointer -> only add to the _axes.
                      row.axe = axe;
                    else // different pointer -> keep the one from the holder.
                      {
                        row.axe = *iter;
                        delete axe;
                      }
                    _rows.push_back(row);
                    return static_cast<T *>(row.axe);
                  }
                else // different axe with the same name -> throw exception
                  {
                    std::ostringstream description;
                    description << "Same name but different axe." << std::endl
                    << "holder axe : " << **iter;
                    description << "Axe to add : " << *axe;
                    // destroy tha ase as it will not be added to the axe list.
                    delete axe;
                    HKLEXCEPTION("Can not add this Axe to the sample axe list",
                                 description.str());
                  }
              }
            else // not same name -> next axe in the axeList
              {
                ++idx; // compute the index of the next axe in the _axeList.
                ++iter;
              }
          }
        // Axe not present in the axeList so add it to the axeList and the _axes.
        hkl::HolderRow row = { axe, _holderList->axes().size() };
        _holderList->axes().push_back(axe);
        _rows.push_back(row);
        return static_cast<T *>(row.axe);
      }

    };

} // namespace hkl
/**
 * @brief Overload of the << operator for the Holder class
 * @param flux
 * @param holder
 * @return the modified flux.
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::Holder const & holder)
{
  return holder.printToStream(flux);
}

/**
 * @brief Overload of the << operator for the HolderList class
 * @param flux
 * @param holders
 * @return the modified flux.
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::HolderList const & holders)
{
  return holders.printToStream(flux);
}
#endif
