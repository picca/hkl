#ifndef _HKL_HOLDER_H
#define _HKL_HOLDER_H

#include <vector>
#include <string>

#include "HKLException.h"
#include "axis.h"

namespace hkl { class Holder; }

namespace hkl
{

	struct HolderRow {
		hkl_axis * axis;
		unsigned int idx;
	};

	class HolderList
	{
		protected:

			hkl_axes * _axes;

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
			inline hkl_axes * axes();

			/**
			 * @brief get the axes store in the holderList.
			 * @return all axes stored in all holders of the HolderList.
			 */
			inline hkl_axes const * axes() const;

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

	};
	/**
	 * @brief get the axes store in the holderList.
	 * @return all axes stored in all holders of the HolderList.
	 */
	inline hkl_axes * HolderList::axes()
	{
		return _axes;
	}

	/**
	 * @brief get the axes store in the holderList.
	 * @return all axes stored in all holders of the HolderList.
	 */
	inline hkl_axes const * HolderList::axes() const
	{
		return _axes;
	}

	/**
	 * @brief get an Holder by its index.
	 * @param idx the index of the holder to get.
	 */
	inline hkl::Holder const * HolderList::operator[](unsigned int idx) const
	{
		return _holders[idx];
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
			hkl_axis * add_rotation(const std::string & name, hkl_svector const * axe) throw(hkl::HKLException);

			/**
			 * @brief apply the holder transformation to a hkl::Quaternion.
			 * @return The q hkl::Quaternion after the transformation.
			 *
			 * It computes q * qi in the Holder.
			 */
			hkl_quaternion * apply(hkl_quaternion * q) const;

			/**
			 * @brief apply the holder consign transformation to a hkl::Quaternion.
			 * @return The q hkl::Quaternion after the transformation.
			 *
			 * It computes q * qi(consign) in the Holder.
			 */
			hkl_quaternion * apply_consign(hkl_quaternion * q) const;

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
