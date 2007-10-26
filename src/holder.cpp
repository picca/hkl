#include "config.h"
#include "holder.h"
#include "axis.h"
#include "svector.h"
#include "quaternion.h"

namespace hkl
{

	/**
	 * @brief Create an empty holderList.
	 */
	HolderList::HolderList()
	{
		hkl_axes_init(_axes, 0);
	}

	HolderList::~HolderList()
	{
		unsigned int i;

		hkl_axes_release(_axes);

		// remove all holders
		for (i=0;i<_holders.size();i++)
			delete _holders[i];
	}

	HolderList::HolderList(const hkl::HolderList & holderList) :
		_holders(holderList._holders)
	{
		// need to do a deep copy of the source._axes.
		_axes = hkl_axes_copy(holderList._axes);

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
		/*
		for (i=0;i<_axes->len;++i)
			flux << "  " << _axes[i] << " " << *_axes[i] << std::endl;
		*/
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
	hkl_axis * Holder::add_rotation(const std::string & name, hkl_svector const * axe) throw(hkl::HKLException)
	{
		size_t i;
		size_t idx;
		struct hkl_axis * axis = NULL;

		idx = hkl_axis_get_idx_by_name(_holderList->_axes, name);
		if (idx >= 0) { // found it in the axe list
			// check that both have the same rotation axe.
			if (hkl_svector_cmp(&holder->axes->axes[idx].axis, rot_axis)) {
				// check that the axis is not already in the holder
				for(i=0; i<holder->len; i++)
					if (idx == holder->idx[i])
						die("can not add two times the \"%s\" axis to an holder.", name);

				// add 1 element to ther holder
				hkl_holder_add_idx(holder, idx);
			} else
				die("can not add two axis with the same name \"%s\" but different axes <%f, %f, %f> != <%f, %f, %f> into an holder",
						name,
						axis->axis.data[X], axis->axis.data[Y], axis->axis.data[Z],
						rot_axis->data[X], rot_axis->data[Y], rot_axis->data[Z]);
		} else {// not already in the axe list
			// add a rotation axe to the holder->axes list
			hkl_axes_add_rotation(holder->axes, name, rot_axis);
			// set the right index in the holder idx list
			hkl_holder_add_idx(holder, holder->axes->len);
		}
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
