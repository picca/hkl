#include "config.h"
#include "reflection.h"

namespace hkl
{

	/**
	 * @brief Create a Reflection.
	 *
	 * @param geometry The hkl::Geometry of the reflection
	 * @param hkl The hkl scattering vactor.
	 * @param flag if the reflection must be use during calculation.
	 * @throw HKLException if the geometry is not valid.
	 */

	Reflection::Reflection(const hkl::Geometry & geometry, hkl_svector const * hkl, bool flag) :
		_geometry(geometry),
		_flag(flag)
	{
		_hkl = *hkl;
	}

	Reflection::~Reflection()
	{
	}

	/**
	 * @brief Set the hkl scattering vector store in the Reflection.
	 * @param hkl The scattering vector in the crystal coordinates to store in the Reflection.
	 */

	void Reflection::set_hkl(hkl_svector const * v)
	{
		_hkl = *v;
	}

	/**
	 * @brief Get a constant reference on the flag store in the Reflection.
	 * @return The flag of the Reflection.
	 *
	 * the flag is true when we use the reflection in the affinement, false otherwise.
	 */

	const bool & Reflection::flag() const
	{
		return _flag;
	}

	/**
	 * @brief Get a constant reference on the flag store in the Reflection.
	 * @return The flag of the Reflection.
	 *
	 * the flag is true when we use the reflection in the affinement, false otherwise.
	 */

	bool & Reflection::flag()
	{
		return _flag;
	}

	/**
	 * @brief compute the theoretical angle beetween two hkl vectors.
	 * @param hkl The second scattering vector to compare with the Reflection internal hkl.
	 * @return the angle between the two hkl.
	 * @todo Maybe move this in the Sample and add a computeAngle(Reflection const & reflection)
	 * @todo add the mathematical formula.
	 */

	hkl::Value Reflection::computeAngle(hkl_svector const * hkl) const
	{
		return Value(::hkl_svector_angle(&_hkl, hkl));
	}

	/**
	 * @brief Check if two reflections are colinear.
	 * @param reflection The reflection to compare with.
	 * @return true if the reflections are colinear, false otherwise.
	 * @todo Add the mathematical formula.
	 */

	bool Reflection::isColinear(const hkl::Reflection & reflection) const
	{
		return ::hkl_svector_is_colinear(&_hkl, &reflection._hkl);
	}

	/**
	 * \brief Are two Reflection equals ?
	 * \param reflection the hkl::Reflection to compare with.
	 * \return true if both are equals flase otherwise.
	 */
	bool Reflection::operator==(const hkl::Reflection & reflection) const
	{
		return _geometry == reflection._geometry
			&& ::hkl_svector_cmp(&_hkl, &reflection._hkl)
			&& _flag == reflection._flag
			&& ::hkl_svector_cmp(&_hkl_phi, &reflection._hkl_phi);
	}

	/**
	 * @brief print the Reflection into a flux
	 * @param flux The stream to print into.
	 * @return The modified flux.
	 */
	std::ostream & Reflection::printToStream(std::ostream & flux) const
	{
		/*
		hkl::AxeList const & axes = _geometry.get_axes();

		for (unsigned int i=0; i<axes.size(); i++)
		{
			flux.width(9);
			flux << axes[i]->get_current().get_value() * HKL_RADTODEG;
		}
		*/
		flux << " |";
		flux.width(9);
		flux << _geometry.source.wave_length;
		flux << " | " << "(" << _flag;

		return flux;
	}

} // namespace hkl
