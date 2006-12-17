#ifndef _HKLOBJECT_H_
#define _HKLOBJECT_H_

#include "object.h"
#include "parameterlist.h"

using namespace std;

namespace hkl
{

/*!
 * \brief Class used to store a object with scalars parameters.
 */
class HKLObject : public ObjectReadOnly
{
public:

    /**
     * @brief The default constructor
     * @param name The name of the HKLObject.
     * @param description The description of the HKLObject.
     * @throw HKLException if the name and/or the description are wrong. 
     */
    HKLObject(MyString const & name, MyString const & description) throw (HKLException);

    /**
     * @brief The default destructor.
     */
    ~HKLObject(void);

    /*!
     * \brief get the ValueList of the HKLObject.
     * \return The ValueList of the HKLObject.
     */
    ParameterList & parameters(void)
    {
        return _parameters;
    }

    /**
     * @brief Are two HKLObject equals ?
     * @param hklObject the HKLObject to compare with.
     * @return True if both are equals, false otherwise.
     */
    bool operator ==(HKLObject const & hklObject) const;

    /**
     * @brief print the HKLObject into a flux
     * @param flux The stream to print into.
     * @return The modified stream.
     */
    ostream & printToStream(ostream & flux) const;

    /**
     * @brief Save the HKLObject into a stream.
     * @param flux the stream to save the HKLObject into.
     * @return The stream with the HKLObject.
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief Restore an HKLObject from a stream.
     * @param flux The stream containing the HKLObject.
     * @return The modified stream.
     */
    istream & fromStream(istream & flux);

protected:

    ParameterList _parameters; //!< values store in the object.
};

} // namespace hkl

inline ostream &
operator << (ostream & flux, hkl::HKLObject const & hklObject)
{
    return hklObject.printToStream(flux);
}

#endif // _HKLOBJECT_H_
