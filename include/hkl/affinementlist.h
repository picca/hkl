#ifndef _AFFINEMENTLIST_H_
#define _AFFINEMENTLIST_H_

#include "affinement.h"

using namespace std;

namespace hkl
  {

  class AffinementList : public vector<Affinement *>
    {
    public:

      /**
       * @brief The default destructor
       */
      virtual ~AffinementList(void);

      /**
       * @brief get the current affinement
       */
      Affinement * & current(void)
      {
        return _current;
      }

      /**
       * @brief Set the current affinement
       * @param name the name of the next current affinement
       */
      void set_current(MyString const & name) throw (HKLException);

      /**
       * @brief Add a affinement to the AffinementList.
       * @throw HKLException if the affinement is already present in the AffinementList.
       */
      void add(Affinement * affinement) throw (HKLException);

      /**
       * @brief Erase a affinement from the AffinementList.
       * @param pos An iterator on the Affinement to erase.
       * @throw HKLException if the iterator is not a valid iterator.
       */
      void erase(vector<Affinement *>::iterator pos) throw (HKLException);

      /**
       * @brief Clear the AffinementList
       *
       * remove all affinements from the AffinementList and release the Memory with a delete on each Affinement.
       */
      void clear(void);

      /**
       * @return the Affinement * named
       * @param name The name of the Affinement we are looking for in the AffinementList.
       * @return The affinement.
       * @throw HKLException if the Affinement is not present n the list.
       */
      Affinement * operator[](MyString const & name) throw (HKLException);

      /**
       * @brief Are two AffinementList equals ?
       * @param affinementList the AffinementList to compare with.
       * @return True if both are equals, false otherwise.
       */
      bool operator==(AffinementList const & affinementList) const;

      /**
       * @brief print the AffinementList into a flux
       * @param flux The stream to print into.
       * @return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Save the AffinementList into a stream.
       * @param flux the stream to save the AffinementList into.
       * @return The stream with the AffinementList.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * @brief Restore an AffinementList from a stream.
       * @param flux The stream containing the AffinementList.
       * @return The modified stream.
       */
      istream & fromStream(istream & flux);

    private:
      Affinement * _current; //!< A pointer use to select the current Affinement method.
    };

} // namespace hkl

inline ostream &
operator <<(ostream & flux, hkl::AffinementList const & affinementList)
{
  return affinementList.printToStream(flux);
}

#endif // _AFFINEMENTLIST_H_
