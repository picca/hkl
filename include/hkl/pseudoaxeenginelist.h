#ifndef _PSEUDOAXEENGINELIST_H_
#define _PSEUDOAXEENGINELIST_H_

#include "pseudoaxeengine.h"
#include "pseudoaxelist.h"

using namespace std;

namespace hkl
  {

  class PseudoAxeEngineList : public vector<PseudoAxeEngine *>
    {
    public:
      /**
       * @brief The default destructor
       */
      ~PseudoAxeEngineList(void);

      PseudoAxeList & pseudoAxes(void) { return _pseudoAxes; }

      /**
       * @brief Add a pseudoAxeEngine to the PseudoAxeEngineList.
       * @throw HKLException if the pseudoAxeEngine is already present in the PseudoAxeEngineList.
       */
      void add(PseudoAxeEngine * pseudoAxeEngine) throw (HKLException);

      /**
       * @brief Clear the PseudoAxeEngineList
       *
       * remove all pseudoAxes from the PseudoAxeList and release the Memory with a delete on each PseudoAxe.
       */
      void clear(void);

      /**
       * @brief Are two PseudoAxeEngineList equals ?
       * @param pseudoAxeList the PseudoAxeEngineList to compare with.
       * @return True if both are equals, false otherwise.
       */
      bool operator==(PseudoAxeEngineList const & pseudoAxeEngineList) const;

      /**
       * @brief print the PseudoAxeEngineList into a flux
       * @param flux The stream to print into.
       * @return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Save the PseudoAxeEngineList into a stream.
       * @param flux the stream to save the PseudoAxeList into.
       * @return The stream with the PseudoAxeList.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * @brief Restore an PseudoAxeEngineList from a stream.
       * @param flux The stream containing the PseudoAxeList.
       * @return The modified stream.
       */
      istream & fromStream(istream & flux);

      protected:
        PseudoAxeList _pseudoAxes;
    };

} // namespace hkl

static ostream &
operator <<(ostream & flux, hkl::PseudoAxeEngineList const & pseudoAxeEngineList)
{
  return pseudoAxeEngineList.printToStream(flux);
}

#endif // _PSEUDOAXEENGINELIST_H_
