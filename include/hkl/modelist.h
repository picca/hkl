#ifndef _MODELIST_H
#define _MODELIST_H


#include <vector>
#include <string>
#include <ostream>
#include <istream>

namespace hkl { class Mode; } 

namespace hkl {

class ModeList {
  protected:
    hkl::Mode * _current;

    std::vector<hkl::Mode *> _modes;


  public:
    typedef std::vector<Mode *>::iterator iterator;

    typedef std::vector<Mode *>::const_iterator const_iterator;

    /**
     * @brief Default constructor of the ModeList class.
     */
    
    ModeList();

    /**
     * @brief The default destructor.
     */
    
    virtual ~ModeList();

    /**
     * @brief Add a mode to the ModeList.
     * @param mode The hkl::Mode to add.
     * @return NULL if the hkl::Mode can not be add or a Pointer on the added hkl::Mode
     */
    hkl::Mode * add(hkl::Mode * mode);

    /**
     * @brief Remove a Mode from the ModeList.
     * @param pos The ModeList::iterator position of the Sample.
     * @throw HKLException If the sample is not present. 
     */
    ModeList::iterator erase(ModeList::iterator & pos);

    /**
     * @brief Remove all sample from the SampleList.
     */
    void clear();

    /**
     * @brief Set the nth Mode as the current Mode.
     * @param name The name of the Mode to set as current.
     * @return NULL if the mode is not present in the list but do not change the _current.
     */
    hkl::Mode * set_current(const std::string & name);

    /**
     * @brief Get the current Mode
     * @return A pointer on the current Mode.
     */
    hkl::Mode * get_current() const;

    /**
     * @brief Get the current sample
     * @return A pointer on the current sample.
     */
    hkl::Mode * current();

    /**
     * @brief Return the names of all samples.
     */
    
    std::vector<std::string> get_names() const;

    unsigned int size() const;

    /**
     * @return the Mode * named
     * @param name The name of the Mode we are looking for in the ModeList.
     * @return The mode or NULL if the mode is not present in the ModeList.
     */
    hkl::Mode * operator[](const std::string & name);

    /**
     * @brief Get an iterator on the first element of ReflectionList.
     * @return The iterator.
     */
    
    ModeList::iterator begin();

    /**
     * @brief Get an iterator on the end of ReflectionList.
     * @return The iterator.
     */
    
    ModeList::iterator end();

    /**
     * @brief Get an iterator on the first element of ReflectionList.
     * @return The iterator.
     */
    
    ModeList::const_iterator begin() const;

    /**
     * @brief Get an iterator on the end of ReflectionList.
     * @return The iterator.
     */
    
    ModeList::const_iterator end() const;

    /**
     * \brief Are two ModeList equals ?
     * \param modeList the ModeList to compare with.
     * \return true if both are equals flase otherwise.
     */
    bool operator==(const ModeList & modeList) const;

    /**
     * @brief print the ModeList into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    std::ostream & printToStream(std::ostream & flux) const;

    /**
     * @brief print on a stream the content of the ModeList
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    std::ostream & toStream(std::ostream & flux) const;

    /**
     * @brief restore the content of the ModeList from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    std::istream & fromStream(std::istream & flux);

};

} // namespace hkl
inline std::ostream &
operator <<(std::ostream & flux, hkl::ModeList const & modeList)
{
  return modeList.printToStream(flux);
}

#endif
