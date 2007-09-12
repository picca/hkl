
#include "fitparameterlist.h"
#include "fitparameter.h"

namespace hkl
  {

  FitParameterList::~FitParameterList()
  {
  }

  /**
   * @brief Get the size of the FitParameterList.
   * @return the number of element in the FitParameterList.
   */
  unsigned int FitParameterList::size() const
    {
      return _parameters.size();
    }

  /**
   * @brief Get the number of Parameter to fit in the FitParameterList.
   * @return The number of Parameter with the fitFlag set to true.
   */
  unsigned int FitParameterList::size_to_fit() const
    {
      std::vector<FitParameter *>::const_iterator iter = _parameters.begin();
      std::vector<FitParameter *>::const_iterator end = _parameters.end();

      unsigned int n = 0;
      while (iter != end)
        {
          if ((*iter)->get_flagFit())
            n++;
          ++iter;
        }
      return n;
    }

  /**
   * @return the std::string * named
   * @param name The name of the std::string we are looking for in the FitParameterList.
   * @return A std::string pointer.
   * @throw HKLException if the std::string is not present n the FitParameterList.
   */
  hkl::FitParameter * FitParameterList::operator[](const std::string & name) throw(hkl::HKLException)
  {
    std::vector<FitParameter *>::iterator iter = _parameters.begin();
    std::vector<FitParameter *>::iterator end = _parameters.end();

    while (iter != end)
      {
        if (name == (*iter)->get_name())
          return *iter;
        ++iter;
      }
    HKLEXCEPTION("can not find the parameter","");
  }

  /**
   * @brief Get an iterator on the first element of the FitParameterList.
   * @return The iterator.
   */
  hkl::FitParameterList::iterator FitParameterList::begin()
  {
    return _parameters.begin();
  }

  /**
   * @brief Get an iterator on the end of the FitParameterList.
   * @return The iterator.
   */
  hkl::FitParameterList::iterator FitParameterList::end()
  {
    return _parameters.end();
  }

  /**
   * @brief Get an const_iterator on the first element of the FitParameterList.
   * @return The const_iterator.
   */
  hkl::FitParameterList::const_iterator FitParameterList::begin() const
    {
      return _parameters.begin();
    }

  /**
   * @brief Get an const_iterator on the end of the FitParameterList.
   * @return The const_iterator.
   */
  hkl::FitParameterList::const_iterator FitParameterList::end() const
    {
      return _parameters.end();
    }

  /*!
   * \brief Are two FitParameterList equals ?
   * \param fitParameterList the hkl::FitParameterList to compare with.
   */

  bool FitParameterList::operator==(const hkl::FitParameterList & fitParameterList) const
    {
      if (_parameters.size() != fitParameterList._parameters.size())
        return false;
      else
        {
          std::vector<FitParameter *>::const_iterator iter = _parameters.begin();
          std::vector<FitParameter *>::const_iterator iter2 = fitParameterList._parameters.begin();
          std::vector<FitParameter *>::const_iterator end = _parameters.end();
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

  /*!
   * \brief print the FitParameterList into a flux
   * \param flux The stream to print into.
   */
  std::ostream & FitParameterList::printToStream(std::ostream & flux) const
    {
      std::vector<FitParameter *>::const_iterator iter = _parameters.begin();
      std::vector<FitParameter *>::const_iterator end = _parameters.end();
      while (iter != end)
        {
          (*iter)->printToStream(flux);
          flux << std::endl;
          ++iter;
        }
      return flux;
    }

  /*!
   * \brief Save the FitParameterList into a stream.
   * \param flux the stream to save the FitParameterList into.
   * \return The stream with the FitParameterList.
   */
  std::ostream & FitParameterList::toStream(std::ostream & flux) const
    {
      std::vector<FitParameter *>::const_iterator iter = _parameters.begin();
      std::vector<FitParameter *>::const_iterator end = _parameters.end();
      while (iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  /*!
   * \brief Restore a FitParameterList from a stream.
   * \param flux The stream containing the FitParameterList to restore.
   * @todo call update_observers or not ?
   */
  std::istream & FitParameterList::fromStream(std::istream & flux)
  {
    std::vector<FitParameter *>::iterator iter = _parameters.begin();
    std::vector<FitParameter *>::iterator end = _parameters.end();
    while (iter != end)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }


} // namespace hkl
