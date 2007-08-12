
#include "observer.h"

namespace hkl {

Observer::Observer() :
  _connected(false)
{
  // Bouml preserved body begin 00024382
  // Bouml preserved body end 00024382
}

Observer::~Observer() 
{
  // Bouml preserved body begin 00034002
  // Bouml preserved body end 00034002
}

void Observer::connect() 
{
  // Bouml preserved body begin 00024482
      _connected = true;
  // Bouml preserved body end 00024482
}

void Observer::unconnect() 
{
  // Bouml preserved body begin 00024502
      _connected = false;
  // Bouml preserved body end 00024502
}

/**
 * @brief The default constructor.
 */
Observable::Observable() :
  _changed(false)
{
  // Bouml preserved body begin 00024582
  // Bouml preserved body end 00024582
}

/**
 * @brief Add an hkl::Observer to the Observable.
 * @param observer The hkl::Observer pointer to add.
 */
void Observable::add_observer(hkl::Observer * observer) 
{
  // Bouml preserved body begin 00024602
      _observers.push_back(observer);
  // Bouml preserved body end 00024602
}

/**
 * @brief Delete an hkl::Observer from the Observable.
 * @param observer The hkl::Observer pointer to remove.
 */
void Observable::del_observer(hkl::Observer * observer) 
{
  // Bouml preserved body begin 00024682
      std::vector<Observer *>::iterator iter = _observers.begin();
      std::vector<Observer *>::iterator end = _observers.end();
      while(iter != end)
      {
        if (*iter == observer)
          {
            _observers.erase(iter);
            break;
          }
        ++iter;
      }
  // Bouml preserved body end 00024682
}

/**
 * @brief Update all the Observer looking for this Observable.
 */
void Observable::update_observers() 
{
  // Bouml preserved body begin 00024702
      if (_changed)
        {
          std::vector<Observer *>::iterator iter = _observers.begin();
          std::vector<Observer *>::iterator end = _observers.end();
          while(iter != end)
            {
              (*iter)->update();
              ++iter;
            }
          _changed = false;
        }
  // Bouml preserved body end 00024702
}

/**
 * @brief Set the changed state of the Observable.
 *
 * If the changed state is not on, no Observer update is possible.
 */
void Observable::set_changed() 
{
  // Bouml preserved body begin 00024782
      _changed = true;
  // Bouml preserved body end 00024782
}


} // namespace hkl
