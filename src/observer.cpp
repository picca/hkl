#include "observer.h"

namespace hkl
  {

  class Axe;

  Observer::Observer(void) :
      _connected(false)
  {}

  void
  Observer::connect(void)
  {
    _connected = true;
  }

  void
  Observer::unconnect(void)
  {
    _connected = false;
  }

  Observable::Observable(void) :
      _changed(false)
  {}

  void
  Observable::add_observer(Observer * observer)
  {
    _observers.push_back(observer);
  }

  void
  Observable::delete_observer(Observer * observer)
  {
    vector<Observer *>::iterator iter = _observers.begin();
    vector<Observer *>::iterator end = _observers.end();
    while(iter != end)
      {
        if (*iter == observer)
          {
            _observers.erase(iter);
            break;
          }
        ++iter;
      }
  }

  void Observable::update_observers(void)
  {
    if (_changed)
      {
        vector<Observer *>::iterator iter = _observers.begin();
        vector<Observer *>::iterator end = _observers.end();
        while(iter != end)
          {
            (*iter)->update();
            ++iter;
          }
        _changed = false;
      }
  }

  void Observable::set_changed(void)
  {
    _changed = true;
  }

} // namespace hkl
