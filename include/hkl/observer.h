#ifndef _OBSERVER_H
#define _OBSERVER_H

#include <vector>

using namespace std;

namespace hkl
  {

  // forward declaration
  class Observable;

  /*!
   * \brief A class design to describe a rotation axe
   */
  class Observer
    {
    public:

      Observer(void);

      virtual void update(void) = 0;

      void connect(void);

      void unconnect(void);

    protected:
      bool _connected;

    };

  class Observable
    {
    public:

      Observable(void);

      void add_observer(Observer * observer);

      void delete_observer(Observer * observer);

      void update_observers(void);

      void set_changed(void);

    private:

      bool _changed;
      vector<Observer *> _observers;
    };

} // namespace hkl

#endif // _OBSERVER_H
