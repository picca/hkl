#ifndef _OBSERVER_H
#define _OBSERVER_H


#include <vector>

namespace hkl {

class Observer {
  protected:
    bool _connected;


  public:
    Observer();

    virtual ~Observer();

    virtual void update() = 0;

    void connect();

    void unconnect();

};
class Observable {
  protected:
    bool _changed;

    std::vector<hkl::Observer *> _observers;


  public:
    /**
     * @brief The default constructor.
     */
    Observable();

    /**
     * @brief Add an hkl::Observer to the Observable.
     * @param observer The hkl::Observer pointer to add.
     */
    void add_observer(hkl::Observer * observer);

    /**
     * @brief Delete an hkl::Observer from the Observable.
     * @param observer The hkl::Observer pointer to remove.
     */
    void del_observer(hkl::Observer * observer);

    /**
     * @brief Update all the Observer looking for this Observable.
     */
    void update_observers();

    /**
     * @brief Set the changed state of the Observable.
     *
     * If the changed state is not on, no Observer update is possible.
     */
    void set_changed();

};

} // namespace hkl
#endif
