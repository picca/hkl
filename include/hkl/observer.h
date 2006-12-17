#ifndef _OBSERVER_H
#define _OBSERVER_H

#include <vector>

using namespace std;

namespace hkl
{

// forward declaration
class Observable;

/**
 * \brief A class design to describe the Observer-Observable Design pattern. 
 */
class Observer
{
public:

    /**
     * @brief the default constructor
     */
    Observer(void);

    /**
     * @brief The update method of the Observer
     */
    virtual void update(void) = 0;

    /**
     * @brief Connect the Observer
     */
    void connect(void);

    /**
     * @brief Un-connect the Observer
     */
    void unconnect(void);

protected:
    bool _connected; //!< the connect state of the Observer.

};

/**
 * \brief A class design to describe the Observer-Observable Design pattern. 
 */
class Observable
{
public:

    /**
     * @brief The default constructor.
     */
    Observable(void);

    /**
     * @brief Add an observer to the Observable.
     * @param observer A pointer on an Observer.
     */
    void add_observer(Observer * observer);

    /**
     * @brief Delete an observer from the Observable.
     * @param observer A pointer on an Observer.
     */
    void delete_observer(Observer * observer);

    /**
     * @brief Update all the observer looking for this Observable.
     */
    void update_observers(void);

    /**
     * @brief Set the changed state of the Observable.
     *
     * If the changed state is not on, no Observer update is possible.
     */
    void set_changed(void);

private:

    bool _changed; //!< the changed state of the Observable.
    vector<Observer *> _observers; //!< A vector of Observer, Observing this Observable.
};

} // namespace hkl

#endif // _OBSERVER_H
