//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/HKLException.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class Error, HKLException

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.5 $

//

// $Log: HKLException.h,v $
// Revision 1.5  2005/02/10 09:19:49  picca
// Rewrite of the documentation
//
// Revision 1.4  2005/01/27 09:23:53  delos
// Commentaires pour CVS en tete des fichiers
//

//

//

// copyleft :       Synchrotron SOLEIL

//                  L'Orme des Merisiers

//                  Saint-Aubin - BP 48

//                  91192 GIF-sur-YVETTE CEDEX

//

//-======================================================================

/// ============================================================================
///
/// = CONTEXT
///    HKL Project
///
/// = FILENAME
///    HKLException.h
///
/// = AUTHORS
///    V. Delos
///    F-E. Picca
///
/// ============================================================================

#ifndef _HKL_EXCEPTION_H_
#define _HKL_EXCEPTION_H_

#include <string>
#include <vector>


/**
 * \brief Errors severities
 *
 * these enums describe the severity of the errors.
 */
typedef enum {
  WARN, //< just a warning message
  ERR,  //< A normal error message
  PANIC //< A important error message
} ErrorSeverity;

/**
 * \brief The Error exception abstraction base class
 *
 * This class describe the Error exception mecanism of TANGO. We will use
 * it to derive the HKLException class
 */
class Error
{
public:

  /**
   * \brief Default constructor.
   *
   * Create and push a error with all parameters set to 0.
   */
  Error (void);

  /**
   * \brief Constructor from char parameters. 
   * \param reason Why the exception occured
   * \param desc what was the exception
   * \param origin In which part of the code the exception occured
   * \param severity an integer corresponding to the severity of the error
   *
   * Create and push a new error with the right parameters set from const char*
   */
  Error (const char *reason,
          const char *desc,
          const char *origin,
          int severity = ERR);

  /**
   * \brief Constructor from string parameters. 
   * \param reason Why the exception occured
   * \param desc what was the exception
   * \param origin In which part of the code the exception occured
   * \param severity an integer corresponding to the severity of the error
   *
   * Create and push a new error with the right parameters set from strings
   */
  Error (const std::string &reason,
          const std::string &desc,
          const std::string &origin, 
          int severity = ERR);

  /**
   * \brief Copy constructor.
   * @param src The src error to copy from
   * 
   * Create and push a new Error from the src error
   */
  Error (const Error &src);

  /**
   * \brief Destructor
   *
   * Destructor which release all the memory used by the Errors 
   */
  virtual ~Error (void);

  /**
   * \brief operator=
   * @param _src The _src error to copy from.
   *
   * This fonction is equivalent to a copy constructor except that if the _src
   * error is the same than (*this) there is no copy.
   */
  Error& operator= (const Error &_src);

  /**
   * Error details: reason 
   */
  std::string reason;

  /**
   * Error details: description 
   */
  std::string desc;

  /**
   * Error details: origin 
   */
  std::string origin;

  /**
   * Error details: severity 
   */
  int severity;

};

/**
 * Define the type ErrorList which will contain the error list.
 */
typedef std::vector<Error> ErrorList;


/**
 * \brief The Error exception abstraction base class
 *
 * This class describe the HKLException mecanism in our. We will use
 * it to derive the HKLException class
 */
class HKLException
{
public:

  /**
   * \brief Default constructor.
   *
   * Create and push a new HKLException with all parameters set to 0.
   */
  HKLException (void);

  /**
   * \brief Constructor from char parameters. 
   * \param reason Why the exception occure
   * \param desc what is this exception
   * \param origin In which part of the code the exception occure
   * \param severity Gravity of the exception
   *
   * Create and push a new HKLException with the right parameters set from const char*
   */
  HKLException (const char *reason,
                const char *desc,
                const char *origin,
                int severity = ERR);
  
  /**
   * \brief Constructor from string parameters. 
   * \param reason Why the exception occure
   * \param desc what is this exception
   * \param origin In which part of the code the exception occured
   * \param severity Gravity of the exception
   *
   * Create and push a new HKLException with the right parameters set from strings
   */
  HKLException (const std::string& reason,
                const std::string& desc,
                const std::string& origin,
                int severity = ERR);

  /**
   * \brief Copy constructor.
   * @param error
   * 
   * Create and push a new HKLException copy of the Error error 
   */
  HKLException (const Error &error);


  /**
   * \brief Copy constructor
   * @param src 
   * 
   * Create and push a new HKLException from the src HKLException
   */
  HKLException (const HKLException &src);

  /**
   * \brief operator=
   * @param _src The _src HKLException to copy from.
   *
   * This fonction is equivalent to a copy constructor except that if the _src
   * HKLException is the same than (*this) there is no copy.
   */
  HKLException& operator= (const HKLException &_src); 

  /**
   * \brief Destructor
   * 
   * Release resources
   */
  virtual ~HKLException (void);

  /**
   * \brief Push the specified error into the errors list.
   * @param reason Why the exception occure
   * @param desc what is this exception
   * @param origin In which part of the code the exception occured
   * @param severity Gravity of the exception
   * 
   * push a new HKLException into the error list with the right
   * parameters set from const char*
   */
  void push_error (const char *reason,
                    const char *desc,
                    const char *origin, 
                    int severity = ERR);
  
  /**
   * \brief Push the specified HKLException into the errors list.
   * @param reason Why the exception occure
   * @param desc what is this exception
   * @param origin In which part of the code the exception occured
   * @param severity Gravity of the exception
   *
   * push a new HKLException into the error list with the right 
   * parameters set from strings.
   */
  void push_error (const std::string& reason,
                   const std::string& desc,
                   const std::string& origin, 
                   int severity = ERR);

  /**
   * \brief Push the specified HKLException into the errors list.
   * @param error
   * 
   * push a new HKLException into the error list with the right 
   * parameters set from the Error error
   */
  void push_error (const Error& error);

  /**
   * \brief The errors list
   *
   * The error list containing all the HKLExceptions.
   */
   ErrorList errors;
    
private:

};


#endif // _HKL_EXCEPTION_H_

