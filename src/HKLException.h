// ============================================================================
//
// = CONTEXT
//    HKL Project
//
// = FILENAME
//    HKLException.h
//
// = AUTHORS
//    V. Delos
//
// ============================================================================

#ifndef _HKL_EXCEPTION_H_
#define _HKL_EXCEPTION_H_

// ============================================================================
// DEPENDENCIES
// ============================================================================
#include <string>
#include <vector>


// ============================================================================
// HKL Errors severities 
// ============================================================================
typedef enum {
  WARN, 
  ERR, 
  PANIC
} ErrorSeverity;

// ============================================================================
//! The HKL exception abstraction base class.  
// ============================================================================
//!  
//! detailed description to be written
//! 
// ============================================================================
class Error
{
public:

  /**
   * Initialization. 
   */
  Error (void);

  /**
   * Initialization. 
   */
  Error (const char *reason,
				 const char *desc,
				 const char *origin,
	       int severity = ERR);
  

  /**
   * Initialization. 
   */
  Error (const std::string& reason,
				 const std::string& desc,
				 const std::string& origin, 
	       int severity = ERR);

  /**
   * Copy constructor. 
   */
  Error (const Error& src);

  /**
   * Error details: code 
   */
  virtual ~Error (void);

  /**
   * operator= 
   */
  Error& operator= (const Error& _src);

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

// ============================================================================
// The HKL error list.	
// ============================================================================
typedef std::vector<Error> ErrorList;

// ============================================================================
//! The HKL exception abstraction base class.  
// ============================================================================
//!  
//! detailed description to be written
//! 
// ============================================================================
class HKLException
{
public:

  /**
   * Initialization. 
   */
  HKLException (void);

  /**
   * Initialization. 
   */
  HKLException (const char *reason,
					   const char *desc,
					   const char *origin,
	           int severity = ERR);
  
  /**
   * Initialization. 
   */
  HKLException (const std::string& reason,
					   const std::string& desc,
					   const std::string& origin, 
	           int severity = ERR);

  /**
   * Initialization. 
   */
  HKLException (const Error& error);


  /**
   * Copy constructor. 
   */
  HKLException (const HKLException& src);

  /**
   * operator=
   */
  HKLException& operator= (const HKLException& _src); 

  /**
   * Release resources.
   */
  virtual ~HKLException (void);

  /**
   * Push the specified error into the errors list.
   */
  void push_error (const char *reason,
					         const char *desc,
						       const char *origin, 
		               int severity = ERR);

  /**
   * Push the specified error into the errors list.
   */
  void push_error (const std::string& reason,
                   const std::string& desc,
                   const std::string& origin, 
                   int severity = ERR);

  /**
   * Push the specified error into the errors list.
   */
  void push_error (const Error& error);

  /**
   * The errors list
   */
   ErrorList errors;
    
private:

};


#endif // _HKL_EXCEPTION_H_

