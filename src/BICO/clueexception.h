#ifndef CLUEEXCEPTION_H
#define CLUEEXCEPTION_H

#include <string>

namespace CluE
{
/**
 * @brief CluE related exceptions base class.
 * 
 * \c message is an human readable error type string. \n
 * \c identifier is an integer used for encoding the error type. The interpratition is defined \b individually for each method!
 * 
 * @ingroup exceptions
 */
struct CluEException
{
	CluEException() :
		identifier(-1),
		message("")
	{};
	CluEException(int identifier, std::string message) :
		identifier(identifier),
		message(message)
	{};
	virtual ~CluEException() {};

	int identifier;
	std::string message;
};
}

#endif
