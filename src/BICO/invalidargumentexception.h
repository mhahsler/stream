#ifndef INVALIDARGUMENTESCEPTION_H
#define INVALIDARGUMENTESCEPTION_H

#include "clueexception.h"

namespace CluE
{

/**
 * @brief Indicates invalid values of arguments.
 * @ingroup exceptions
 */
struct InvalidArgumentException : public CluEException
{
	InvalidArgumentException() {};
	InvalidArgumentException(int identifier, std::string message, std::string argument) :
		argument(argument)
	{
		this->identifier = identifier;
		this->message = message;
	};
	virtual ~InvalidArgumentException() {};

	std::string argument;
};
}

#endif
