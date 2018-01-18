#ifndef INVALIDRUNTIMECONFIGURATIONEXCEPTION_H
#define INVALIDRUNTIMECONFIGURATIONEXCEPTION_H

#include "clueexception.h"

namespace CluE
{

/**
 * @brief Indicates that a computation entered an invalid configuration state.
 * @ingroup exceptions
 */
struct InvalidRuntimeConfigurationException : public CluEException
{
	InvalidRuntimeConfigurationException() {};
	InvalidRuntimeConfigurationException(int identifier, std::string message)
	{
		this->identifier = identifier;
		this->message = message;
	};
	virtual ~InvalidRuntimeConfigurationException() {};
};
}

#endif
