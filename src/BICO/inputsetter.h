#ifndef INPUTSETTER_H
#define INPUTSETTER_H

#include <vector>

namespace CluE
{
/**
 * @brief Interface to propagate the ability to set input data
 *
 * @ingroup base_classes
 */
template<typename T> class InputSetter
{
public:
	virtual void setInput(std::vector<T*> const*) = 0;
};
}

#endif
