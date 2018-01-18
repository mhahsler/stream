#ifndef RANDOMNESS_H
#define RANDOMNESS_H

#include <random>
#include <time.h>

#include "randomgenerator.h"

namespace CluE
{

/**
 * @brief Random number generator.
 *
 * @ingroup helper_classes
 */
class Randomness
{
private:
    static std::mt19937 mt19937Generator;

public:

    static RandomGenerator getRandomGenerator()
    {
        return RandomGenerator(&mt19937Generator);
    }

    static void initialize(uint_fast32_t seed)
    {
        mt19937Generator = std::mt19937(seed);
    }
};

}

#endif
