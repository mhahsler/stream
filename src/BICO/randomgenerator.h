#ifndef RANDOMGENERATOR_H
#define RANDOMGENERATOR_H

class RandomGenerator
{
private:
    std::mt19937 * generator;
public:
    typedef decltype((*generator)()) result_type;

    RandomGenerator(std::mt19937 * generator) :
    generator(generator)
    {
    }

    result_type operator()()
    {
        return (*generator)();
    }

    result_type min()
    {
        return generator->min();
    }

    result_type max()
    {
        return generator->max();
    }
};

#endif