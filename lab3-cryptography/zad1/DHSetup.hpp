#ifndef GF_CPP_DHSETUP_HPP
#define GF_CPP_DHSETUP_HPP

#include "gf/tools.hpp"

template<typename T>
class DHSetup {
private:
    T g;

public:
    DHSetup() : g{} {
        static_assert(is_gf<T>::value);
        this->g = T(gen_key(g.charact(), sizeof(g.charact()) * 8));
    }
    T get_generator() { return this->g; }

    template<typename E>
    T power(T a, E b) {
        static_assert(std::is_integral_v<E>);

        if (b == 0) { return T{1}; }
        T temp = power(a, b / 2);
        if (b % 2 == 0) {
            return temp * temp;
        } else {
            return a * temp * temp;
        }
    }
};

#endif//GF_CPP_DHSETUP_HPP
