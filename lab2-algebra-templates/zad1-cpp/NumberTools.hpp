#ifndef ZAD1_NUMBERTOOLS_HPP
#define ZAD1_NUMBERTOOLS_HPP

#include <cmath>
#include <cstdint>
#include <limits>
#include <memory>
#include <optional>

class NumberTools {
public:
    static bool is_prime(uint32_t num);
    static std::optional<uint32_t> gcd(uint32_t a, uint32_t b);
    static int32_t ext_euclid(int32_t x, int32_t y);
};

#endif  //ZAD1_NUMBERTOOLS_HPP
