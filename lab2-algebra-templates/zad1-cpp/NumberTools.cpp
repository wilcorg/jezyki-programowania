#include "NumberTools.hpp"

int32_t NumberTools::ext_euclid(int32_t x, int32_t y) {
    int32_t old_r{x};
    int32_t r{y};
    int32_t old_s{1};
    int32_t s{0};
    int32_t old_t{0};
    int32_t t{1};
    int32_t temp;

    while (r != 0) {
        int32_t quotient{old_r / r};

        temp = r;
        r = old_r - quotient * temp;
        old_r = temp;

        temp = s;
        s = old_s - quotient * temp;
        old_s = temp;

        temp = t;
        t = old_t - quotient * temp;
        old_t = temp;
    }
    return old_s * old_r;
}

std::optional<uint32_t> NumberTools::gcd(uint32_t a, uint32_t b) {

    if (a == 0 || b == 0) {
        return {};
    }

    while (b != 0) {
        uint32_t temp{b};
        b = a % b;
        a = temp;
    }
    return {a};
}

bool NumberTools::is_prime(uint32_t num) {
    if (num < 2) {
        return false;
    } else if (num == 2) {
        return true;
    } else {
        int32_t bound{static_cast<int32_t>(std::sqrt(num)) + 1};
        if (num % 2 == 0) { return false; }
        for (int32_t i = 3; i < bound; i += 2) {
            if (num % i == 0) { return false; }
        }
        return true;
    }
}
