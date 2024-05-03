#ifndef GF_CPP_TOOLS_HPP
#define GF_CPP_TOOLS_HPP

#include <boost/multiprecision/gmp.hpp>
#include <optional>
#include <random>
#include <type_traits>

typedef boost::multiprecision::mpz_int mpz_int;

template<typename T>
constexpr T floor_sqrt(T target) {
    static_assert(std::is_integral_v<T>);

    T low = 1;
    T high = 1;
    while (high * high < target) {
        low = high;
        high <<= 1;
    }

    T delta = low / 2;
    while (delta != 0) {
        T test = low + delta;
        if (test * test > target) {
            delta >>= 1;
        } else {
            low += delta;
        }
    }
    return low;
}

template<typename T>
constexpr bool is_prime(T N) {
    static_assert(std::is_integral_v<T>);

    if (N < 2) { return false; }
    if (N == 2) { return true; }
    if (N % 2 == 0) { return false; }

    T bound{floor_sqrt(N)};

    for (T i = 3; i <= bound; i += 2) {
        if (N % i == 0) {
            return false;
        }
    }
    return true;
}

//constexpr bool is_digit(const std::basic_string<char>& str) {
//    for (auto& val: str) {
//        if (val < '0' || val > '9') {
//            return false;
//        }
//    }
//    return true;
//}

//constexpr bool is_prime(const mpz_int& num) {
//    if (num < 2) { return false; }
//    if (num == 2) { return true; }
//    if (num % 2 == 0) { return false; }
//
//    mpz_int bound{floor_sqrt(num)};
//
//    for (mpz_int i = 3; i <= bound; i += 2) {
//        if (num % i == 0) {
//            return false;
//        }
//    }
//    return true;
//}

template<typename T>
constexpr std::optional<T> gcd(T a, T b) {
    static_assert(std::is_integral_v<T> || std::is_same_v<T, mpz_int>);

    if (a <= 0 || b <= 0) { return {}; }
    while (b != 0) {
        T temp{b};
        b = a % b;
        a = temp;
    }
    return {a};
}

template<typename T>
constexpr T ext_euclid(T x, T y) {
    static_assert(std::is_integral_v<T> || std::is_same_v<T, mpz_int>);

    T old_r{x};
    T r{y};
    T old_s{1};
    T s{0};
    T old_t{0};
    T t{1};
    T temp;

    while (r != 0) {
        T quotient{old_r / r};

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

template<typename T>
T gen_key(T, size_t bits) {
    static_assert(std::is_integral_v<T> || std::is_same_v<T, mpz_int>);
    static_assert(!std::is_same_v<T, bool>);

    std::random_device rd;
    std::mt19937 rng(rd());
    std::uniform_int_distribution<short> dist(0, 1);

    T result{};
    if (std::is_signed_v<T>) { bits -= 1; }
    for (size_t i = 0; i < bits; ++i) {
        result <<= 1;
        result |= dist(rng);
    }

    if (result == 0) {
        return gen_key(result, bits);
    } else {
        return result;
    }
}

#endif//GF_CPP_TOOLS_HPP
