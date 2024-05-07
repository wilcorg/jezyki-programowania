#ifndef GF_CPP_GF_HPP
#define GF_CPP_GF_HPP

#include "tools.hpp"
#include <iostream>


template<int64_t N>
class GF {
private:
    int64_t mValue;

public:
    GF() : mValue(0) {
        static_assert(is_prime(N));
    }

    explicit GF(int64_t value) : GF() { this->mValue = value % N; }

    GF(const GF& rhs) = default;

    GF& operator=(const GF& rhs) = default;

    GF(GF&& rhs) noexcept = default;

    GF& operator=(GF&& rhs) noexcept = default;

    [[maybe_unused]] constexpr int64_t charact() { return N; }
    [[nodiscard]] int64_t value() const { return this->mValue; }

    constexpr GF<N> inverse() const;

    constexpr GF<N> negative() const;


    constexpr GF<N>& operator+=(const GF<N>& rhs);
    constexpr GF<N>& operator-=(const GF<N>& rhs);
    constexpr GF<N>& operator*=(const GF<N>& rhs);
    constexpr GF<N>& operator/=(const GF<N>& rhs);
};

template<int64_t N>
constexpr GF<N> GF<N>::inverse() const {
    if (gcd(this->mValue, N).value_or(0) != 1) {
        throw std::invalid_argument("Value and character aren't coprime");
    }
    auto inverted = ext_euclid(this->mValue, N);
    if (inverted < 0) {
        inverted += N;
    }
    inverted %= N;
    return GF(inverted);
}

template<int64_t N>
constexpr GF<N> GF<N>::negative() const {
    return GF(N - this->mValue);
}

template<int64_t N>
constexpr GF<N>& GF<N>::operator+=(const GF<N>& rhs) {
    unsigned long int temp = this->mValue + rhs.mValue;
    this->mValue = temp % N;
    return *this;
}

template<int64_t N>
constexpr GF<N>& GF<N>::operator-=(const GF<N>& rhs) {
    auto negated = rhs.negative();
    *this += negated;
    return *this;
}

template<int64_t N>
constexpr GF<N>& GF<N>::operator*=(const GF<N>& rhs) {
    unsigned long long int temp = this->mValue * rhs.mValue;
    this->mValue = temp % N;
    return *this;
}

template<int64_t N>
constexpr GF<N>& GF<N>::operator/=(const GF<N>& rhs) {
    auto inverted = rhs.inverse();
    *this *= inverted;
    return *this;
}

template<int64_t N>
constexpr GF<N> operator+(const GF<N>& lhs, const GF<N>& rhs) {
    auto temp = GF<N>(lhs);
    temp += rhs;
    return std::move(temp);
}

template<int64_t N>
constexpr GF<N> operator-(const GF<N>& lhs, const GF<N>& rhs) {
    auto temp = GF<N>(lhs);
    temp -= rhs;
    return std::move(temp);
}

template<int64_t N>
constexpr GF<N> operator*(const GF<N>& lhs, const GF<N>& rhs) {
    auto temp = GF<N>(lhs);
    temp *= rhs;
    return std::move(temp);
}

template<int64_t N>
constexpr GF<N> operator/(const GF<N>& lhs, const GF<N>& rhs) {
    auto temp = GF<N>(lhs);
    temp /= rhs;
    return std::move(temp);
}

template<int64_t N>
constexpr std::weak_ordering operator<=>(const GF<N>& lhs, const GF<N>& rhs) {
    return lhs.value() <=> rhs.value();
}

template<int64_t N>
std::ostream& operator<<(std::ostream& ostr, const GF<N>& gf) {
    ostr << gf.value();
    return ostr;
}

template<typename T>
class is_gf : public std::false_type {};

template<int64_t N>
class is_gf<GF<N>> : public std::true_type {};

#endif//GF_CPP_GF_HPP
