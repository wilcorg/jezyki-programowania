#ifndef ZAD1_GALOISFIELD_HPP
#define ZAD1_GALOISFIELD_HPP

#include "NumberTools.hpp"
#include <cstdint>
#include <iostream>

template<uint32_t CHARACT>
class GaloisField {
public:
    GaloisField<CHARACT>(); // default

    explicit GaloisField(uint32_t value);  // value ctor

    GaloisField(const GaloisField& gf);  // copy ctor

    GaloisField& operator=(const GaloisField& rhs);  // copy assign ctor

    GaloisField(GaloisField&& src) noexcept;  // move ctor

    GaloisField& operator=(GaloisField&& rhs) noexcept;  // move assign ctor

    constexpr uint32_t getCharacteristic();

    GaloisField<CHARACT> inverse() const;
    static GaloisField<CHARACT> negative(int64_t value);

    [[nodiscard]] uint32_t getValue() const;

    constexpr GaloisField<CHARACT>& operator+=(const GaloisField<CHARACT>& rhs);
    constexpr GaloisField<CHARACT>& operator-=(const GaloisField<CHARACT>& rhs);
    constexpr GaloisField<CHARACT>& operator*=(const GaloisField<CHARACT>& rhs);
    constexpr GaloisField<CHARACT>& operator/=(const GaloisField<CHARACT>& rhs);


private:
    uint32_t mValue;

    void moveFrom(GaloisField& src) noexcept;
};

template<uint32_t CHARACT>
GaloisField<CHARACT>::GaloisField()
        : mValue(0) {
//    std::cout << "DEBUG: default constructor called\n";
    if (!NumberTools::is_prime(CHARACT)) {
        throw std::invalid_argument("Character must be prime");
    }
}

template<uint32_t CHARACT>
GaloisField<CHARACT>::GaloisField(uint32_t value)
        : GaloisField() {
//    std::cout << "DEBUG: value constructor called\n";
    if (value < CHARACT) {
        this->mValue = value;
    } else {
        throw std::invalid_argument("Provided value is not in [0, CHARACT) range");
    }
}

template<uint32_t CHARACT>
GaloisField<CHARACT>::GaloisField(const GaloisField<CHARACT>& gf)
        : GaloisField(gf.mValue) {
//    std::cout << "DEBUG: copy constructor called\n";
}

template<uint32_t CHARACT>
GaloisField<CHARACT>& GaloisField<CHARACT>::operator=(const GaloisField<CHARACT>& rhs) {
//    std::cout << "DEBUG: copy assignment constructor called\n";
    if (this == &rhs) {
        return *this;
    }

    // copy and swap idiom
    GaloisField<CHARACT> temp(rhs);
    this->mValue = temp.mValue;
    return *this;
}

template<uint32_t CHARACT>
[[maybe_unused]] GaloisField<CHARACT>::GaloisField(GaloisField<CHARACT>&& src) noexcept
        : GaloisField() {
//    std::cout << "DEBUG: move constructor called\n";
    moveFrom(src);
}

template<uint32_t CHARACT>
GaloisField<CHARACT>& GaloisField<CHARACT>::operator=(GaloisField<CHARACT>&& rhs) noexcept {
//    std::cout << "DEBUG: move assignment constructor called\n";
    if (this == &rhs) {
        return *this;
    }
    moveFrom(rhs);
    return *this;
}

template<uint32_t CHARACT>
void GaloisField<CHARACT>::moveFrom(GaloisField<CHARACT>& src) noexcept {
    mValue = src.mValue;

    // reset the source object
    src.mValue = 0;
}


template<uint32_t CHARACT>
constexpr uint32_t GaloisField<CHARACT>::getCharacteristic() {
    return CHARACT;
}

template<uint32_t CHARACT>
GaloisField<CHARACT> GaloisField<CHARACT>::negative(int64_t value) {
    if (value <= 0) {
        return GaloisField<CHARACT>((CHARACT - 1 + value) % CHARACT);
    } else {
        return GaloisField<CHARACT>(value);
    }
}

template<uint32_t CHARACT>
uint32_t GaloisField<CHARACT>::getValue() const {
    return this->mValue;
}


template<uint32_t CHARACT>
constexpr GaloisField<CHARACT>& GaloisField<CHARACT>::operator+=(const GaloisField<CHARACT>& rhs) {
    uint64_t temp = this->mValue + rhs.mValue;
    this->mValue = static_cast<uint32_t>(temp % CHARACT);
    return *this;
}

template<uint32_t CHARACT>
constexpr GaloisField<CHARACT> operator+(const GaloisField<CHARACT>& lhs, const GaloisField<CHARACT>& rhs) {
    auto temp = GaloisField<CHARACT>(lhs);
    temp += rhs;
    return std::move(temp);
}

template<uint32_t CHARACT>
constexpr GaloisField<CHARACT>& GaloisField<CHARACT>::operator-=(const GaloisField<CHARACT>& rhs) {
    uint64_t temp = this->mValue + negative(rhs.mValue);
    this->mValue = static_cast<uint32_t>(temp % CHARACT);
    return *this;
}

template<uint32_t CHARACT>
constexpr GaloisField<CHARACT> operator-(const GaloisField<CHARACT>& lhs, const GaloisField<CHARACT>& rhs) {
    auto temp = GaloisField<CHARACT>(lhs);
    temp -= rhs;
    return std::move(temp);
}

template<uint32_t CHARACT>
constexpr GaloisField<CHARACT>& GaloisField<CHARACT>::operator*=(const GaloisField<CHARACT>& rhs) {
    uint64_t temp = this->mValue * rhs.mValue;
    this->mValue = static_cast<uint32_t>(temp % CHARACT);
    return *this;
}

template<uint32_t CHARACT>
constexpr GaloisField<CHARACT> operator*(const GaloisField<CHARACT>& lhs, const GaloisField<CHARACT>& rhs) {
    auto temp = GaloisField<CHARACT>(lhs);
    temp *= rhs;
    return std::move(temp);
}

template<uint32_t CHARACT>
constexpr GaloisField<CHARACT>& GaloisField<CHARACT>::operator/=(const GaloisField<CHARACT>& rhs) {
    auto rhsInv = rhs.inverse();
    *this *= rhsInv;
    return *this;
}

template<uint32_t CHARACT>
constexpr GaloisField<CHARACT> operator/(const GaloisField<CHARACT>& lhs, const GaloisField<CHARACT>& rhs) {
    auto temp = GaloisField<CHARACT>(lhs);
    temp /= rhs;
    return std::move(temp);
}

template<uint32_t CHARACT>
constexpr std::ostream& operator<<(std::ostream& ostr, const GaloisField<CHARACT>& gf) {
    ostr << gf.getValue();
    return ostr;
}

template<uint32_t CHARACT>
constexpr std::istream& operator>>(std::istream& istr, GaloisField<CHARACT>& gf) {
    uint32_t value;
    istr >> value;
    gf = GaloisField<CHARACT>(value);
    return istr;
}

template<uint32_t CHARACT>
constexpr std::weak_ordering operator<=>(const GaloisField<CHARACT>& lhs, const GaloisField<CHARACT>& rhs) {
    return lhs.getValue() <=> rhs.getValue();
}

template<uint32_t CHARACT>
GaloisField<CHARACT> GaloisField<CHARACT>::inverse() const {
    if (NumberTools::gcd(this->mValue, CHARACT).value_or(0) != 1) {
        throw std::invalid_argument("Value and character aren't coprime");
    }
    auto inverted = NumberTools::ext_euclid(this->mValue, CHARACT);
    return GaloisField::negative(inverted);
}
#endif  //ZAD1_GALOISFIELD_HPP
