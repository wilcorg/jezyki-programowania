#ifndef GF_CPP_USER_HPP
#define GF_CPP_USER_HPP

#include "DHSetup.hpp"
#include <iostream>
#include <optional>

template<typename T, typename E = int64_t>
class User {
private:
    DHSetup<T> dh;
    E SECRET;
    T KEY;

public:
    explicit User<T>(const DHSetup<T>& dh) : KEY{0}, dh{dh} {
        this->SECRET = E(gen_key(SECRET, sizeof(SECRET) * 8));
        std::cout << "my private key: " << SECRET << std::endl;
    }

    T get_pub_key() {
        return dh.power(dh.get_generator(), SECRET);
    }

    void set_key(T key) {
        this->KEY = dh.power(key, SECRET);
    }

    std::optional<T> encrypt(T& plaintext) {
        if (KEY.value() == 0) {
            std::cerr << "no public key!" << std::endl;
            return {};
        }
        return plaintext *= KEY;
    }

    std::optional<T> decrypt(T& encrypted) {
        if (KEY.value() == 0) {
            std::cerr << "no public key!" << std::endl;
            return {};
        }
        return encrypted /= KEY;
    }
};

#endif//GF_CPP_USER_HPP
