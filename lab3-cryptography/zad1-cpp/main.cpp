#include "gf/gf.hpp"
#include "DHSetup.hpp"
#include "User.hpp"

int main() {
    const auto CHARACT = 1234567891;
    DHSetup<GF<CHARACT>> dh;

    std::cout << "common generator: " << dh.get_generator() << std::endl;


    std::cout << "Alice says: ";
    User<GF<CHARACT>> alice(dh);

    std::cout << "Bob says: ";
    User<GF<CHARACT>> bob(dh);

    auto from_alice = alice.get_pub_key();
    std::cout << "Alice's key: " << from_alice << std::endl;

    bob.set_key(from_alice);

    auto from_bob = bob.get_pub_key();
    std::cout << "Bob's key: " << from_bob << std::endl;

    alice.set_key(from_bob);

    auto message = GF< CHARACT>(110);
    auto encrypted = alice.encrypt(message);
    std::cout << "encrypted message: " << encrypted.value() << std::endl;

    auto decrypted = bob.decrypt(encrypted.value());
    std::cout << "decrypted message: " << decrypted.value() << std::endl;

    return 0;
}
