#include "GaloisField.hpp"
#include <iostream>

int main() {
    auto a = GaloisField<1234577>(110);
    auto b = GaloisField<1234577>(145);
    auto c = b.inverse();
    std::cout << a / b << std::endl;
    std::cout << a * c << std::endl;
    std::cout << a.getCharacteristic() << std::endl;
    return 0;
}
