package org.example;

import java.util.Optional;

public class User<T extends GF> {

    private DHSetup<T> dh;
    private final Long SECRET;
    private T KEY;

    User(DHSetup<T> dh) {
        this.dh = dh;
        this.SECRET = Tools.genKey();
        System.out.println("my private key: " + SECRET);
    }

    T getPublicKey() {
        return dh.power(dh.getG(), this.SECRET, dh.getG().getCharact());
    }

    void setKey(T a) {
        this.KEY = dh.power(a, SECRET, a.getCharact());
    }

    public Optional<T> encrypt(T plaintext) {
        if (KEY == null) {
            System.err.println("no key!");
            return Optional.empty();
        }
        return Optional.of((T) plaintext.mul(KEY));
    }

    public Optional<T> decrypt(T encrypted) {
        if (KEY == null) {
            System.err.println("no key!");
            return Optional.empty();
        }
        return Optional.of((T) encrypted.div(KEY));
    }
}

