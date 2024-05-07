package org.example;

import lombok.Getter;

public class DHSetup<T extends GF> {

    @Getter
    private T g;

    public DHSetup(T dum) {
        this.g = (T) new GF(dum.getCharact(), Tools.genKey());
    }

    public T power(T a, long b, long charact) {
        if (b == 0) { return (T) new GF(charact, 1); }
        T temp = power(a, b / 2, charact);
        if (b % 2 == 0) {
            return (T) temp.mul(temp);
        } else {
            return (T) a.mul(temp).mul(temp);
        }
    }

//    private static init() {
//
//    }
}
