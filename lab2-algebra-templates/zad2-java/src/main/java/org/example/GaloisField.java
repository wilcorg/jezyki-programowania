package org.example;

import lombok.Getter;

public class GaloisField {
    public GaloisField(int charact, int value) {
        if (!NumberTools.isPrime(charact)) {
            throw new IllegalArgumentException("character isn't prime");
        }
        if (value < 0 || value >= charact) {
            throw new IllegalArgumentException("value < 0 or value >= charact");
        }
        this.charact = charact;
        this.value = value;
    }

    public GaloisField(final GaloisField rhs) {
        this.charact = rhs.charact;
        this.value = rhs.value;
    }

    public GaloisField negative(int charact, int value) {
        if (value <= 0) {
            return new GaloisField(charact, (charact - 1 + value) % charact);
        } else {
            return new GaloisField(charact, value);
        }
    }

    public GaloisField inverse() {
        if (NumberTools.gcd(this.value, this.charact).orElse(0) != 1) {
            throw new IllegalArgumentException("Value and character aren't coprime");
        }
        int inverted = NumberTools.extEuclid(this.value, this.charact);
        return negative(this.charact, inverted);
    }

    public GaloisField add(final GaloisField rhs) {
        if (this.charact != rhs.charact) {
            throw new IllegalArgumentException("characters doesn't match");
        }
        return new GaloisField(this.charact, (this.value + rhs.value + this.charact) % this.charact);
    }

    public GaloisField substract(final GaloisField rhs) {
        if (this.charact != rhs.charact) {
            throw new IllegalArgumentException("characters doesn't match");
        }
        return new GaloisField(this.charact, (this.value - rhs.value + this.charact) % this.charact);
    }

    public GaloisField multiply(final GaloisField rhs) {
        if (this.charact != rhs.charact) {
            throw new IllegalArgumentException("characters doesn't match");
        }
        return new GaloisField(this.charact, (int) ((long) this.value * rhs.value) % this.charact);
    }

    public GaloisField divide(final GaloisField rhs) {
        if (this.charact != rhs.charact) {
            throw new IllegalArgumentException("characters doesn't match");
        }
        var opposite = new GaloisField(rhs).inverse();
        return new GaloisField(this.charact, (int) ((long) this.value * opposite.value) % this.charact);
    }

    public boolean equals(final GaloisField lhs, final GaloisField rhs) {
        return lhs.charact == rhs.charact && lhs.value == rhs.value;
    }

    public boolean notEquals(final GaloisField lhs, final GaloisField rhs) {
        return !equals(lhs, rhs);
    }

    public boolean greater(final GaloisField lhs, final GaloisField rhs) {
        return lhs.charact == rhs.charact && lhs.value > rhs.value;
    }

    public boolean less(final GaloisField lhs, final GaloisField rhs) {
        return lhs.charact == rhs.charact && lhs.value < rhs.value;
    }

    public boolean greaterEqual(final GaloisField lhs, final GaloisField rhs) {
        return !less(lhs, rhs);
    }

    public boolean lessEqual(final GaloisField lhs, final GaloisField rhs) {
        return !greater(lhs, rhs);
    }

    @Override
    public String toString() {
        return "characteristic: " + this.charact + "; value: " + this.value;
    }

    @Getter
    private final int charact;

    private final int value;
}
