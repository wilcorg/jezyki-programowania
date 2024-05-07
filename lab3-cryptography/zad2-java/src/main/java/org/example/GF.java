package org.example;

import lombok.Getter;

import java.util.Comparator;

public class GF implements Comparable<GF>, Comparator<GF> {
    @Getter
    private final long charact;

    private final long value;

    public GF(long charact, long value) {
        if (!Tools.isPrime(charact)) {
            throw new IllegalArgumentException("character isn't prime");
        }
        this.value = value % charact;
        if (this.value < 0 || this.value >= charact) {
            throw new IllegalArgumentException("value < 0 or value >= charact");
        }
        this.charact = charact;
    }

    public GF(final GF rhs) {
        this.charact = rhs.charact;
        this.value = rhs.value;
    }

    public GF negative(long charact, long value) {
        if (value <= 0) {
            return new GF(charact, (charact - 1 + value) % charact);
        } else {
            return new GF(charact, value);
        }
    }

    public GF inverse() {
        if (Tools.gcd(this.value, this.charact).orElse(0L) != 1) {
            throw new IllegalArgumentException("Value and character aren't coprime");
        }
        long inverted = Tools.extEuclid(this.value, this.charact);
        return negative(this.charact, inverted);
    }

    public GF add(final GF rhs) {
        if (this.charact != rhs.charact) {
            throw new IllegalArgumentException("characters doesn't match");
        }
        return new GF(this.charact, (this.value + rhs.value + this.charact) % this.charact);
    }

    public GF sub(final GF rhs) {
        if (this.charact != rhs.charact) {
            throw new IllegalArgumentException("characters doesn't match");
        }
        return new GF(this.charact, (this.value - rhs.value + this.charact) % this.charact);
    }

    public GF mul(final GF rhs) {
        if (this.charact != rhs.charact) {
            throw new IllegalArgumentException("characters doesn't match");
        }
        return new GF(this.charact, (this.value % this.charact * rhs.value % this.charact) % this.charact);
    }

    public GF div(final GF rhs) {
        if (this.charact != rhs.charact) {
            throw new IllegalArgumentException("characters doesn't match");
        }
        var opposite = new GF(rhs).inverse();
        return new GF(this.charact, (this.value % this.charact * opposite.value % this.charact) % this.charact);
    }

    public boolean equals(final GF lhs, final GF rhs) {
        return lhs.charact == rhs.charact && lhs.value == rhs.value;
    }

    public boolean notEquals(final GF lhs, final GF rhs) {
        return !equals(lhs, rhs);
    }

    public boolean greater(final GF lhs, final GF rhs) {
        return lhs.charact == rhs.charact && lhs.value > rhs.value;
    }

    public boolean less(final GF lhs, final GF rhs) {
        return lhs.charact == rhs.charact && lhs.value < rhs.value;
    }

    public boolean greaterEqual(final GF lhs, final GF rhs) {
        return !less(lhs, rhs);
    }

    public boolean lessEqual(final GF lhs, final GF rhs) {
        return !greater(lhs, rhs);
    }

    @Override
    public String toString() {
        return "characteristic: " + this.charact + "; value: " + this.value;
    }

    @Override
    public int compareTo(GF o) {
        return Long.compare(this.value, o.value);
    }

    @Override
    public int compare(GF o1, GF o2) {
        return Long.compare(o1.value, o2.value);
    }
}
