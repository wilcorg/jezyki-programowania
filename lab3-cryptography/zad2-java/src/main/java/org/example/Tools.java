package org.example;

import java.security.SecureRandom;
import java.util.Optional;

public class Tools {
    public static boolean isPrime(long num) {
        if (num < 2) {
            return false;
        }
        if (num == 2) {
            return true;
        }
        if (num % 2 == 0) {
            return false;
        }

        var bound = (long) Math.sqrt(num);

        for (var i = 3; i < bound; i += 2) {
            if (num % i == 0) {
                return false;
            }
        }
        return true;
    }

    public static Optional<Long> gcd(long a, long b) {
        if (a == 0 || b == 0) {
            return Optional.empty();
        }

        a = Math.abs(a);
        b = Math.abs(b);
        while (b != 0) {
            long temp = b;
            b = a % b;
            a = temp;
        }
        return Optional.of(a);
    }

    static long extEuclid(long x, long y) {
        long oldR = x;
        long r = y;
        long oldS = 1;
        long s = 0;
        long oldT = 0;
        long t = 1;
        long temp;

        while (r != 0) {
            long quotient = oldR / r;

            temp = r;
            r = oldR - quotient * temp;
            oldR = temp;

            temp = s;
            s = oldS - quotient * temp;
            oldS = temp;

            temp = t;
            t = oldT - quotient * temp;
            oldT = temp;
        }
        return oldS * oldR;
    }

    static long genKey() {
        SecureRandom random = new SecureRandom();
        long result = 0;
        for (int i = 0; i < 32; i++) {
            result <<= 1;
            result |= random.nextInt(2);
        }

        if (result == 0) {
            return genKey();
        } else {
            return result;
        }
    }
}
