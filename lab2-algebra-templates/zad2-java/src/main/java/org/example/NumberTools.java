package org.example;

import java.util.Optional;

public class NumberTools {
    public static boolean isPrime(int num) {
        if (num < 2) {
            return false;
        } else if (num == 2) {
            return true;
        } else {
            int bound = (int) Math.sqrt(num) + 1;

            for (int i = 2; i < bound; i++) {
                if (num % i == 0) {
                    return false;
                }
            }
            return true;
        }
    }

    public static Optional<Integer> gcd(int a, int b) {
        if (a == 0 || b == 0) {
            return Optional.empty();
        }

        a = Math.abs(a);
        b = Math.abs(b);
        while (b != 0) {
            int temp = b;
            b = a % b;
            a = temp;
        }
        return Optional.of(a);
    }

    static int extEuclid(int x, int y) {
        int oldR = x;
        int r = y;
        int oldS = 1;
        int s = 0;
        int oldT = 0;
        int t = 1;
        int temp;

        while (r != 0) {
            int quotient = oldR / r;

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
}
