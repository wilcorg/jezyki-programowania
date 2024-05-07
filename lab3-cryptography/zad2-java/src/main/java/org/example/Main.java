package org.example;

public class Main {
    public static void main(String[] args) {
//        System.out.println("Hello world!");
        final long CHARACT = 1234567891;
        GF dum = new GF(CHARACT, 110);
        DHSetup<GF> dh = new DHSetup<>(dum);

        System.out.println("Common generator: " + dh.getG());

        System.out.print("Alice says: ");
        User<GF> alice = new User<>(dh);

        System.out.print("Bob says: ");
        User<GF> bob = new User<>(dh);

        GF fromAlice = alice.getPublicKey();
        System.out.println("Alice's key: " + fromAlice);

        bob.setKey(fromAlice);

        GF fromBob = bob.getPublicKey();
        System.out.println("Bob's key: " + fromBob);

        alice.setKey(fromBob);
        GF encrypted = alice.encrypt(new GF(CHARACT, 110)).orElseThrow();
        System.out.println("Encrypted message: " + encrypted);

        GF decrypted = bob.decrypt(encrypted).orElseThrow();
        System.out.println("Decrypted message: " + decrypted);
    }
}