package org.example;

public class Main {
    public static void main(String[] args) {
        var a = new GaloisField(1234577, 110);
        var b = new GaloisField(1234577, 145);
        var c = new GaloisField(b).inverse();
        System.out.println(a.divide(b));
        System.out.println(a.multiply(c));
        System.out.println(a.getCharact());
    }
}