function Gcd(a, b) {
    while (a != 0) {
        b = b % a;

        // swap
        a = a ^ b;
        b = b ^ a;
        a = a ^ b;
    }
    return b;
}
print(Gcd(12707, 12319));
