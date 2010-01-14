function Gcd(a, b) {
    while (a != 0) {
        b = b % a;

        // this line, instead of above, makes inferring precision quite better.
//      b = (b % a) & 2147483647;

        // swap
        a = a ^ b;
        b = b ^ a;
        a = a ^ b;
    }
    return b;
}
print(Gcd(12707, 12319));
