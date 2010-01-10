function Max2(a, b) {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}
function Max3(a, b, c) {
    return Max2(a, Max2(b, c));
}
Max3(1, 2, 3);
Max3('1', '2', '3');
