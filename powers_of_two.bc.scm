define powers_of_two(n) {
    auto value

    value = 2;
    for (i = 0; i < n - 1; i++) {
        value = 2 ^ value;
    }

    return value;
}