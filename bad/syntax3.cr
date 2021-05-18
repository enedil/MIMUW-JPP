void main() {
    int a, b;
    tuple<int, int> tup;
    [tup] := [[2, 3]]; // OK
    [a, b] := [2, 3]; // OK
    tup := [2, 3]; // nie OK
}
