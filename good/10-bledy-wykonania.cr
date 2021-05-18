void main() {
    // Wykonanie funkcji poniżej skończy się błędem.
    // f1();
    // f2();
    // f3();
}
void f1() {
    23 % 0;
}
void f2() {
    int x = 0;
    23 / x;
}
void f3() {
    int x;
    [x, "to nie jest 2"] := [8, "to nie jest 2"]; // to zadziała
    [x, 2] := [8, "to nie jest 2"];
}
