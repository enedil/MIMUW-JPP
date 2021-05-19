void main() {
    // Wykonanie funkcji poniżej skończy się błędem.
    // f1();
    // f2();
    // f3();
    // f4();
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
    [x, "to też nie jest 2, ale string jest inny"] := [8, "to nie jest 2"];
}
void f4() {
    int x;
    x + 3;
}
