string f(boolean b) {
    if (b) return "a";
    else return "b";
}
int g() {
    return 16;
}

void main() {
    string s;
    print(f(true)); print("\n");
    print(f(false)); print("\n");
    print(tostring(g())); print("\n");
}
