void printint(int a) {
    print(tostring(a));
}

void main() {
    int a, b;
    string s;
    tuple<int, string> c;
    [a, b, c] := [2, 5, [13, "aaa"]];
    [a, s] := c;
    printint(a); print(" "); printint(b); print(" "); print(s); print("\n");
    c := [3, "1"];
    int x; string s1;
    [x, s1] := c;
    printint(x); print(" "); print(s1); print("\n");
}
