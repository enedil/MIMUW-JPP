void main() {
    int a, b;
    string s;
    tuple<int, string> c;
    [a, b, c] := [2, 5, [13, "aaa"]];
    [a, s] := c;
}
