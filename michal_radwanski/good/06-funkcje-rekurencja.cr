int fib(int n) {
    if (n <= 1) return n;
    return n * fib(n - 1);
}
void main() {
    print(tostring(fib(7)));
    print("\n");
}
