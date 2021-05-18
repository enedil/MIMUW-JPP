void main() {
    int a = 0;
    printint(a);
    inc(a);
    printint(a);
    dec(a);
    printint(a);
}
void printint(int arg) {
    print("arg = ");
    print(tostring(arg));
    print("\n");
}

void inc(int ref a) {
    a = a + 1;
}
int dec(int a) {
    a = a - 1;
    return a;
}
