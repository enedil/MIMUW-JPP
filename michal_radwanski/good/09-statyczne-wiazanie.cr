void g() {
    int a = 4;
    {
        int a = 5;
        {
            a = 8;
        }
        if (a == 8) print("1. OK\n");
    }
    if (a == 4) print("2. OK\n");
}

int x;
void f1() {
    if (x == 42) print("3. OK\n");
}
void f2() {
    int x = 1000;
    f1();
}

void main() {
    x = 42;
    g();
    f2();
}
