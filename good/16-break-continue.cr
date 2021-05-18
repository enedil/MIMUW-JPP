void main() {
    int i = 27;
    while (true) {
        if (i == 1) break;
        if (i % 2 == 0) {
            i = i / 2;
            continue;
        }
        print(tostring(i)); print(" było nieparzyste\n");
        i = 3*i + 1;
    }
}
