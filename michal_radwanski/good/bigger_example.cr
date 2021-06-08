// Random ----------------------------------------------------------------------
int rnd_state;
void stand_r(int ref state) {
    state = 473829;
}
void srand() {
    stand_r(rnd_state);
}
int rand_r(int ref state) {
    state = (state * 432789 + 3484747) % 57483927854;
    return state;
}
int rand() {
    return rand_r(rnd_state);
}

// Assert ----------------------------------------------------------------------
int identity(int x) {
    return x;
}

void assert_equal(int a, int b) {
    // Potrzeba funkcji, a nie literału, gdyż w przeciwnym wypadku byśmy
    // przypisali b do a. 
    identity(a) := b;
}

void assert_equal2(int a, int b) {
    // Druga wersja tego samego.
    true := (a == b);
}

void assert_notequal(int a, int b) {
    false := (a == b);
}

void assert(boolean condition) {
    true := condition;
}

void assert_false() {
    0 := 1;
}

// Utils -----------------------------------------------------------------------
void swap(int ref a, int ref b) {
    [a, b] := [b, a];
}

void printint(int x) {
    print(tostring(x));
    print(" ");
}

// Bubble sort tuple -----------------------------------------------------------
int getsetnth(tuple<int, int, int, int> ref krotka, int n, int newval) {
    int x, a, b, c;
    if (n == 0) {
        [x, a, b, c] := krotka;
        krotka := [newval, a, b, c];
    } else if (n == 1) {
        [a, x, b, c] := krotka;
        krotka := [a, newval, b, c];
    } else if (n == 2) {
        [a, b, x, c] := krotka;
        krotka := [a, b, newval, c];
    } else if (n == 3) {
        [a, b, c, x] := krotka;
        krotka := [a, b, c, newval];
    } else {
        assert_false();
    }
    return x;
}

int getnth(tuple<int, int, int, int> ref krotka, int n) {
    int x, a;
    if (n == 0)
        [x, a, a, a] := krotka;
    else if (n == 1)
        [a, x, a, a] := krotka;
    else if (n == 2)
        [a, a, x, a] := krotka;
    else if (n == 3)
        [a, a, a, x] := krotka;
    else
        assert_false();
    return x;
}

void sort(tuple<int, int, int, int> ref krotka) {
    int i = 4, j;
    while (i > 0) {
        j = i;
        while (j < 4) {
            int x, y;
            [x, y] := [getnth(krotka, j - 1), getnth(krotka, j)];
            if (x > y)
                swap(x, y);
            getsetnth(krotka, j-1, x);
            getsetnth(krotka, j, y);
            j = j + 1;
        }
        i = i - 1;
    }
}

void sort_test() {
    int a, b, c, d;
    tuple<int, int, int, int> tup = [2, 1, 3, 7];
    sort(tup);
    // Nie można porównać bezpośrednio krotek, więc nie mogę użyć funkcji assert.
    [1, 2, 3, 7] := tup;

    tup = [ 4738, -44, -44, -55];
    sort(tup);
    [-55, -44, -44, 4738] := tup;
    print("Sort test succeeded.\n");
}

// Miller Rabin primality testing ----------------------------------------------
int modpow(int base, int exp, int mod) {
    assert(mod > 0);
    assert(exp >= 0);
    if (exp == 0)
        return 1;
    if (exp % 2 == 1)
        return (base * modpow(base, exp - 1, mod)) % mod;
    int m = modpow(base, exp / 2, mod);
    return (m*m) % mod;
}

boolean modpow_test() {
    assert_equal(modpow(2, 0, 5000), 1);
    assert_equal(modpow(2, 9, 500), 12);
    // 2147483647 == 2**31 - 1 (liczba pierwsza)
    assert_equal(modpow(200001, 2147483647, 2147483647), 200001);
    print("Modpow test succeeded.\n");
}

boolean miller_rabin_round(int p, int a, tuple<int, int> r_d_decomposition) {
    int r, d;
    [r, d] := r_d_decomposition;
    int x = modpow(a, d, p);
    if (x == 1 || x == p - 1) {
        return false;
    }
    while (r >= 1) {
        x = modpow(x, 2, p);
        if (x == p - 1) 
            return false;
        r = r - 1;
    }
    return true;
}

tuple<boolean, int> miller_rabin(int p, int tries) {
    int r = 0, d = p - 1, i = 0;
    while (d % 2 == 0) {
        d = d / 2;
        r = r + 1;
    }
    while (i < tries) {
        int a = 0;
        while (true) {
            a = rand() % p;
            if (a >= 2 && a <= p - 2)
                break;
        }
        if (miller_rabin_round(p, a, [r, d]))
            break;
        i = i + 1;
    }
    return [i == tries, tries];
}

void miller_rabin_test() {
    boolean is_prime; int tries;
    [is_prime, tries] := miller_rabin(8, 50);
    assert(!is_prime);
    [is_prime, tries] := miller_rabin(9, 50);
    assert(!is_prime);
    [is_prime, tries] := miller_rabin(11, 50);
    assert(is_prime);
    [is_prime, tries] := miller_rabin(2147483647, 50);
    assert(is_prime);
}

void main() {
    modpow_test();
    sort_test();
    srand();
    miller_rabin_test();
}
