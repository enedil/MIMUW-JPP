# Cerber

Cerber is an imperative programming language implemented in Haskell.
Examples are in directory [good](michal_radwanski/good/).

The project can be compiled with `make`. Bootstrapping compilation from start requires BNFC, with the version available from Cabal.

Rest of the description is in Polish:


Mój język bazowany jest na języku Latte. Nie posiada operatorów inkrementacji i dekrementacji. Typowanie jest stat
czne, tak samo jak zakres widoczności. Punktem wejścia do programu napisanego w Ceberze jest funkcja `void main()`
(bez argumentów).
W przeciwieństwie (być może) do Latte, w Cerberze wartości do funkcji są przekazywane przez kopię domyślnie. Jeśli
zamierzamy przekazać referencję, robimy to to poprzez podanie słowa kluczowego `ref` pomiędzy typem parametru form
lnego, a jego nazwą. Referencje mają semantykę podobną do C++, tzn jeśli `x` była przekazana do funkcji przez refe
encję, to przypisanie do `x` spowoduje zmianę tej zmiennej, a nie jedynie modyfikację tego, z jakim obiektem związ
na jest nazwa `x` (jak by to było w Pythonie, albo w Javie).

Poza typami z Latte (czyli `int`, `boolean`, `string`), obsługiwane są dowolnie zagnieżdżone krotki. Krotki można 
worzyć naturalną składnią: `(14, "1111", (x*12))`. Typ krotki podajemy tak jak w szablonach C++, tzn przykładowo `
uple<int, tuple<tuple<>, string>>`.
Drugim dodatkowym typem, są generatory. Generatory tworzy się poprzez definiowanie funkcji, w której ciele znajduj
 się `yield expr`. Generatory zachowują się jak wartości, można je przypisywać do zmiennych. Są jednak jedynym typ
m, który w przypadku przypisań/przekazań jako argument, zawsze przyjmuje semantykę referencji (tzn. nie da się sko
iować stanu generatora). Typ generatora podaje się poprzez `generator<typ_w_środku>`. Generatory mogą być zagnieżd
one, aczkolwiek nie ma to wielkiego sensu. Wartość z generatora można uzyskać wołając funkcję `next`, albo za pomo
ą iteracji: `for nazwa in generator()`. Jeśli w ciele generatora wystąpi bezargumentowa instrukcja `return`, gener
tor kończy działanie, a każde kolejne zawołanie `next` skutkuje błędem czasu wykonania. Jeśli natomiast zajdzie `r
turn expr`, wówczas komunikatem błędu czasu wykonania, jest tekstowa reprezentacja wyrażenia `expr`.
Istnieje też możliwość tworzenia funkcji anonimowych (domknięć), za pomocą składni `[int arg1, string ref arg2] : 
eturn_type ~> { block; }`.

W Cerberze, pętle `while` oraz `for x in generator` można przerwać za pomocą instrukcji `break` oraz `continue`.

Istnieją dostępne funkcje:
 - `void print(string text)`
 - `string tostring(int num)`
 - `T next(generator<T> gen)`

Na krotkach można wykonywać "pattern matching", wspierana jest składnia
```
(a, b, (c, d, e)) := tuple_value;
```
W przeciwieństwie do Pythona jednak, nawiasy zewnętrzne są obowiązkowe. Zmienne do których przypisujemy muszą być zadeklarowane, i być odpowiedniego typu.
Zachowanie programu, który używa niezainicjalizowanej zmiennej nie jest zdefiniowane.

# Przykłady
```
tuple<int, int> f1(int a) {
    yield (a, a*2);
}
```


```
int f2() {
    int a, b;
    (a, b) := next(f1(13));
    return a+b;
}
```

```
void f3() {
    for x in f1(44) {
        int a, b;
        (a, b) := x; 
        print("a+b = "); print(tostring(a+b)); print("\n");
    }
}
```

```
int f4() { 
    function<int, string> f = [string a] : int ~> { return 15; }; 
    return f();
}
```

```
function<int, int> f5(int a) {
    return [int ref b] : int ~> {
        return a + b;
    }; 
}
```

```
int f6() {
    int i = 0;
    while (true) {
        if (i > 2) break;
        continue;
        i = i + 1;
    }
}
```

