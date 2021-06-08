// wbudowane funkcje tostring oraz print:
// tostring ma typ function<string, int> (tzn zwraca stringa, przyjmuje jednego inta)
// print ma typ function<void, string>
void main() {
    boolean b = true;
    string s = "napis\n";
    int i = 42;
    print(s);
    print(tostring(42));
    // błedne wykonania (nie przejdą kontroli typów):
    //     tostring(true)
    //     tostring("string")
    //     print(true)
    //     print(42) 
    if (b || (!b)) {
        print("\nLogika klasyczna działa.\n");
    }
}
