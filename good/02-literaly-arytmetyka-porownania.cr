void main() {
    int a = 12;
    int b = 14;
    // Pod koniec następuje short-circuit, więc dzielenie przez zero się nie zadzieje.
    boolean bl = a > b || a < b || (a + 23 < 44) && (12 / 0 == 2);
    if (bl) {
        print("Ala ma kota");
    }
}
