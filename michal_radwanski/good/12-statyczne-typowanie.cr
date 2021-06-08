// Niestety nie wiem jak skutecznie opisać istnienie statycznego typowania.
// Mam nadzieję, że ten przykład dowodzi, że sprawdzanie typów nie odbywa się w
// momencie interpretacji.
void main() {
    if (false) {
        // To spowoduje błąd, mimo, że nigdy się nie wykona.
        print("Tej linijki też nie będzie widać.\n");
        "a" + "b";
    }
}
