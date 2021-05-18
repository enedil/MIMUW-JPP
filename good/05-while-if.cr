void main() {
   int i = 0; 
   while (i < 13) {
       if (i % 5 == 0) {
           print("fizz\n");
       }
       if (i % 3 == 0) {
           print("buzz\n");
       } else {
           print("ten fizz buzz nie działa chyba: ");
           print(tostring(i));
           print("\n");
       }
       i = i + 1;
   }
   print("\n");
}
