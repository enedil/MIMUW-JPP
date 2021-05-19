# Cerber
Rozwiązanie ogólnie nie wyróżnia się specjalnie. Najpierw jest sprawdzanie typów, sprawdzanie czy takie rzeczy jak break/continue są wyłącznie w pętli. Następnie następuje wykonanie.
Funkcje oraz zmienne rezydują w tej samej przestrzeni nazw (i dzięki temu nie było problemu z istnieniem funkcji rekurencyjnych).

W obecnej wersji nie ująłem generatorów ani funkcji anonimowych (czyli zakładając brak innych problemów, mógłbym dostać 27 punktów).

Z dodatkowych szczególnych cech, obsługiwana jest forma asercji - pattern matching na krotkach zezwala na przypisania do stałych (lub wyrażeń które nie są identyfikatorami).
Przykładowo
```
[a, 5] := [2, b];
```
spowoduje błąd czasu wykonania, jeśli `b` nie było równe 5.

Aby umożliwić konstrukcje takie jak `break`, czy `return`, funkcja wykonująca jest typu `exec :: [S.Stmt] -> JakaśMonada ReturnType` - lista ta jest pozostałymi instrukcjiami w danym bloku. Dzięki temu, można zwrócić wartość, bądź też przekazać informację, że należy bądź to przerwać pętlę, bądź zrobić `continue`.

Składnia została nieco zmodyfikowana (krotki konstruuje się za pomocą nawiasów kwadratowych), a także pozbyłem się konstrukcji związanych z generatorami (nie ma `yield` ani `for .. in`). Pozostaje jeden konflikt w składni, wynikający z istnienia `if` oraz `if else`.

Język obsługuje również zmienne globalne, jednakże nie można im nadać wartości. Jeśli zachodzi taka potrzeba, należy nadać im wartość przed pierwszym użyciem.

Odwołanie do zmiennej której nie nadano wartości, kończy się błędem wykonania.

W rozwiązaniu gdzieniegdzie używam funkcji `trace`:
```haskell
trace x y = if debug then Debug.Trace.trace x y else y -- (stała debug jest ustawiana wyżej)
```
Uznałem, że zasadne jest nie usuwać tych wstawek, na potrzeby późniejszego rozwijania.
