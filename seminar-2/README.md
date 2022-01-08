# Seminarska naloga 2

> Namen seminarske naloge je implementacija in uporaba preiskovalnih algoritmov na domeni iskanja poti v labirintu. Vsak vhodni labirint vsebuje eno začetno in eno ciljno polje. Prav tako, vsak labirint vsebuje vsaj eno polje z zakladom. Želimo poiskati čim cenejšo pot od začetnega do enega izmed ciljnih polj tako, da na poti poberemo vse zaklade. Premiki skozi steno (polja z oznako -1) niso dovoljeni. Pozitivne vrednosti v polju predstavljajo ceno prehoda preko tega polja. Prehodi skozi specialna polja (začetno, ciljno, ter polja z zakladom) imajo ceno 0.

Primer izrisanega labirinta z skripto `draw.R`:

![](./images/1.png)

## Algoritem

> Pomembno vozlisce je bodisi zacetno ali koncno vozlisce ali pa vozlisce z zakladom.

- izracunaj najkrajse poti med vsemi pomembnimi vozlisci z uporabo poljubnega iskalnega algoritma (BFS, DFS, A*,...).
- uredi dobljene poti v tako zaporedje, ki minimizira ceno oz. razdaljo, s tem da je zacetno vozlisce na zacetku in koncno vozlisce na koncu tega zaporedja