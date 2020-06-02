# Haskell_FNSC
UAIC_FII_PF_FNSC

## Utilizare

Se compileaza fisierul fnsc.hs si este apelat executa executabilul cu un fisier text care contine o formula logica valida
```
ghc fnsc.hs -o fnsc
./fnsc text.txt
```
Exemplu de input:
```
Forall z.Exists x . Forall y. g(x, y) <=> f( y, x) | z & t
```


## Descriere

Se parseaza sirul de caractere citit din fisier si se transforma intr-o formula logica, apoi sunt aplicate acestei formule urmatoarele operatii:
  * trecerea in FNP
  * inchiderea existentiala a formulei
  * aducerea in FNS
  * transformarea formulei in FNSC
