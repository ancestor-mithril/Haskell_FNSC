# Haskell_FNSC
UAIC_FII_PF_FNSC

* My project for the Functional Programing course at UAIC FII
* Transformes a logical formula into it's Clausal Skolem normal form

## Usage
```
ghc fnsc.hs -o fnsc.exe
./fnsc.exe text.txt
```
Input example:
```
Forall z.Exists x . Forall y. g(x, y) <=> f( y, x) | z & t
```


## Exmplanation

The string from the input file is parsed and transformed into a logical formula, then the following steps are applied:
 * transformation into the prenex normal form
 * skolemization
 * transformation into Clausal Skolem normal form

