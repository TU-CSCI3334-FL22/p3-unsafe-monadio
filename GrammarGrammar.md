# Grammar of Grammars

|     |                 |        |                                         |
| --- | --------------- | ------ | --------------------------------------- |
| 1   | Grammar         | &rarr; | ProductionList                          |
| 2   | ProductionList  | &rarr; | ProductionSet SEMICOLON ProductionList' |
| 3   | ProductionList' | &rarr; | ProductionSet SEMICOLON ProductionList' |
| 4   |                 | \|     | $\epsilon$                              |
| 5   | ProductionSet   | &rarr; | SYMBOL DERIVE Rhs ProductionSet'        |
| 6   | ProductionSet'  | &rarr; | ALSODERIVS Rhs ProductionSet'           |
| 7   |                 | \|     | $\epsilon$                              |
| 8   | Rhs             | &rarr; | SymbolList                              |
| 9   |                 | \|     | EPSILON                                 |
| 10  | SymbolList      | &rarr; | SYMBOL SymbolList'                      |
| 11  | SymbolList'     | &rarr; | $\epsilon$                              |
| 12  |                 | \|     | SymbolList                              |

## EVERYTHING BELOW IS fixed

## First

|     |  Nonterminal/Termial       |  First                  |
| --- | -------------------------- | ------------------------|
| 1   | Grammar                    | SYMBOL                  |
| 2   | ProductionList             | SYMBOL                  |
| 3   | ProductionList'            | SYMBOL, $\epsilon$      |
| 4   | ProductionSet              | SYMBOL                  |
| 5   | ProductionSet'             | ALSODERIVS, $\epsilon$  |
| 6   | Rhs                        | SYMBOL, EPSILON.        |
| 7   | SymbolList                 | SYMBOL                  |
| 8   | SymbolList'                | SYMBOL, $\epsilon$      |

The first of a terminal is itself

## Follow

|     | Nonterminal     |  Follow                          |
| --- | --------------- | -------------------------------- |
| 1   | Grammar         | EOF                              |
| 2   | ProductionList  | EOF                              |
| 3   | ProductionList' | EOF                              |
| 4   | ProductionSet   | SEMICOLON                        |
| 5   | ProductionSet'  | SEMICOLON                        |
| 6   | Rhs             | SEMICOLON, ALSODERIVS            |
| 7   | SymbolList      | SEMICOLON, ALSODERIVS            |
| 8   | SymbolList'     | SEMICOLON, ALSODERIVS            |



<!--
## Next
| Rule |                 |  Next                             |
| ---- | --------------- | --------------------------------- |
| 1    | Grammar         | SYMBOL                            |
| 2    | ProductionList  | SYMBOL                            |
| 3    | ProductionList' | SYMBOL                            |
| 4    |                 | $\epsilon$, EOF                   |
| 5    | ProductionSet   | SYMBOL                            |
| 6    | ProductionSet'  | ALSODERIVS                        |
| 7    |                 | $\epsilon$, SEMICOLON             |
| 8    | Rhs             | SYMBOL                            |
| 9    |                 | EPSILON                           |
| 10   | SymbolList      | SYMBOL                            |
| 11   | SymbolList'     | SYMBOL                            |
| 12   |                 | $\epsilon$, SEMICOLON, ALSODERIVS |
-->
???

## Predict table
|                 | Semicolon | Derives | AlsoDerives | Symbol | EPSILON | EOF |
| --------------- | --------- | ------- | ----------- | ------ | ------- | --- |
| Grammar         |           |         |             | 1      |         |     |
| ProductionList  |           |         |             | 2      |         |     |
| ProductionList' |           |         |             | 3      |         | 4   |
| ProductionSet   |           |         |             | 5      |         |     |
| ProductionSet'  | 7         |         | 6           |        |         |     |
| Rhs             |           |         |             | 8      | 9       |     |
| SymbolList      |           |         |             | 10     |         |     |
| SymbolList'     | 12        |         | 12          | 11     |         |     |



