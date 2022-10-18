# Grammar of Grammars

|     |                 |        |                                         |
| --- | --------------- | ------ | --------------------------------------- |
| 1   | Grammar         | &rarr; | ProductionList                          |
| 2   | ProductionList  | &rarr; | ProductionSet SEMICOLON ProductionList' |
| 3   | ProductionList' | &rarr; | ProductionSet SEMICOLON ProductionList' |
| 4   |                 | \|     | EPSILON                                 |
| 5   | ProductionSet   | &rarr; | SYMBOL DERIVE Rhs ProductionSet'        |
| 6   | ProductionSet'  | &rarr; | ALSODERIVS Rhs ProductionSet'           |
| 7   |                 | \|     | EPSILON                                 |
| 8   | Rhs             | &rarr; | SymbolList                              |
| 9   |                 | \|     | EPSILON                                 |
| 10  | SymbolList      | &rarr; | SYMBOL SymbolList'                      |
| 11  | SymbolList'     | &rarr; | eps                                     |
| 12  |                 | \|     | SymbolList                              |

## EVERYTHING BELOW IS WRONG

## Follow

|     |                 |                       |
| --- | --------------- | --------------------- |
| 1   | Grammar         | EOF                   |
| 2   | ProductionList  | EOF                   |
| 3   | ProductionList' | EOF                   |
| 4   | ProductionSet   | SEMICOLON             |
| 5   | ProductionSet'  | SEMICOLON             |
| 6   | Rhs             | ALSODERIVS, SEMICOLON |
| 7   | SymbolList      | ALSODERIVS, SEMICOLON |


## First

|     |                 |                     |
| --- | --------------- | ------------------- |
| 1   | Grammar         | SYMBOL              |
| 2   | ProductionList  | SYMBOL              |
| 3   | ProductionList' | SYMBOL, EPSILON     |
| 4   | ProductionSet   | SYMBOL              |
| 5   | ProductionSet'  | ALSODERIVS, EPSILON |
| 6   | Rhs             | SYMBOL, EPSILON     |
| 7   | SymbolList      | SYMBOL              |

The first of a terminal is itself

<!--
## Next
|     |                 |                       |
| --- | --------------- | --------------------- |
| 1   | Grammar         | SYMBOL                |
| 2   | ProductionList  | SYMBOL                |
| 3   | ProductionList' | SYMBOL, EOF           |
| 4   |                 | EOF                   |
| 5   | ProductionSet   | SYMBOL                |
| 6   | ProductionSet'  | ALSODERIVS            |
| 7   |                 | SEMICOLON             |
| 8   | Rhs             | SYMBOL                |
| 9   |                 | ALSODERIVS, SEMICOLON |
| 10  | SymbolList      | SYMBOL                |
| 11  |                 | SYMBOL                |
-->
???

## Predict table
|                 | Semicolon | Derives | AlsoDerives | Symbol | EOF |
| --------------- | --------- | ------- | ----------- | ------ | --- |
| Grammar         |           |         |             | 1      |     |
| ProductionList  |           |         |             | 2      |     |
| ProductionList' |           |         |             | 3      | 4   |
| ProductionSet   |           |         |             | 5      |     |
| ProductionSet'  | 7         |         | 6           |        |     |
| Rhs             |           |         |             | 8      |     |
| SymbolList      |           |         |             |        |     |



