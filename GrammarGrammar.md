# Grammar of Grammars

|     |                 |        |                                         |
| --- | --------------- | ------ | --------------------------------------- |
| 1   | Grammar         | &rarr; | ProductionList                          |
| 2   | ProductionList  | &rarr; | ProductionSet SEMICOLON ProductionList' |
| 3   | ProductionList' | &rarr; | ProductionSet SEMICOLON ProductionList' |
| 4   |                 | \|     | $\epsilon$                                |
| 5   | ProductionSet   | &rarr; | SYMBOL DERIVE Rhs ProductionSet'        |
| 6   | ProductionSet'  | &rarr; | ALSODERIVS Rhs ProductionSet'           |
| 7   |                 | \|     | $\epsilon$                                 |
| 8   | Rhs             | &rarr; | SymbolList                              |
| 9   |                 | \|     | EPSILON                                 |
| 10  | SymbolList      | &rarr; | SYMBOL SymbolList                       |
| 11  |                 | \|     | SYMBOL                                  |


