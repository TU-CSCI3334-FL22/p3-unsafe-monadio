#!/usr/bin/env python3


from pathlib import Path
from subprocess import PIPE, run

grammars = Path('grammars').iterdir()
run(['make', 'build']).check_returncode()

for g in grammars:
    if 'nonLL1' in str(g) or 'invalid' in str(g):
        print(f'Skipping {str(g)}')
        continue

    run(f'./Main -w {str(g)} > w.out', shell=True).check_returncode()
    run(f'./Main {str(g)} > nw.out', shell=True).check_returncode()

    run(f'diff w.out nw.out', shell=True).check_returncode()

    print(f'Seems good: {str(g)}')
