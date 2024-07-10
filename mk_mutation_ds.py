#!/usr/bin/env python3
import json
import subprocess
import sys
import multiprocessing
import tqdm

def run_mutator(decl: tuple[str, str]):
    p = subprocess.Popen(['ocaml/bin/fstar_mutator.exe'], encoding='utf-8', stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    stdout, _ = p.communicate(decl[1])
    try:
        return {'name': decl[0], **json.loads(stdout)}
    except:
        return {'name': decl[0], 'error': stdout}

if __name__ == '__main__':
    pool = multiprocessing.Pool()

    defs: dict[str, str] = {}
    for fn in sys.argv[1:]:
        j = json.load(open(fn))
        for d in j:
            defs[d['name']] = d['source_definition']
        del j

    sys.stdout.write('[')
    for i, j in enumerate(tqdm.tqdm(pool.imap(run_mutator, defs.items()), total=len(defs))):
        if i > 0: sys.stdout.write(',\n')
        sys.stdout.write(json.dumps(j))
    sys.stdout.write(']')
    sys.stdout.flush()
