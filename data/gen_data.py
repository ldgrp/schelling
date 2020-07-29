import random
import sys

if __name__ == '__main__':
    filename = sys.argv[1]

    # width and height of the grid
    w, h = int(sys.argv[2]), int(sys.argv[3])

    # percent empty
    empty = float(sys.argv[4])

    # X (vs O) ratio
    ratio = float(sys.argv[5])

    # population
    pop = w * h * (1 - empty)
    # x population
    nx = int (pop * ratio)
    # o population
    no = int (pop * (1 - ratio))

    # spaces
    nspace = (w * h) - (nx + no)

    out = list(('X' * nx) + ('O' * no) + (' '  * nspace))
    random.shuffle(out)

    out2d = [''.join(out[i:i+w]) for i in range (0, len(out), w)]

    with open(filename, 'w') as f:
        f.writelines(o + '\n' for o in out2d)
