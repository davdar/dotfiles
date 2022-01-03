import subprocess
import os
import pickle
import sys

def main():
    print("↓↓↓↓ David Darais's Janky DBLP LaTeX Tool ↓↓↓↓")

    cache_file = sys.argv[1]
    list_file = sys.argv[2]
    bib_file = sys.argv[3]
    input_files = sys.argv[4:]

    os.system(f'mkdir -p {os.path.dirname(cache_file)}')
    os.system(f'mkdir -p {os.path.dirname(list_file)}')
    os.system(f'mkdir -p {os.path.dirname(bib_file)}')

    # make a dummy `list` file if there isn't already one
    if not os.path.exists(list_file):
        os.system(f'touch {list_file}')

    # read the list file
    with open(list_file, 'rb') as f:
        old_list = f.read()

    # generate a new list
    new_list = subprocess.check_output(
            f'grep -hoE "DBLP:[[:alnum:]]*/[[:alnum:]]*/[[:alnum:]-]*" {" ".join(input_files)}'
            ' | sort -u'
            ' | sed s/DBLP://',
            shell=True)

    # test if the list has changed
    if old_list == new_list:
        print("⇨ NO CHANGE TO DBLP LIST FILE")
        # touch the bib_file
        os.system(f'touch {bib_file}')
    else:
        print("⇨ NEW DBLP LIST FILE")
        # record the new list
        with open(list_file, 'wb') as f:
            f.write(new_list)


        # load the cached db
        if os.path.exists(cache_file):
            # read from disk if it exists
            with open(cache_file, 'rb') as f:
                cache = pickle.load(f)
        else:
            # create a new one otherwise
            cache = {}

        # split the list (a string) into a (Python) list of keys
        keys = new_list.decode("utf-8").splitlines()

        # for each key, look it up in the cache, or fetch it if it's not already
        # there
        with open(bib_file, 'wb') as f:
            for key in keys:
                if key in cache:
                    print(f'⇨ [CACHED] dblp.uni-trier.de/rec/{key}.bib')
                    value = cache[key]
                else:
                    print(f'⇨ FETCHING dblp.uni-trier.de/rec/{key}.bib')
                    sys.stdout.flush()
                    value = subprocess.check_output(
                            f'curl -L dblp.uni-trier.de/rec/{key}.bib', 
                            shell=True)
                    sys.stdout.flush()
                    print("⇨ RECEIVED")
                    cache[key] = value
                f.write(f'% {key}\n'.encode("utf-8"))
                f.write(value)

        # record the updated cache
        with open(cache_file, 'wb') as f:
            pickle.dump(cache, f)

    print("↑↑↑↑ David Darais's Janky DBLP LaTeX Tool ↑↑↑↑")
    sys.exit(0)

if __name__ == '__main__': main()
