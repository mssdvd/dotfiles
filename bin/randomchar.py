#!env /usr/bin/python

import random
import string


def random_char(file_name='testrandom', row=1000000, column=80):
    fout = open(file_name, 'w')
    for i in range(row):
        for l in range(column):
            fout.write(random.choice(string.ascii_letters))
        fout.write('\n')
    fout.close()


def main():
    random_char('testrandom', 10000000000, 200)


if __name__ == '__main__':
    main()
