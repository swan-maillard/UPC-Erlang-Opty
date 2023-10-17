#!/usr/bin/env python3

# Runs experiments with different parameters and parses the output of the erl program

import subprocess
import sys
import os
import re
import time

# Runs the erl program with the given parameters


def run_erl(n_clients, n_entries, n_reads, n_writes, max_time):
    cmd = 'erl -noshell -eval "opty:start(1, 1, 1, 1, 1)." -s init stop'
    out = subprocess.check_output(cmd, shell=True)
    print(out)
    return out.decode("utf-8")


run_erl(1, 1, 1, 1, 1)
