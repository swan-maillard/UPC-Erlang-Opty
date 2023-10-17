#!/usr/bin/env python3

# Runs experiments with different parameters and parses the output of the erl program

import subprocess
import re

from dataclasses import dataclass


@dataclass
class ClientResult:
    client_id: int
    total_transactions: int
    valid_transactions: int


@dataclass
class Result:
    n_clients: int
    n_entries: int
    n_reads: int
    n_writes: int
    max_time: int
    clients: list[ClientResult]


def run_erl(n_clients, n_entries, n_reads, n_writes, max_time):
    cmd = f'erl -noshell -eval "opty:start({n_clients}, {n_entries}, {n_reads}, {n_writes}, {max_time})." -s init stop'
    out = subprocess.check_output(cmd, shell=True)
    return out.decode("utf-8")


def parse_erl_output(output: str) -> list[ClientResult]:
    lines = output.split("\n")
    clients = []
    for line in lines:
        if "Transactions TOTAL" in line:
            client_id = int(line.split(":")[0])
            total_transactions = int(re.search(r"TOTAL:(\d+)", line).group(1))
            valid_transactions = int(re.search(r"OK:(\d+)", line).group(1))
            clients.append(
                ClientResult(client_id, total_transactions, valid_transactions)
            )

    return clients


def run_experiment(n_clients, n_entries, n_reads, n_writes, max_time) -> Result:
    output = run_erl(n_clients, n_entries, n_reads, n_writes, max_time)
    clients = parse_erl_output(output)
    return Result(n_clients, n_entries, n_reads, n_writes, max_time, clients)


if __name__ == "__main__":
    result = run_experiment(5, 5, 10, 10, 1)
    from pprint import pprint

    pprint(result)
