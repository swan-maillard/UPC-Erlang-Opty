#!/usr/bin/env python3

# Runs experiments with different parameters and parses the output of the erl program

import subprocess
import re
import os
import numpy as np
from matplotlib import pyplot as plt
import tqdm

from dataclasses import dataclass

DATA_DIR = "data"
PLOT_DIR = "plots"


@dataclass
class ClientResult:
    client_id: int
    total_transactions: int
    valid_transactions: int

    @property
    def success_rate(self):
        return self.valid_transactions / self.total_transactions


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
    """Run the experiment.

    Args:
        n_clients (int): Number of concurrent clients in the system
        n_entries (int): Number of entries in the store
        n_reads (int): Number of read operations per transaction
        n_writes (int): Number of write operations per transaction
        max_time (int): Duration of the experiment (in secs)
    """

    output = run_erl(n_clients, n_entries, n_reads, n_writes, max_time)
    clients = parse_erl_output(output)
    return Result(n_clients, n_entries, n_reads, n_writes, max_time, clients)


if __name__ == "__main__":
    n_exprs = 5
    max_time_ms = 100

    os.makedirs(DATA_DIR, exist_ok=True)
    os.makedirs(PLOT_DIR, exist_ok=True)

    # The following parameters should be tested:
    # 1. Different number of concurrent clients in the system
    def test_clients():
        min_clients, max_clients = 1, 10
        n_reads, n_writes, n_entires = 10, 10, 5

        clients = np.arange(min_clients, max_clients + 1)
        results = []
        for n_clients in tqdm.tqdm(clients):
            result = run_experiment(
                n_clients, n_entires, n_reads, n_writes, max_time_ms
            )
            results.append(result)

        avrg_client_success_rate = []
        for result in results:
            success_rate = []
            for client in result.clients:
                success_rate.append(client.success_rate)
            avrg_client_success_rate.append(np.mean(success_rate))

        # Plot the results
        plt.figure()
        plt.title(
            f"Average client success rate for {n_reads} reads and {n_writes} writes"
        )
        plt.xlabel("Number of clients")
        plt.ylabel("Success rate")
        plt.plot(clients, avrg_client_success_rate)
        plt.savefig(f"{PLOT_DIR}/clients_avg.png")

    # test_clients()

    def test_clients_comprehensive():
        min_clients, max_clients = 1, 10
        clients = np.arange(min_clients, max_clients + 1)
        n_reads, n_writes, n_entires = 10, 10, 5

        success_rates: list[tuple[int, float]] = []

        if os.path.exists(f"{DATA_DIR}/clients_comprehensive.txt"):
            with open(f"{DATA_DIR}/clients_comprehensive.txt", "r") as f:
                for line in f:
                    n_clients, success_rate = line.split(",")
                    success_rates.append((int(n_clients), float(success_rate)))
        else:
            for _ in range(n_exprs):
                for n_clients in tqdm.tqdm(clients):
                    result = run_experiment(
                        n_clients, n_entires, n_reads, n_writes, max_time_ms
                    )
                    for c in result.clients:
                        success_rates.append((n_clients, c.success_rate))

        # store results
        with open(f"{DATA_DIR}/clients_comprehensive.txt", "w") as f:
            for n_clients, success_rate in success_rates:
                f.write(f"{n_clients},{success_rate}\n")

        success_rates_dict = {n_clients: [] for n_clients in clients}
        for n_clients, success_rate in success_rates:
            success_rates_dict[n_clients].append(success_rate)

        means = [np.mean(success_rates_dict[n_clients]) for n_clients in clients]
        standard_derivations = [
            np.sqrt(np.var(success_rates_dict[n_clients])) for n_clients in clients
        ]

        # Plot the results
        plt.figure()
        plt.title(
            f"Scatter plot of client success rate for {n_reads} reads and {n_writes} writes\nruntime:{max_time_ms}ms and {n_exprs} runs"
        )
        plt.xlabel("Number of clients")
        plt.ylabel("Success rate")
        plt.scatter(*zip(*success_rates), marker="x", s=2)
        plt.plot(clients, means, label="mean")
        plt.fill_between(
            clients,
            np.array(means) - np.array(standard_derivations),
            np.array(means) + np.array(standard_derivations),
            alpha=0.5,
            label="standard derivation",
        )
        plt.legend()
        plt.savefig(f"{PLOT_DIR}/clients_comprehensive.png")
        plt.show()

    test_clients_comprehensive()

    # 2. Different number of entries in the store
    def test_entries():
        pass

    # 3. Different number of read operations per transaction
    def test_read_operations():
        pass

    # 4. Different number of write operations per transaction
    def test_write_operations():
        pass

    # 5. Different ratio of read and write operations for a fixed amount of operations per transaction
    #    (including special cases having only read or write operations)
    def test_read_write_ratio():
        pass

    # 6. Different percentage of accessed entries with respect to the total number of entries
    #    (each client accesses a randomly generated subset of the store: the number of entries in each subset
    #    should be the same for all the clients, but the subsets should be different per client and should
    #    not contain necessarily contiguous entries)
    def test_accessed_entries():
        pass
