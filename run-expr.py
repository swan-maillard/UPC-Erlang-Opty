#!/usr/bin/env python3

# Runs experiments with different parameters and parses the output of the erl program

from collections import defaultdict
from typing import Callable
import os
import subprocess
from itertools import chain
import pickle
import re
import numpy as np
from matplotlib import pyplot as plt
from tqdm import tqdm

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
    n_subset: int
    max_time: int
    clients: list[ClientResult]


def collect_data(
    key_func: Callable[[Result], float],
    data_func: Callable[[ClientResult], float],
    results: list[Result],
) -> dict[float, list[float]]:
    data = defaultdict(list)
    for r in results:
        data[key_func(r)].extend([data_func(c) for c in r.clients])
    return data


def _run_erl(
    n_clients: int,
    n_entries: int,
    n_reads: int,
    n_writes: int,
    max_time_ms: int,
    n_subset: int,
):
    cmd = f'erl -noshell -eval "opty:start({n_clients}, {n_entries}, {n_reads}, {n_writes}, {max_time_ms}, {n_subset})." -s init stop'
    out = subprocess.check_output(cmd, shell=True)
    return out.decode("utf-8")


def _parse_erl_output(output: str) -> list[ClientResult]:
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


def run_experiment(
    n_clients: int,
    n_entries: int,
    n_reads: int,
    n_writes: int,
    n_subset: int = 0,
    max_time_ms: int = 100,
) -> Result:
    """Run the experiment.

    Args:
        n_clients (int): Number of concurrent clients in the system
        n_entries (int): Number of entries in the store
        n_reads (int): Number of read operations per transaction
        n_writes (int): Number of write operations per transaction
        max_time (int): Duration of the experiment (in secs)

    Returns:
        Result: The result of the experiment. Containts list of ClientResult objects for every client in the system
    """

    if not n_subset:
        n_subset = n_entries
    output = _run_erl(n_clients, n_entries, n_reads, n_writes, max_time_ms, n_subset)
    clients = _parse_erl_output(output)
    return Result(
        n_clients, n_entries, n_reads, n_writes, n_subset, max_time_ms, clients
    )


def plot_experiment(
    xs: list[float],
    data: list[float],
    title: str,
    xlabel: str,
    ylabel: str,
    filename: str,
):
    plt.figure()
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.plot(xs, data)
    plt.savefig(f"{PLOT_DIR}/{filename}.png")


def plot_multistep_experiment(
    data: dict[float, list[float]],
    title: str,
    xlabel: str,
    ylabel: str,
    filename: str,
):
    data: list[tuple[float, list[float]]] = [(x, y) for x, y in data.items()]
    data.sort(key=lambda x: x[0])

    xs = [x for x, _ in data]
    y_datas = [y for _, y in data]

    means = [np.mean(d) for d in y_datas]
    standard_deviations = [np.sqrt(np.var(d)) for d in y_datas]

    plt.figure()
    plt.title(title)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.scatter(
        list(chain(*[[x] * len(y_data) for x, y_data in data])),
        list(chain(*y_datas)),
        marker="x",
        s=2,
    )
    plt.plot(xs, means, label="mean")
    plt.fill_between(
        xs,
        np.array(means) - np.array(standard_deviations),
        np.array(means) + np.array(standard_deviations),
        alpha=0.5,
        label="standard deviation",
    )
    plt.legend()
    plt.savefig(f"{PLOT_DIR}/{filename}.png")


if __name__ == "__main__":
    n_exprs = 2
    max_time_ms = 100

    os.makedirs(DATA_DIR, exist_ok=True)
    os.makedirs(PLOT_DIR, exist_ok=True)

    # The following parameters should be tested:
    # 1. Different number of concurrent clients in the system
    def test_clients():
        min_clients, max_clients = 1, 10
        clients = np.arange(min_clients, max_clients + 1)
        n_reads, n_writes, n_entires = 10, 10, 5
        experiments = list(chain(list(clients) * n_exprs))

        try:
            results = pickle.load(open(f"{DATA_DIR}/clients", "rb"))
        except FileNotFoundError:
            results = [
                run_experiment(n_clients, n_entires, n_reads, n_writes, max_time_ms)
                for n_clients in tqdm(experiments)
            ]
            pickle.dump(results, open(f"{DATA_DIR}/clients", "wb"))

        data = defaultdict(list)
        for r in results:
            data[r.n_clients].extend([c.success_rate for c in r.clients])

        plot_multistep_experiment(
            data,
            title=f"Scatter plot of client success rate for {n_reads} reads and {n_writes} writes\nruntime:{max_time_ms}ms and {n_exprs} runs",
            xlabel="Number of clients",
            ylabel="Success rate",
            filename="clients",
        )

    # 2. Different number of entries in the store
    def test_entries():
        min_entries, max_entries = 1, 10
        n_clients, n_reads, n_writes = 5, 10, 10
        entries = np.arange(min_entries, max_entries + 1)
        experiments = list(chain(list(entries) * n_exprs))

        try:
            results = pickle.load(open(f"{DATA_DIR}/entries", "rb"))
        except FileNotFoundError:
            results = [
                run_experiment(n_clients, n_entries, n_reads, n_writes, max_time_ms)
                for n_entries in tqdm(experiments)
            ]
            pickle.dump(results, open(f"{DATA_DIR}/entries", "wb"))

        data = collect_data(
            lambda r: r.n_entries,
            lambda c: c.success_rate,
            results,
        )

        plot_multistep_experiment(
            data,
            title=f"Scatter plot of client success rate for {n_reads} reads and {n_writes} writes\nruntime:{max_time_ms}ms and {n_exprs} runs",
            xlabel="Number of entries",
            ylabel="Success rate",
            filename="entries",
        )

    # 3. Different number of read operations per transaction
    def test_read_operations():
        min_reads, max_reads = 1, 10
        n_clients, n_writes, n_entries = 5, 5, 5
        reads = np.arange(min_reads, max_reads + 1)
        experiments = list(chain(list(reads) * n_exprs))

        try:
            results = pickle.load(open(f"{DATA_DIR}/reads", "rb"))
        except FileNotFoundError:
            results = [
                run_experiment(n_clients, n_entries, n_reads, n_writes, max_time_ms)
                for n_reads in tqdm(experiments)
            ]
            pickle.dump(results, open(f"{DATA_DIR}/reads", "wb"))

        data = collect_data(
            lambda r: r.n_reads,
            lambda c: c.success_rate,
            results,
        )

        plot_multistep_experiment(
            data,
            title=f"Scatter plot of client success rate for {n_clients} clients and {n_writes} writes\nruntime:{max_time_ms}ms and {n_exprs} runs",
            xlabel="Number of reads",
            ylabel="Success rate",
            filename="reads",
        )

    # 4. Different number of write operations per transaction
    def test_write_operations():
        min_writes, max_writes = 1, 10
        n_clients, n_reads, n_entries = 5, 5, 5
        writes = np.arange(min_writes, max_writes + 1)
        experiments = list(chain(list(writes) * n_exprs))

        try:
            results = pickle.load(open(f"{DATA_DIR}/writes", "rb"))
        except FileNotFoundError:
            results = [
                run_experiment(n_clients, n_entries, n_reads, n_writes, max_time_ms)
                for n_writes in tqdm(experiments)
            ]
            pickle.dump(results, open(f"{DATA_DIR}/writes", "wb"))

        data = collect_data(
            lambda r: r.n_writes,
            lambda c: c.success_rate,
            results,
        )

        plot_multistep_experiment(
            data,
            title=f"Scatter plot of client success rate for {n_clients} clients and {n_reads} reads\nruntime:{max_time_ms}ms and {n_exprs} runs",
            xlabel="Number of writes",
            ylabel="Success rate",
            filename="writes",
        )

    # 5. Different ratio of read and write operations for a fixed amount of operations per transaction
    #    (including special cases having only read or write operations)
    def test_read_write_ratio():
        min_ratio, max_ratio = 0, 1
        total_operations = 20
        n_clients, n_entries = 5, 5
        ratios = list(np.linspace(min_ratio, max_ratio, 10))
        experiments = list(chain(list(ratios) * n_exprs))

        try:
            results = pickle.load(open(f"{DATA_DIR}/read_write_ratio", "rb"))
        except FileNotFoundError:
            results = []
            for ratio in tqdm(experiments):
                n_reads = int(total_operations * ratio)
                n_writes = int(total_operations * (1 - ratio))
                if n_reads + n_writes < total_operations:
                    n_reads += 1
                assert n_reads + n_writes == total_operations
                results.append(
                    (
                        ratio,
                        run_experiment(
                            n_clients,
                            n_entries,
                            n_reads,
                            n_writes,
                            max_time_ms,
                        ),
                    )
                )
            pickle.dump(results, open(f"{DATA_DIR}/read_write_ratio", "wb"))

        data = defaultdict(list)
        for ratio, r in results:
            data[ratio].extend([c.success_rate for c in r.clients])

        plot_multistep_experiment(
            data,
            title=f"Scatter plot of client success rate for {n_clients} clients and {total_operations} operations\nruntime:{max_time_ms}ms and {n_exprs} runs",
            xlabel="Read/Write ratio",
            ylabel="Success rate",
            filename="read_write_ratio",
        )

        del data[0]
        del data[ratios[-1]]

        plot_multistep_experiment(
            data,
            title=f"Scatter plot of client success rate for {n_clients} clients and {total_operations} operations\nruntime:{max_time_ms}ms and {n_exprs} runs. Magnified.",
            xlabel="Read/Write ratio",
            ylabel="Success rate",
            filename="read_write_ratio_magnified",
        )

    # 6. Different percentage of accessed entries with respect to the total number of entries
    #    (each client accesses a randomly generated subset of the store: the number of entries in each subset
    #    should be the same for all the clients, but the subsets should be different per client and should
    #    not contain necessarily contiguous entries)
    def test_accessed_entries():
        min_subset, max_subset = 1, 100
        n_clients, n_reads, n_writes = 5, 10, 10
        subsets = np.arange(min_subset, max_subset + 1, 10)
        experiments = list(chain(list(subsets) * n_exprs))

        filename = f"subset_minss{min_subset}-maxss{max_subset}-nc{n_clients}-nr{n_reads}-nw{n_writes}-nt{n_exprs}-rt{max_time_ms}ms"

        try:
            results = pickle.load(open(f"{DATA_DIR}/{filename}", "rb"))
        except FileNotFoundError:
            results = [
                run_experiment(
                    n_clients, max_subset, n_reads, n_writes, n_subset, max_time_ms
                )
                for n_subset in tqdm(experiments)
            ]
            pickle.dump(results, open(f"{DATA_DIR}/{filename}", "wb"))

        data = collect_data(
            lambda r: r.n_subset,
            lambda c: c.success_rate,
            results,
        )

        plot_multistep_experiment(
            data,
            title=f"Scatter plot of client success rate for {n_clients} clients and {n_reads} reads, {n_writes} writes\nruntime:{max_time_ms}ms and {n_exprs} runs",
            xlabel="Size of the subset",
            ylabel="Success rate",
            filename=filename,
        )
        plt.show()

<<<<<<< HEAD
    def test_accessed_entries_multiple_nclients():
        min_subset, max_subset = 1, 20
        n_clients, n_reads, n_writes = 20, 50, 50
        subsets = np.arange(min_subset, max_subset + 1, 2)
        experiments = list(chain(list(subsets) * n_exprs))
        filename = f"subset_minss{min_subset}-maxss{max_subset}-nc{n_clients}-nr{n_reads}-nw{n_writes}-nt{n_exprs}-rt{max_time_ms}ms"

        try:
            results = pickle.load(open(f"{DATA_DIR}/{filename}", "rb"))
        except FileNotFoundError:
            results = [
                run_experiment(
                    n_clients, max_subset, n_reads, n_writes, n_subset, max_time_ms
                )
                for n_subset in tqdm(experiments)
            ]
            pickle.dump(results, open(f"{DATA_DIR}/{filename}", "wb"))

        data = collect_data(
            lambda r: r.n_subset,
            lambda c: c.success_rate,
            results,
        )

        plot_multistep_experiment(
            data,
            title=f"Scatter plot of client success rate for {n_clients} clients and {n_reads} reads, {n_writes} writes\nruntime:{max_time_ms}ms and {n_exprs} runs",
            xlabel="Size of the subset",
            ylabel="Success rate",
            filename=filename,
        )
        plt.show()

    test_entries()
    test_clients()
    test_read_operations()
    test_write_operations()
    test_read_write_ratio()
=======
    # test_entries()
    # test_clients()
    # test_read_operations()
    # test_write_operations()
    # test_read_write_ratio()
>>>>>>> 464c18d (Run with different subsets)

    test_accessed_entries()
