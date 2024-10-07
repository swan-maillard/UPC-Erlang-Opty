# Opty - A Distributed Transaction Simulation Framework

## Description

**Opty** is an Erlang-based framework designed to simulate distributed transactions over a simple database. The system simulates multiple clients interacting with a server to perform read and write operations on a shared data store. The framework allows for concurrent transactions, supports commit/abort operations, and provides rudimentary validation mechanisms to ensure data integrity in a distributed environment.

The simulation allows you to specify the number of clients, entries in the store, as well as the ratio of read/write operations. It can be used for educational purposes, testing distributed transaction algorithms, and understanding concurrency control mechanisms.

## Features

- Simulates a distributed client-server architecture for performing transactions.
- Supports configurable read and write operations for each client.
- Provides transaction commit and validation mechanisms.
- Allows for customizable simulation durations and parameters.
- Clients interact with a shared store via a server and handler processes.
- Implements basic concurrency and conflict resolution logic.

## Architecture Overview

The system consists of the following main components:

1. **Server**: Manages the interaction between clients and the shared store.
2. **Clients**: Simulate transactions by performing reads and writes on the store via a handler.
3. **Handlers**: Mediate client operations, managing reads and writes and interacting with the validator and store.
4. **Validator**: Ensures that transactions are valid before committing the results.
5. **Store**: Represents the shared data that clients operate on.

Clients initiate transactions with the server, and transactions are validated before committing the results. The validator checks if any conflicts occurred during the execution of the transaction and decides whether to commit or abort it.

## Usage

### Starting the Simulation

To start the simulation, call the `start/5` function in the `opty.erl` module with the following parameters:

```erlang
start(Clients, Entries, Reads, Writes, Time).
```

- **Clients**: Number of clients performing transactions.
- **Entries**: Number of entries in the shared store.
- **Reads**: Number of read operations per transaction.
- **Writes**: Number of write operations per transaction.
- **Time**: Duration of the simulation in milliseconds.

Example:

```erlang
opty:start(10, 100, 5, 2, 10000).
```

This example will start the simulation with 10 clients, 100 entries in the store, 5 reads, 2 writes per transaction, and a simulation duration of 10 seconds.

### Stopping the Simulation

The simulation stops automatically after the specified time. However, you can manually stop the clients and server by calling the `stop/1` function:

```erlang
stop(ClientList).
```

This will terminate all clients and stop the server.

### Transaction Execution

During the simulation, each client performs a series of transactions that include reading and writing data entries. Each transaction is processed by the server, and depending on the outcome, the client can either successfully commit the transaction or abort it.

## Client and Server Interaction

The interaction between the clients and the server is based on a message-passing mechanism:

1. A client requests a transaction by sending an `{open, Client}` message to the server.
2. The server responds by providing the client with access to a transaction handler.
3. The handler facilitates the reads and writes on the store for the client.
4. Once the transaction is complete, the client attempts to commit the transaction.
5. The validator checks the transaction for conflicts and determines whether it should be committed or aborted.

## Transaction Handling

### Read and Write Operations

Each client can perform a combination of read and write operations on the shared store entries. The number of reads and writes is configurable for each client when the simulation is started.

- **Read**: A client sends a `{read, Ref, EntryNum}` message to the handler, which either returns the value from the write-set or requests it from the store.
- **Write**: A client sends a `{write, EntryNum, Value}` message to the handler, which records the write in the local write-set.

### Commit and Validation

After performing the read and write operations, the client sends a `{commit, Ref}` message to the handler. The handler forwards this to the validator, which checks for any conflicts with other transactions.

If the transaction is valid, it is committed to the store; otherwise, the transaction is aborted, and no changes are made.

## Code Structure

- **opty.erl**: Manages the simulation lifecycle, starting and stopping clients and servers.
- **server.erl**: Handles client requests for transactions and manages the store.
- **client.erl**: Simulates client behavior, performing transactions through the server.
- **handler.erl**: Handles transaction operations (read/write/commit) for each client.
- **validator.erl**: Validates transactions and determines if they should be committed or aborted.
- **store.erl**: Represents the shared data structure that clients read from and write to.

## Authors

- Swan Maillard (maillard.swan@gmail.com)
- Felix Mühlenberend

## License

This project is licensed under the MIT License. Please consult the `LICENSE` file for more information.
