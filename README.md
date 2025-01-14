# OCaml High-Performance Orderbook

A high-performance order book implementation in OCaml using Red-Black Binary Search Trees (BST) for optimal time.

## Features

* Red-Black BST implementation for O(log n) operations which has consistent performance across all operations (e.g. search,insert,delete,etc.)
* Fast order matching engine optimized for high-frequency trading environments
* Support for limit and market orders
* Efficient order cancellation and modification capabilities

## Installation

### Prerequisites

* OCaml (version 5.2.0 or higher)
* opam (OCaml package manager)
* dune (build system)

### Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/yourusername/ocaml-orderbook-engine.git
   cd ocaml-orderbook-engine
   ```

2. Create a local switch with OCaml 5.2.0:
   ```bash
   opam switch create . ocaml-compiler.5.2.0
   ```

3. Install dependencies:
   ```bash
   opam install ocaml-lsp-server odoc ocamlformat utop
   ```

## Usage

Build the project using dune:

```bash
dune build
```

To run the main executable:

```bash
dune exec bin/main.exe
```
