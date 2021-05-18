# `wasm-alife`

This is the implementation of the Artificial Life system for my thesis.

## Build from source

The easiest way to reproduce the environment used for development is via nix (https://nixos.org/). Type nix-shell to enter a nix shell, and continue with the next steps described below.

## Run Experiments

The following sections detail how to run the different experiments that are explained in the text.

All experiments can be run using the main function in src/Main.hs.
Drawing graphs for the different stats of the system like population and resource growth can be run using python/trackingStats.py.

### Experiments with random starting organism

Running python/countPop.py prints the ending population summed over 50 iterations.

#### Starting resources on all cells
To run the experiment with various resources on all the cells in the environment, run the main function on branch 'randomTest'.

#### Starting resources on cells with organisms only
To run the experiment with various resources cells with organisms, run the main function on branch 'randomOrgsTest'.

#### Starting with resources in all cells combined with more resources in cells with organisms
To run experiments with 2x, 3x or 4x as much resources in the cells with an organism than other run the main function from the appropriate brach.

2x: 'randomDoubleOrgsTest'

3x: 'randomTripleOrgsTest'

4x: 'randomQuadrupleTest'

### Experiments starting with and ancestor

Running python/countPop.py prints the ending population summed over 50 iterations.

There are two possible experiments to run.

For the experiment with different ancestors and the same amounts of resources on all cells in the environment, execute the main function on branch 'randomAncestorTest'.

For the experiment with different ancestors and only resources on the cell with the starting ancestor, execute the main function on branch 'randomAncestorOrgTest'.

### Mutation experiments

#### Experiment with multiplication factor in subtree mutation

To run experiments to compare the multiplication factor for the depth of the new subtree, execute the main function on branch 'differentSubtreeMutationTest'.

#### Mutation comparison

To run experiments to compare different forms of mutation from pure subtree mutation to pure point mutation, execute the main function on branch 'mutationComparisonTest'.
Ratio 0 means pure subtree mutation and ratio 4 means pure point mutation.

## Experiments with resource generators

### Short experiments with ancestor and generator

To run short exploratory experiments (100 iterations) with one ancestor and various amounts of resources and generators in the environment, execute the main function on branch 'firstResourceGeneratorTest'.

### Medium experiments with ancestor and generator

To run medium exploratory experiments (10000 iterations) with one ancestor and various amounts of resources and generators in the environment, execute the main function on branch 'secondResourceGeneratorTest'.

### Long experiments with ancestor and generator

To run long exploratory experiments (30000 iterations) with one ancestor and various amounts of resources and generators in the environment, execute the main function on branch 'thirdResourceGeneratorTest'.
