# Functional-optimization-system-with-capacity-and-autonomy-constraints

## Works
This project aims to solve a tour optimization problem with capacity and autonomy constraints.

Study of an initial solution, then application of an intra-tour 2-Opt, followed by a Tabu Search with Relocate and Swap (inter/intra-tour) movements.

Cost: total distance + penalties for overcapacity and autonomy.

Movements: • Relocate: move a client from one tour to another.

• Swap: swap two clients between two tours or within the same tour.

• 2-Opt: intra-toured local improvement after each accepted move.

## How to use ?
**Use a JVM or an online interpreter as [Scastie](https://scastie.scala-lang.org)**