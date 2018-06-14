# Loom - Push Pelabel
The loom algorithm is a push relabel implementation for solving the 
[max flow problem](https://en.wikipedia.org/wiki/Push%E2%80%93relabel_maximum_flow_algorithm#Practical_implementations) The algorithm is tested on directed graphs.

### Definitions
A network $N$ is defined by a directed graph $G$ its source and sink $s,t$ and the capacities $C : E \rightarrow R^+$ 
Following push-relabel's terminology the residual graph $R$ is a network containing both original edges of $G$ (forward edges) and backward (reverse) edges, with the additional properties of **preflow** $F : E_R \rightarrow R^+ $ on the forward edges and residual capacities $C_R$ on all edges of $R$ as well as the following properties on the vertices:
- Height $H: V \rightarrow R^+$ which determines whether preflow can be pushed through an edge.
- Excess $X:  V \rightarrow R^+$ recording the excess flow of each vertex. 
When the algorithm terminates all excess is $0$ and the preflow of each edges is
the actual maximum flow of $N$.

### Operations
The main difference in the definitions lies in the split of the PR push operation
into two, depending on whether it is performed in a forward edge or backward
edge. The former operation is called **push** (from now on, *push* will be used in
this context) and the latter **pull**.
Relabel is the usual PR adjusting of heights.

*PR* guaranties that there is a cut between the source and sink in the residual
graph partitioning $R$ into $S$ and $T$ vertices.

The algorithm is iterative and each iteration consists of three steps
1. global-relabel
2. global-push
3. global-pull

