This repository holds the exercises I did in order to get up speed in pathfinding in games. I started by doing HackerRank Astar challenges and ended up doing half of Coursera's [Algorithms I](https://www.coursera.org/course/algs4partI) & [Algorithms II](https://www.coursera.org/course/algs4partII). Apart from the being Java focused, the courses themselves are quite excellent. As a self taught programmer, I never realized how much I was missing until I took them and I feel I gained a lot from just watching the videos.

The only thing remaining for me right now is to add Jump Point Search for Pacman. The GVGAI style games could benefit significantly from it.

The N puzzle example, shows how to apply a tabular priority queue along with Manhattan distance and the linear conflicts heuristics for Astar search. The tabular queue is quite a bit faster than a tree based one.

I'll try IDSA* and the pattern database on the N puzzle and after that I'll use a tabular priority queue on Pacman as my next target. Jump Point Search comes after that.

The final endgame for this repository is for me to use DiffSharp or the Spiral library and implement a neural net for the heuristic function.

UPDATE 3/21/2016:

I am done. I got JPS working and in the end, I've decided to use [Fringe Search](https://en.wikipedia.org/wiki/Fringe_search) the less known alternative to IDSA* and A*. JPS works well, but as it searches such large swaths where Fringe Search does not, it tallies up to FS being a tad faster. I've been skeptical of JPS being so much faster than Astar and I think this confirms it. Fringe Search is also a much better idea than the tabular priority queue, so much that I've completely forgotten about the tabular PQ - it is not worth testing.

FS is also a more general method, well worth knowing. When I am done with the GVGAI library, I'll come back to the N puzzle aiming to bring to bear all the power of neural nets upon the problem with it as the workhorse.

Also in permutation_hashing_encoder_decoder_v2.fsx there are functions for optimally packing permutations (with and without repetitions), which might be of interest to those working on the [pattern database](https://heuristicswiki.wikispaces.com/pattern+database) for the N puzzle problem. It took me a while to figure out and it is absolutely necessary to make storing the patterns efficient. My first idea would have been to use a bigass trie as the data structure. Thanks to some helpful people, I found this much better solution.