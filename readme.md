This repository holds the exercises I did in order to get up speed in pathfinding in games. I started by doing HackerRank Astar challenges and ended up doing half of Coursera's [Algorithms I](https://www.coursera.org/course/algs4partI) & [Algorithms II](https://www.coursera.org/course/algs4partII). Apart from the being Java focused, the courses themselves are quite excellent. As a self taught programmer, I never realized how much I was missing until I took them and I feel I gained a lot from just watching the videos.

The only thing remaining for me right now is to add Jump Point Search for Pacman. The GVGAI style games could benefit significantly from it.

The N puzzle example, shows how to apply a tabular priority queue along with Manhattan distance and the linear conflicts heuristics for Astar search. The tabular queue is quite a bit faster than a tree based one.

I'll try IDSA* and the pattern database on the N puzzle and after that I'll use a tabular queue on Pacman as my next target. Jump Point Search comes after that.

The final endgame for this repository is for me to use DiffSharp or the Spiral library and implement a neural net for the heuristic function.