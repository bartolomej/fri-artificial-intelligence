import java.util.*;

/**
 * Algorithms for solving travelling salesman problem on a given subgraph.
 *
 * Every function receives a subgraph G1 of the original labyrinth graph G.
 * Subgraph is a fully connected graph of nodes, where each node is a vertex of interest (treasure or start/end node),
 * and each edge is the combined cost of traveling to a given vertex.
 *
 * The goal is to find an optimal sequence of vertices that minimizes the cost needed to traverse all the nodes in subgraph.
 */
public class TSP {

    /**
     * Always chooses the current optimal neighbour.
     */
    public static Integer[] greedy(Graph graph) {
        Stack<Integer> path = new Stack<>();
        Set<Integer> visited = new TreeSet<>();
        path.push(graph.start);
        visited.add(graph.start);
        while (path.peek() != graph.end) {
            int node = path.peek();
            int min = -1;
            for (int n = 0; n < graph.matrix[    node].length; n++) {
                int nWeight = graph.matrix[node][n];
                int minWeight = min >= 0 ? graph.matrix[node][min] : Integer.MAX_VALUE;

                if (visited.size() < graph.nodes - 1 && n == graph.end) {
                    continue;
                }
                if (nWeight > 0 && nWeight < minWeight && !visited.contains(n)) {
                    min = n;
                }
            }
            visited.add(min);
            path.push(min);
        }
        return path.toArray(new Integer[0]);
    }
}
