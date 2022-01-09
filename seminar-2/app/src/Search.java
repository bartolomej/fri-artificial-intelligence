import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedList;
import java.util.Queue;

public class Search {

    public static Integer[] search(int[][] graph, int startNode, int endNode) {
        boolean[] marked = new boolean[graph.length];
        int[] from = new int[graph.length];
        ArrayList<Integer> path = new ArrayList<>();

        Queue<Integer> queue = new LinkedList<Integer>();

        marked[startNode] = true;
        from[startNode] = -1;

        queue.add(startNode);

        while (!queue.isEmpty()) {
            int curNode = queue.remove();

            if (curNode == endNode) {
                path.add(curNode);

                while (true) {
                    curNode = from[curNode];
                    if (curNode != -1)
                        path.add(curNode);
                    else
                        break;
                }

                break;
            }

            for (int nextNode = 0; nextNode < graph[curNode].length; nextNode++) {
                if (graph[curNode][nextNode] >= 0 && !marked[nextNode]) {
                    marked[nextNode] = true;
                    from[nextNode] = curNode;
                    queue.add(nextNode);
                }
            }
        }
        Collections.reverse(path);
        return path.toArray(new Integer[0]);
    }

    public static void main(String[] args) {
        Integer[][] labyrinth = new Integer[][]{
                {-1, -1, -1, -1, -1},
                {-1, -2, 3, -3, -1},
                {-1, 1, -1, 1, -1},
                {-1, 5, -3, -4, -1},
                {-1, -1, -1, -1, -1}
        };
        Graph graph = Utils.labyrinthToGraph(labyrinth);
        Integer[] path = Search.search(graph.matrix, graph.start, graph.end);
    }
}
