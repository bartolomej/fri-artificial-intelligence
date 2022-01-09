import java.util.*;

public class Search {

    public static Integer[] dfs(int[][] graph, int startNode, int endNode) {
        boolean[] marked = new boolean[graph.length];
        int[] from = new int[graph.length];
        Integer[] path = null;


        Stack<Integer> stack = new Stack<Integer>();

        from[startNode] = -1;
        marked[startNode] = true;
        stack.push(startNode);

        while (!stack.isEmpty()) {
            int curNode = stack.peek();

            if (curNode == endNode) {
                path = buildPath(from, curNode);
                break;
            }

            // najdi neobiskanega naslednjika
            boolean found = false;
            for (int nextNode = 0; nextNode < graph[curNode].length; nextNode++) {
                if (graph[curNode][nextNode] >= 0 && !marked[nextNode]) {
                    marked[nextNode] = true;
                    from[nextNode] = curNode;
                    stack.push(nextNode);
                    found = true;
                    break;
                }
            }

            if (!found) {
                stack.pop();
            }
        }

        return path;
    }

    public static Integer[] bfs(int[][] graph, int startNode, int endNode) {
        boolean[] marked = new boolean[graph.length];
        int[] from = new int[graph.length];
        Integer[] path = null;

        Queue<Integer> queue = new LinkedList<Integer>();

        marked[startNode] = true;
        from[startNode] = -1;

        queue.add(startNode);

        while (!queue.isEmpty()) {
            int curNode = queue.remove();

            if (curNode == endNode) {
                path = buildPath(from, curNode);
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
        return path;
    }

    static Integer[] buildPath(int[] from, int curNode) {
        ArrayList<Integer> path = new ArrayList<>();
        path.add(curNode);
        while (true) {
            curNode = from[curNode];
            if (curNode != -1)
                path.add(curNode);
            else
                break;
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
        Integer[] bfsPath = Search.bfs(graph.matrix, graph.start, graph.end);
        System.out.println(Utils.arrayJoin(bfsPath, " --> "));

        Integer[] dfsPath = Search.dfs(graph.matrix, graph.start, graph.end);
        System.out.println(Utils.arrayJoin(dfsPath, " --> "));
    }
}
