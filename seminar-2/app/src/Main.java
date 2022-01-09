import org.javatuples.Pair;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Main {

    static char[] allN = new char[]{'1', '2', '3', '4', '5', '6', '7', '8', '9'};

    public static void main(String[] args) throws Exception {
        // TODO: remove mock
        args = new String[]{"1"};

        if (args.length != 1) {
            throw new Exception("Invalid arguments");
        }
        String[] list = args[0].split(",");
        for (String n : list) {
            char cn = n.charAt(0);
            if (cn >= '1' && cn <= '9') {
                solve(cn);
            } else if (cn == '*') {
                solveAll();
            }
        }
    }

    static void storeMetadata() throws IOException {
        for (char n : allN) {
            String inPath = String.format("./data/labyrinth_%c.txt", n);
            Integer[][] labyrinth = Utils.readLabyrinthFile(inPath);
            Graph graph = Utils.labyrinthToGraph(labyrinth);
            String graphPath = String.format("./graphs/labyrinth_%c.txt", n);
            Utils.writeToFile(graphPath, graph.toString());
            String mapPath = String.format("./maps/labyrinth_%c.txt", n);
            Pair<Integer, Integer[][]> nodeIndexMap = Utils.labyrinthToNodeIndexMap(labyrinth);
            Utils.writeToFile(mapPath, Utils.serializeMatrix(nodeIndexMap.getValue1()));
        }
    }

    static void solveAll() throws Exception {
        for (char n : allN) {
            solve(n);
        }
    }

    static void solve(char n) throws Exception {
        String filePath = String.format("./data/labyrinth_%c.txt", n);
        Integer[][] labyrinth = Utils.readLabyrinthFile(filePath);
        Graph graph = Utils.labyrinthToGraph(labyrinth);
        Integer[][] paths = computePossiblePaths(graph);
        Graph subgraph = generateSubgraph(graph, paths);
        Integer[] path = TSP.greedy(subgraph);
        Search.printPath(path);
    }

    static Graph generateSubgraph(Graph graph, Integer[][] paths) {
        // TODO: better calculate matrix dimensions, this is not optimal
        int[][] adjacencyMatrix = new int[graph.matrix.length][graph.matrix[0].length];
        for (Integer[] path : paths) {
            int start = path[0];
            int end = path[path.length - 1];
            int cost = graph.getPathCost(path);
            adjacencyMatrix[start][end] = cost;
            adjacencyMatrix[end][start] = cost;
        }
        return new Graph(graph.start, graph.end, adjacencyMatrix, graph.mustVisit, graph.mustVisit.length + 2);
    }

    static Integer[][] computePossiblePaths(Graph graph) {
        // create a list of interested nodes {start, end, ...treasures}
        Integer[] interestedNodes = new Integer[2 + graph.mustVisit.length];
        interestedNodes[0] = graph.start;
        interestedNodes[1] = graph.end;
        System.arraycopy(graph.mustVisit, 0, interestedNodes, 2, graph.mustVisit.length);
        List<Integer[]> paths = new ArrayList<>();
        for (int i = 0; i < interestedNodes.length; i++) {
            for (int j = i + 1; j < interestedNodes.length; j++) {
                paths.add(Search.search(graph.matrix, interestedNodes[i], interestedNodes[j]));
            }
        }
        return paths.toArray(new Integer[0][0]);
    }
}
