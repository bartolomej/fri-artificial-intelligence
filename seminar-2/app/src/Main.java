import org.javatuples.Pair;

import java.io.IOException;
import java.util.*;

public class Main {

    static char[] allN = new char[]{'1', '2', '3', '4', '5', '6', '7', '8', '9'};

    public static void main(String[] args) throws Exception {
        if (args.length == 0) {
            args = new String[]{"*"}; // default value
        }

        String[] list = args[0].split(",");
        for (String n : list) {
            char cn = n.charAt(0);
            if (cn >= '1' && cn <= '9') {
                solveSingle(cn);
            } else if (cn == '*') {
                solveAll();
            }
        }
    }

    static void solveAll() throws Exception {
        for (char n : allN) {
            solveSingle(n);
        }
    }

    static void solveSingle(char n) throws Exception {
        System.out.print("\n\n-------------- DATASET " + n  + " ----------------\n\n");
        String[] searchAlgorithms = new String[]{
                "dfs",
                "bfs"
        };
        for (String algo : searchAlgorithms) {
            System.out.println("ALGORITHM: " + algo);
            solve(n, algo);
        }
    }

    static void solve(char n, String searchAlgorithm) throws Exception {
        String filePath = String.format("./data/labyrinth_%c.txt", n);
        Integer[][] labyrinth = Utils.readLabyrinthFile(filePath);
        Graph graph = Utils.labyrinthToGraph(labyrinth);
        Integer[][] paths = computePossiblePaths(graph, searchAlgorithm);
        // probably not the nicest solution, consider refactoring :)
        Pair<Graph, Map<String, Integer[]>> subgraphTuple = generateSubgraph(graph, paths);
        Graph subgraph = subgraphTuple.getValue0();
        Map<String, Integer[]> edgeVerticesToPathMap = subgraphTuple.getValue1();

        Integer[] greedyPath = TSP.greedy(subgraph);
        Integer[] greedyFullPath = buildFullPath(edgeVerticesToPathMap, greedyPath);
        System.out.printf("GREEDY (cost=%d, path=%s)\n", subgraph.getPathCost(greedyPath), Utils.arrayJoin(greedyFullPath, ","));
        Utils.writeToFile(String.format("./paths/labyrinth_%c_greedy.txt", n), Utils.arrayJoin(greedyFullPath, ","));

        Integer[] localOptimumPath = TSP.localSearch(subgraph, 1000000);
        Integer[] localOptimumFullPath = buildFullPath(edgeVerticesToPathMap, localOptimumPath);
        System.out.printf("LOCAL OPTIMUM (cost=%d, path=%s)\n", subgraph.getPathCost(localOptimumPath), Utils.arrayJoin(localOptimumFullPath, ","));
        Utils.writeToFile(String.format("./paths/labyrinth_%c_local.txt", n), Utils.arrayJoin(localOptimumFullPath, ","));

        System.out.println();
    }

    static Integer[] buildFullPath(Map<String, Integer[]> edgeVerticesMap, Integer[] path) {
        List<Integer> fullPath = new ArrayList<>();
        for (int i = 1; i < path.length; i++) {
            Integer[] subPath = getFullPath(edgeVerticesMap, path[i - 1], path[i]);
            fullPath.addAll(Arrays.asList(subPath));
        }
        return fullPath.toArray(new Integer[0]);
    }

    static Integer[] getFullPath(Map<String, Integer[]> edgeVerticesMap, int start, int end) {
        String key = String.format("%d,%d", start, end);
        if (edgeVerticesMap.containsKey(key)) {
            return edgeVerticesMap.get(key);
        }
        String keyReversed = String.format("%d,%d", end, start);
        if (edgeVerticesMap.containsKey(keyReversed)) {
            Integer[] path = edgeVerticesMap.get(keyReversed);
            Collections.reverse(Arrays.asList(path));
            return path;
        }
        return null;
    }

    static Pair<Graph, Map<String, Integer[]>> generateSubgraph(Graph graph, Integer[][] paths) {
        // adjacency matrix of subgraph has unchanged dimensions,
        // which is not optimal for space complexity, but I ain't got time for this :)
        int[][] adjacencyMatrix = new int[graph.matrix.length][graph.matrix[0].length];
        for (int[] row : adjacencyMatrix) {
            Arrays.fill(row, -1);  // -1 = no edge between nodes
        }
        Map<String, Integer[]> pathMap = new HashMap<>();
        for (Integer[] path : paths) {
            int start = path[0];
            int end = path[path.length - 1];
            int cost = graph.getPathCost(path);
            adjacencyMatrix[start][end] = cost;
            adjacencyMatrix[end][start] = cost;
            pathMap.put(String.format("%d,%d", start, end), path);
        }
        Graph subgraph = new Graph(graph.start, graph.end, adjacencyMatrix, graph.mustVisit, graph.mustVisit.length + 2);
        return new Pair<>(subgraph, pathMap);
    }

    static Integer[][] computePossiblePaths(Graph graph, String searchAlgorithm) {
        // create a list of interested nodes {start, end, ...treasures}
        Integer[] interestedNodes = new Integer[2 + graph.mustVisit.length];
        interestedNodes[0] = graph.start;
        interestedNodes[1] = graph.end;
        System.arraycopy(graph.mustVisit, 0, interestedNodes, 2, graph.mustVisit.length);
        List<Integer[]> paths = new ArrayList<>();
        for (int i = 0; i < interestedNodes.length; i++) {
            for (int j = i + 1; j < interestedNodes.length; j++) {
                if (searchAlgorithm.equals("bfs")) {
                    paths.add(Search.bfs(graph.matrix, interestedNodes[i], interestedNodes[j]));
                }
                else {
                    paths.add(Search.dfs(graph.matrix, interestedNodes[i], interestedNodes[j]));
                }
            }
        }
        return paths.toArray(new Integer[0][0]);
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
}
