import org.javatuples.Pair;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Scanner;

public class Utils {

    /**
     * Reads an input file containing labyrinth matrix.
     * <p>
     * Legend:
     * -1 = Wall
     * >=0 = Hallway
     * -2 = Start
     * -3 = Treasure
     * -4 = Destination
     */
    static Integer[][] readLabyrinthFile(String path) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File(path));
        ArrayList<Integer[]> array = new ArrayList<>();
        while (scanner.hasNextLine()) {
            String[] rowElements = scanner.nextLine().split(",");
            ArrayList<Integer> row = new ArrayList<>();
            for (String e : rowElements) {
                row.add(Integer.parseInt(e));
            }
            array.add(row.toArray(new Integer[0]));
        }
        return array.toArray(new Integer[0][0]);
    }

    /**
     * Converts a 2D array that represents labyrinth to adjacency matrix representing a graph.
     * <p>
     * Here is an example labyrinth matrix with 8 nodes:
     * <p>
     * -1 -1 -1 -1 -1
     * -1 -2  3  1 -1
     * -1  1 -1  1 -1
     * -1  5 -3  1 -1
     * -1 -1 -1 -1 -1
     * <p>
     *
     */
    static Graph labyrinthToGraph(Integer[][] labyrinth) {
        // because the labyrinth is surrounded by walls (-1 values), It's safe to skip the first and last elements
        Pair<Integer, Integer[][]> mapPair = labyrinthToNodeIndexMap(labyrinth);
        Integer nodeCount = mapPair.getValue0();
        Integer[][] indexMap = mapPair.getValue1();

        int[][] graph = new int[nodeCount][nodeCount];
        ArrayList<Integer> treasures = new ArrayList<>();
        int startNodeIndex = 0;
        int endNodeIndex = 0;

        int nodeIndex = 0;
        for (int i = 1; i < labyrinth.length - 1; i++) {
            for (int j = 1; j < labyrinth[0].length - 1; j++) {
                int node = labyrinth[i][j];
                switch (node) {
                    case -1: {
                        continue;
                    }
                    case -2: {
                        startNodeIndex = nodeIndex;
                        break;
                    }
                    case -3: {
                        treasures.add(nodeIndex);
                        break;
                    }
                    case -4: {
                        endNodeIndex = nodeIndex;
                        break;
                    }
                }
                int top = labyrinth[i - 1][j];
                int topIndex = indexMap[i - 1][j];
                setAdjacentNodes(graph, nodeIndex, node, topIndex, top);

                int right = labyrinth[i][j + 1];
                int rightIndex = indexMap[i][j + 1];
                setAdjacentNodes(graph, nodeIndex, node, rightIndex, right);

                int bottom = labyrinth[i + 1][j];
                int bottomIndex = indexMap[i + 1][j];
                setAdjacentNodes(graph, nodeIndex, node, bottomIndex, bottom);

                int left = labyrinth[i][j - 1];
                int leftIndex = indexMap[i][j - 1];
                setAdjacentNodes(graph, nodeIndex, node, leftIndex, left);

                nodeIndex++;
            }
        }
        return new Graph(startNodeIndex, endNodeIndex, graph, treasures.toArray(new Integer[0]));
    }

    static Pair<Integer, Integer[][]> labyrinthToNodeIndexMap(Integer[][] labyrinth) {
        int nodeCount = 0;
        Integer[][] map = new Integer[labyrinth.length][labyrinth.length];
        for (int i = 0; i < labyrinth.length; i++) {
            for (int j = 0; j < labyrinth[0].length; j++) {
                int node = labyrinth[i][j];
                if (node != -1) {
                    map[i][j] = nodeCount;
                    nodeCount++;
                } else {
                    // -1 indicates a wall (should be ignored)
                    map[i][j] = -1;
                }
            }
        }
        return new Pair<>(nodeCount, map);
    }

    /**
     * Sets adjacency matrix values.
     * Note that if a given edge has zero cost, value in adjacency matrix will equal to 1.
     */
    static void setAdjacentNodes(int[][] graph, int srcIndex, int srcNode, int destIndex, int destNode) {
        // destination node is a wall, ignore it
        if (destIndex == - 1) {
            return;
        }
        // end, start or treasure nodes have no cost
        if (isZeroCostNode(srcNode) && isZeroCostNode(destNode)) {
            graph[srcIndex][destIndex] = 1;
            graph[destIndex][srcIndex] = 1;
        }
        else if (isZeroCostNode(srcNode)) {
            graph[destIndex][srcIndex] = 1;
            graph[srcIndex][destIndex] = destNode + 1;
        }
        else if (isZeroCostNode(destNode)) {
            graph[destIndex][srcIndex] = srcNode + 1;
            graph[srcIndex][destIndex] = 1;
        }
        // If the destination node isn't a wall (-1), set adjacency matrix accordingly.
        else if (destNode != -1) {
            // Because this is undirected graph, set values in both ways.
            graph[srcIndex][destIndex] = destNode + 1;
            graph[destIndex][srcIndex] = srcNode + 1;
        }
    }

    static boolean isZeroCostNode(int node) {
        return node == -4 || node == -3 || node == -2;
    }

}
