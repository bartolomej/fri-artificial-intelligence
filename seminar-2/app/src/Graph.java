import java.util.stream.Stream;

public class Graph {
    public int start;
    public int end;
    public int[][] adjacencyMatrix;
    public Integer[] treasures;

    public Graph(int start, int end, int[][] adjacencyMatrix, Integer[] treasures) {
        this.start = start;
        this.end = end;
        this.adjacencyMatrix = adjacencyMatrix;
        this.treasures = treasures;
    }

    int getTotalEdges() {
        int count = 0;
        for (int[] matrix : this.adjacencyMatrix) {
            for (int i : matrix) {
                if (i > 0) {
                    count++;
                }
            }
        }
        return count;
    }

    int getTotalNodes() {
        return this.adjacencyMatrix.length;
    }

    public String toString() {
        String matrix = Utils.serializeMatrix(adjacencyMatrix);
        String treasures = Utils.serializeArray(this.treasures);
        int edges = this.getTotalEdges();
        int nodes = this.getTotalNodes();
        return String.format("Graph{start=%d, end=%d, edges=%d, nodes=%d, treasures=%s, matrix=%s}", start, end, edges, nodes, treasures, matrix);
    }

}
