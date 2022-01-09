public class Graph {
    public int start;
    public int end;
    public int[][] adjacencyMatrix;
    public Integer[] mustVisit;

    public Graph(int start, int end, int[][] adjacencyMatrix, Integer[] mustVisit) {
        this.start = start;
        this.end = end;
        this.adjacencyMatrix = adjacencyMatrix;
        this.mustVisit = mustVisit;
    }

    /**
     * Returns a cost (sum of costs of all the edges) for a given path (a sequence of vertices).
     */
    int getPathCost(Integer[] path) {
        int sum = 0;
        for (int i = 0; i < path.length - 1; i++) {
            sum += adjacencyMatrix[path[i]][path[i + 1]];
        }
        return sum;
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
        String matrix = Utils.serializeMatrix(adjacencyMatrix, true);
        String treasures = Utils.serializeArray(this.mustVisit);
        int edges = this.getTotalEdges();
        int nodes = this.getTotalNodes();
        return String.format("Graph{start=%d, end=%d, edges=%d, nodes=%d, treasures=%s, matrix=%s}", start, end, edges, nodes, treasures, matrix);
    }

}
