public class Graph {
    public int start;
    public int end;
    public int nodes;
    public int[][] matrix;
    public Integer[] mustVisit;

    public Graph(int start, int end, int[][] matrix, Integer[] mustVisit, int nodes) {
        this.start = start;
        this.end = end;
        this.matrix = matrix;
        this.mustVisit = mustVisit;
        this.nodes = nodes;
    }

    /**
     * Returns a cost (sum of costs of all the edges) for a given path (a sequence of vertices).
     */
    int getPathCost(Integer[] path) {
        int sum = 0;
        for (int i = 0; i < path.length - 1; i++) {
            sum += matrix[path[i]][path[i + 1]];
        }
        return sum;
    }

    int getTotalEdges() {
        int count = 0;
        for (int[] matrix : this.matrix) {
            for (int i : matrix) {
                if (i > 0) {
                    count++;
                }
            }
        }
        return count;
    }

    public String toString() {
        String matrix = Utils.serializeMatrix(this.matrix, true);
        String treasures = Utils.serializeArray(this.mustVisit);
        int edges = this.getTotalEdges();
        return String.format("Graph{start=%d, end=%d, edges=%d, nodes=%d, treasures=%s, matrix=%s}", start, end, edges, nodes, treasures, matrix);
    }

}
