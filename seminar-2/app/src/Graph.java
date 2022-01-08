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
}
