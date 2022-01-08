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
}
