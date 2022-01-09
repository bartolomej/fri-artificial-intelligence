import org.javatuples.Pair;
import org.junit.jupiter.api.Test;

import java.io.FileNotFoundException;

import static org.junit.jupiter.api.Assertions.*;

class UtilsTest {

    Integer[][] labyrinth = new Integer[][]{
            {-1, -1, -1, -1, -1},
            {-1, -2, 3, -3, -1},
            {-1, 1, -1, 1, -1},
            {-1, 5, -3, -4, -1},
            {-1, -1, -1, -1, -1}
    };

    @Test
    void testReadInputFile() throws FileNotFoundException {
        Integer[][] array = Utils.readLabyrinthFile("../data/labyrinth_1.txt");
        assertEquals(11, array[0].length);
        assertEquals(11, array.length);
        assertEquals(-1, array[0][0]);
    }

    @Test
    void testLabyrinthToNodeIndexMap() {
        Pair<Integer, Integer[][]> info = Utils.labyrinthToNodeIndexMap(labyrinth);
        assertEquals(8, info.getValue0());
    }

    @Test
    void testLabyrinthToGraph() {
        Graph graph = Utils.labyrinthToGraph(labyrinth);

        assertEquals(0, graph.start);
        assertEquals(7, graph.end);
        assertEquals(16, graph.getTotalEdges());
        assertArrayEquals(new Integer[]{2,6}, graph.mustVisit);
    }
}