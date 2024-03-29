import java.util.ArrayList;
import java.util.Collections;
import java.util.Random;

// TODO: Refactor Code!
// TODO: Write Documentation!

public class MapCreator {
    private int sizeX = 20, sizeY = 20;
    private int NumberOfOrcs, NumberOfHumans, NumberOfTouchdowns;

    public MapCreator(int numberOfTouchdowns, int numberOfOrcs, int numberOfHumans) {
        this.NumberOfOrcs = numberOfOrcs;
        this.NumberOfHumans = numberOfHumans;
        this.NumberOfTouchdowns = numberOfTouchdowns;
    }

    public char[][] create_map(){
        char[][] map = new char[sizeX][sizeY];

        for (int i = 0; i < map.length; ++i)
            for (int j = 0; j < map.length; ++j)
                map[i][j] = ' ';

        map[0][0] = 'R';
        Random randomizer = new Random();

        for (int i = 0; i < NumberOfTouchdowns; ++i){
            while (true){
                int X = randomizer.nextInt(20);
                int Y = randomizer.nextInt(20);

                if (map[X][Y] == ' '){
                    map[X][Y] = 'T';
                    break;
                }
            }
        }
        for (int i = 0; i < NumberOfHumans; ++i){
            while (true){
                int X = randomizer.nextInt(20);
                int Y = randomizer.nextInt(20);

                if (map[X][Y] == ' '){
                    map[X][Y] = 'H';
                    break;
                }
            }
        }
        for (int i = 0; i < NumberOfOrcs; ++i){
            while (true){
                int X = randomizer.nextInt(20);
                int Y = randomizer.nextInt(20);

                if (map[X][Y] == ' '){
                    map[X][Y] = 'O';
                    break;
                }
            }
        }

        return map;
    }

    public static String printable_map(char[][] map){
        StringBuilder out = new StringBuilder();

        for (int j = map.length - 1; j >= 0; --j){
            out.append("% <" + (j <= 9 ? "0" + j : j) + "> |");
            for (int i = 0; i < map.length; ++i){
                out.append(" " + map[i][j] + (i == map.length - 1 ? " |\n" : " |"));
            }
        }
//        out.append("%      ---------------------------------------------------------------------------------\n");
        out.append("%      <00><01><02><03><04><05><06><07><08><09><10><11><12><13><14><15><16><17><18><19>  \n");
        return out.toString();
    }

    public static String convert_to_string(char[][] map){
        StringBuilder output = new StringBuilder();
        ArrayList<String> facts = new ArrayList<>();
        for (int i = 0; i < map.length; ++i){
            for (int j = 0; j < map.length; ++j){
                if (map[i][j] == 'T'){
                    facts.add("t(" + i + "," + j + ").\n");
                } else if (map[i][j] == 'H'){
                    facts.add("h(" + i + "," + j + ").\n");
                } else if (map[i][j] == 'O'){
                    facts.add("o(" + i + "," + j + ").\n");
                }
            }
        }
        Collections.sort(facts);
        for (String s: facts)
            output.append(s);

        return output.toString();
    }

}
