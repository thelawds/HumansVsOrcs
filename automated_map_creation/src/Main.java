import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;

// TODO: Refactor Code!
// TODO: Write Documentation!

public class Main {

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);

        System.out.print("Please, write down #tuchdowns, #orcs, #humans: ");
        int touchdowns = input.nextInt();
        int orcs = input.nextInt();
        int humans = input.nextInt();

        MapCreator mc = new MapCreator(1, 10, 20);

        System.out.print("How much maps do you need?: ");
        int N = input.nextInt();

        File path_to_create = new File("Test_Input");
        path_to_create.mkdir();

        for (int i = 0; i < N; ++i){
            FileWriter file = null;
            try{
                file = new FileWriter("./Test_Input/input_"+i+".pl");
                char[][] map = mc.create_map();
                file.write(MapCreator.convert_to_string(map) + "\n\n\n");
                file.write(MapCreator.printable_map(map));
            }catch (IOException e){
                e.printStackTrace();
            }finally {
                try {
                    if (file != null)
                        file.close();
                }catch (IOException e){
                    e.printStackTrace();
                }
            }
        }

        input.close();
    }
}
