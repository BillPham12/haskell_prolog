import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

public class comp3007_w19_101023249_a1_1{

    // indicate word "A,E,I,O or U"
    // using charAt, pop, remove in list
    // input and output

    public static void main (String args[]){
        //
        Scanner in = new Scanner(System.in);

        // Get input from user
        System.out.println("Type your input: \n" );
        String input = in.nextLine();
        if(!StoredInputToList(input).isEmpty()) System.out.println(convertList(StoredInputToList(input)));
    }

    //part a
    public static boolean NoUpperLetterCase(String string){
        if(string.equals("")) return true;
        if(string.charAt(0) >= 65 && string.charAt(0) <= 90) return false;
        else return NoUpperLetterCase (string.substring(1)) && true;
     }
     //part b
    public static int UnderScore(String string){
        if (string.equals("")) return 0;
        else {
            if (string.charAt(0) == 95) return 0;
            else return UnderScore(string.substring(1)) + 1;
        }
    }
    // part c
    private static LinkedList StoredInputToList(String input) {
        LinkedList output = new LinkedList<String>();
        if(!NoUpperLetterCase(input)) System.out.println("Your input has upper letters, please avoid using it!!\n");
        if(!input.equals("") && NoUpperLetterCase(input)){
            output.add(input.substring(0, UnderScore(input)));
            if(!input.substring(UnderScore(input)).equals(""))
                 output.addAll(StoredInputToList(input.substring(UnderScore(input) + 1)));
        }
        return output;
    }

    // part d
    // 97 101 105 111 117
    public static int IndexOfVowel(String input){
        if (input.equals("")) return 0;
        else {
            if (input.charAt(0) == 97 || input.charAt(0) == 101 ||
                    input.charAt(0) == 105 || input.charAt(0) == 111 ||  input.charAt(0) == 117) return 0;
            else return IndexOfVowel(input.substring(1)) + 1;
        }
    }

    // part E
    public static String convertString(String string){
        String output = string;

        if( !string.equals("")  && string.charAt(0) >= 97 && string.charAt(0) <= 122 && IndexOfVowel(string) < 1){
            output += "ay";}
        else if (string.charAt(0) >= 97 && string.charAt(0) <= 122 && IndexOfVowel(string) >= 1){
            String additional = output.substring(0,IndexOfVowel(output));
            String out = output.substring(IndexOfVowel(output));
            out += additional + "ay";
            return out;
        }
        return output;
    }
    // part F
    public static LinkedList convertList(LinkedList list){
        LinkedList output = new LinkedList<String>();
        if(!list.isEmpty()){
            output.add(convertString((String)list.getFirst()));
            list.removeFirst();
            output.addAll(convertList(list));
        }

        return output;
    }



}
