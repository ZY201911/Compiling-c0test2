import java.io.*;
import java.util.List;
import java.util.Scanner;

public class App {
    public static void main(String[] args) {
        try{
            InputStream input = new FileInputStream(args[0]);
            Scanner scanner = new Scanner(input);
            StringIter it = new StringIter(scanner);
            Tokenizer tokenizer = new Tokenizer(it);
            Analyser analyser = new Analyser(tokenizer);
//            throw new Exception("异常")；
        }
        catch (Exception e){
            System.exit(-1);
        }
    }
}
