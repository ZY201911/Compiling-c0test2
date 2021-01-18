

import java.io.*;
import java.util.List;
import java.util.Scanner;

public class App {
    public static void main(String[] args) {
        try{
            //读入并开始
            //InputStream input = new FileInputStream("input.txt");
            InputStream input = new FileInputStream(args[0]);
            Scanner scanner = new Scanner(input);
            StringIter it = new StringIter(scanner);
            Tokenizer tokenizer = new Tokenizer(it);
            Analyser analyser = new Analyser(tokenizer);
            analyser.analyse();

            //debug测试
            System.out.println("全局符号表大小："+analyser.getGlobalTable().size());
            System.out.println("全局符号表：");
            for (Global global : analyser.getGlobalTable()) {
                System.out.println(global);
            }
            System.out.println("起始函数：\n"+analyser.get_start());
            System.out.println("函数：");
            for (Function function : analyser.getFunctionTable()) {
                System.out.println(function);
            }

            //输出格式转换
            Output out = new Output(analyser.getGlobalTable(), analyser.getFunctionTable(), analyser.get_start());
            List<Byte> bytes = out.transfer();
            byte[] result = new byte[bytes.size()];
            for (int i = 0; i < bytes.size(); ++i) {
                result[i] = bytes.get(i);
            }

            //输出
            //DataOutputStream output = new DataOutputStream(new FileOutputStream(new File("output.txt")));
            DataOutputStream output = new DataOutputStream(new FileOutputStream(new File(args[1])));
            output.write(result);
        }
        catch (Exception e){
            System.exit(-1);
        }
    }
}
