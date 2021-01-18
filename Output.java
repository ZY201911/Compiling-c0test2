import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Output {
    /** 虚拟机指令集 */
    Map<String, Integer> operations = Operation.getOperations();
    List<Global> globalTable;
    List<Function> functionTable;
    Function _start;
    List<Byte> output;
    //必输出项
    int magic=0x72303b3e;
    int version=0x00000001;
    public Output(List<Global> globalTable, List<Function> functionTable, Function _start){
        this.globalTable = globalTable;
        this.functionTable = functionTable;
        this._start = _start;
        this.output = new ArrayList<>();
    }

    public List<Byte> transfer(){
        //放magic和version
        addInt(4, magic);
        addInt(4, version);

        System.out.println("end transfer1.");

        //放globals.count
        addInt(4, globalTable.size());
        System.out.println("end transfer2.");

        //放全局变量
        for(Global global:globalTable){
            //1.is_const
            addInt(1, global.getIsConst());

            //2.value.count
            //3.value.items
            //如果是全局变量或者全局常量
            if(global.getItems() == null){
                addInt(4, 8);
                addLong(8, 0L);
            }
            //如果是函数
            else{
                addInt(4, global.getItems().length());
                addString(global.getItems());
            }
        }
        System.out.println("end transfer3.");

        //放functions.count
        addInt(4, functionTable.size() + 1);
        System.out.println("end transfer4.");
        //放_start
        transferFunction(_start);
        System.out.println("end transfer5.");

        //放其他函数
        for(Function function:functionTable)
            transferFunction(function);
        System.out.println("end transfer6.");
        return output;
    }

    private void transferFunction(Function function){
        //1.name
        addInt(4, function.getId());
        //2.ret_slots
        addInt(4, function.getRetSlots());
        //3.param_slots
        addInt(4, function.getParamSlots());
        //4.loc_slots
        addInt(4, function.getLocSlots());
        //5.body.count
        addInt(4, function.getBody().size());
        System.out.println("end transferfunction1.");
        //6.body.items
        List<Instruction> instructions = function.getBody();
        System.out.println("end transferfunction2.");
        for(Instruction instruction:instructions){
            String op = instruction.getOp();
            System.out.println("end transferfunction3.---"+op);
            //指令
            int opInt = operations.get(op);
            addInt(1, opInt);
            System.out.println("end transferfunction4.---"+opInt);
            //操作数
            Object i = instruction.getX();
            System.out.println(i);
            if(instruction.getX() != null){
                //只有push的操作数是64位
                if(opInt == 1) {
                    System.out.println("opInt==1");
                    long l = Long.valueOf(String.valueOf(instruction.getX()));
                    addLong(8, l);
                    System.out.println("end transferfunction4.1.");
                }
                else {
                    int in = Integer.parseInt(String.valueOf(instruction.getX()));
                    addInt(4, in);
                    System.out.println("end transferfunction4.2.");
                }
            }
            System.out.println("end transferfunction5.");
        }
    }

    private void addInt(int length, int x){
        int start = 8 * (length-1);
        for(int i = 0 ; i < length; i++){
            int part = x >> ( start - i * 8 ) & 0xFF;
            byte b = (byte) part;
            output.add(b);
        }
    }

    private void addLong(int length, long x){
        int start = 8 * (length-1);
        System.out.println("addLong1");
        for(int i = 0 ; i < length; i++){
            long part = x >> ( start - i * 8 ) & 0xFF;
            System.out.println("addLong2");
            byte b = (byte) part;
            System.out.println("addLong3");
            output.add(b);
            System.out.println("addLong4");
        }
    }


    private void addString(String x) {
        for (int i = 0; i < x.length();i++){
            char c = x.charAt(i);
            output.add((byte) c);
        }
    }
}
