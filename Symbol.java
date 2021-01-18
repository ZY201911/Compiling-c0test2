

import java.util.ArrayList;
import java.util.List;

//写文件方式
//try {
//        FileWriter out =new FileWriter("oo.txt", true);
//        out.write("00 01");
//        out.close();
//        System.out.println("文件创建成功！");
//        } catch (IOException e) {
//        }
public class Symbol {
    //标识符的名字
    String name;
    //是否是常量
    boolean isConst;
    //标识符的类别，int，function(void类型的只可能是函数)，double
    String type;
    //是否初始化
    boolean isInitialized;
    //偏移
    int stackOffset;
    //标识符所在层数
    int floor;
    //如果是函数，则是参数列表
    List<Symbol> params = new ArrayList<>();
    //函数返回类型
    String returnType;

    //是否是函数参数,如果不是则为-1，如果是则为在函数参数列表的位置
    int isParam;
    //如果是参数，则为其函数符号；如果不是则为null
    Symbol function;

    //如果是局部变量，他的id;如果不是，-1
    int localId;
    //如果是全局变量，他的id;如果不是，-1
    int globalId;

    //构造函数
    public Symbol(String name, boolean isConst, String type, boolean isInitialized, int stackOffset, int floor, List<Symbol> params, String returnType, int isParam, Symbol function, int localId, int globalId){
        this.name = name;
        this.isConst = isConst;
        this.type = type;
        this.isInitialized = isInitialized;
        this.stackOffset = stackOffset;
        this.floor = floor;
        this.params = params;
        this.returnType = returnType;
        this.isParam = isParam;
        this.function = function;
        this.localId = localId;
        this.globalId = globalId;
    }
    public Symbol(){

    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public boolean isInitialized() {
        return isInitialized;
    }

    public void setInitialized(boolean initialized) {
        isInitialized = initialized;
    }

    public int getFloor() {
        return floor;
    }

    public void setFloor(int floor) {
        this.floor = floor;
    }

    public List<Symbol> getParams() {
        return params;
    }

    public void setParams(List<Symbol> params) {
        this.params = params;
    }

    public int getStackOffset() {
        return stackOffset;
    }

    public void setStackOffset(int stackOffset) {
        this.stackOffset = stackOffset;
    }

    public boolean isConst() {
        return isConst;
    }

    public void setConst(boolean aConst) {
        isConst = aConst;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    public int getIsParam() {
        return isParam;
    }

    public void setIsParam(int isParam) {
        this.isParam = isParam;
    }

    public int getLocalId() {
        return localId;
    }

    public void setLocalId(int localId) {
        this.localId = localId;
    }

    public int getGlobalId() {
        return globalId;
    }

    public void setGlobalId(int globalId) {
        this.globalId = globalId;
    }

    public Symbol getFunction() {
        return function;
    }

    public void setFunction(Symbol function) {
        this.function = function;
    }
}
