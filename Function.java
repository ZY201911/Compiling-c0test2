

import java.util.List;

public class Function {
    String name;
    //该函数在全局符号表里的位置
    Integer id;
    //如果返回值是int，则为1；是double，则为2；是void，则为0
    Integer retSlots;
    //参数列表大小
    Integer paramSlots;
    //局部变量个数
    Integer locSlots;
    //函数内指令
    List<Instruction> body;
    int floor;
    String returnType;

    public Function(String name, Integer id, Integer retSlots, Integer paramSlots, Integer locSlots, List<Instruction> body, int floor, String returnType){
        this.name = name;
        this.id = id;
        this.retSlots = retSlots;
        this.paramSlots = paramSlots;
        this.locSlots = locSlots;
        this.body = body;
        this.floor = floor;
        this.returnType = returnType;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getRetSlots() {
        return retSlots;
    }

    public void setRetSlots(Integer retSlots) {
        this.retSlots = retSlots;
    }

    public Integer getParamSlots() {
        return paramSlots;
    }

    public void setParamSlots(Integer paramSlots) {
        this.paramSlots = paramSlots;
    }

    public Integer getLocSlots() {
        return locSlots;
    }

    public void setLocSlots(Integer locSlots) {
        this.locSlots = locSlots;
    }

    public List<Instruction> getBody() {
        return body;
    }

    public void setBody(List<Instruction> body) {
        this.body = body;
    }

    public int getFloor() {
        return floor;
    }

    public void setFloor(int floor) {
        this.floor = floor;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }

    @Override
    public String toString() {
        return "Function{\n" +
                "   name=" + name +
                ",\n    id=" + id +
                ",\n    retSlots=" + retSlots +
                ",\n    paramSlots=" + paramSlots +
                ",\n    locSlots=" + locSlots +
                ",\n    body=" + body +
                ",\n    returnType=" + returnType +
                ",\n    floor=" + floor +'\n'+
                '}';
    }

}
