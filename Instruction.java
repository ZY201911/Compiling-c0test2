import java.util.Objects;

public class Instruction {
    //操作
    String op;
    //参数
    Object x;

    public Instruction(String op, Object x){
        this.op = op;
        this.x = x;
    }

    public String getOp() {
        return op;
    }

    public void setOp(String op) {
        this.op = op;
    }

    public Object getX() {
        return x;
    }

    public void setX(Integer x) {
        this.x = x;
    }

    @Override
    public String toString() {
        return "" + op + " " + x + '\n';
    }

}
