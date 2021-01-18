

public class Global {
    //如果是常量则为1，如果不是则为0
    Integer isConst;
    Integer count;
    String items;

    public Global(Integer isConst, Integer count, String items){
        this.isConst = isConst;
        this.count = count;
        this.items = items;
    }
    public Global(Integer isConst){
        this.isConst = isConst;
        this.count = 0;
        this.items = null;
    }

    public Integer getIsConst() {
        return isConst;
    }

    public void setIsConst(Integer isConst) {
        this.isConst = isConst;
    }

    public Integer getCount() {
        return count;
    }

    public void setCount(Integer count) {
        this.count = count;
    }

    public String getItems() {
        return items;
    }

    public void setItems(String items) {
        this.items = items;
    }

    @Override
    public String toString() {
        return "Global{" +
                "isConst=" + isConst +
                ", count=" + count +
                ", items=" + items +
                '}';
    }

}
