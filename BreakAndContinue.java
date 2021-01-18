public class BreakAndContinue {
    Instruction instruction;
    int location;
    int whileNum;
    public BreakAndContinue(Instruction instruction, int location, int whileNum){
        this.instruction = instruction;
        this.location = location;
        this.whileNum = whileNum;
    }

    public Instruction getInstruction() {
        return instruction;
    }

    public void setInstruction(Instruction instruction) {
        this.instruction = instruction;
    }

    public int getLocation() {
        return location;
    }

    public void setLocation(int location) {
        this.location = location;
    }

    public int getWhileNum() {
        return whileNum;
    }

    public void setWhileNum(int whileNum) {
        this.whileNum = whileNum;
    }
}
