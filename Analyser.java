import java.sql.SQLOutput;
import java.util.*;

public final class Analyser {
    // 词法分析器 
    Tokenizer tokenizer;
    // 偷看的下一个token 
    Token peekedToken = null;

    // 符号表 
    List<Symbol> symbolTable = new ArrayList<>();
    // 全局符号表 
    List<Global> globalTable = new ArrayList<>();
    // 函数表 
    List<Function> functionTable = new ArrayList<>();

    // 开始函数 
    Function _start;

    // 当前正在声明的函数 
    Symbol presentFunction;
    // 上一个返回的函数
    Symbol returnFunction;
    // 函数个数 
    int functionCount = 1;
    // 层数 
    int floor = 1;

    // 下一个变量的栈偏移 
    int nextOffset = 0;
    // 全局符号数
    int globalCount = 0;
    // 局部变量数
    int localCount = 0;

    // 操作符号栈 
    Stack<TokenType> op = new Stack<>();

    // 指令集合 
    ArrayList<Instruction> instructions;

    // while循环层数 
    int isInWhile = 0;

    // break和continue栈
    List<BreakAndContinue> continueInstruction = new ArrayList<BreakAndContinue>();
    List<BreakAndContinue> breakInstruction = new ArrayList<BreakAndContinue>();

    // 构造函数
    public Analyser(Tokenizer tokenizer) {
        this.tokenizer = tokenizer;
    }

    // 调用入口
    public void analyse() throws CompileError {
        analyseProgram();
    }

    // 偷看下一个 Token 
    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    // 获取下一个 Token 
    private Token next() throws TokenizeError {
        if (peekedToken != null) {
            Token token = peekedToken;
            peekedToken = null;
            System.out.println(token.getValue() + " ");
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }

    // 检查偷看的 token 类型 
    private boolean check(TokenType tt) throws TokenizeError {
        Token token = peek();
        return token.getTokenType() == tt;
    }


    // 期望的 token 类型 
    private Token expect(TokenType tt) throws CompileError {
        Token token = peek();
        if (token.getTokenType() == tt) {
            return next();
        }
        else {
            throw new ExpectedTokenError(tt, token);
        }
    }

    // 获取下一个变量的栈偏移 
    private int getNextVariableOffset() {
        return this.nextOffset++;
    }

    // 根据名称查符号表 
    private int searchSymbolByName(String name){
        for(int i=0; i<symbolTable.size(); i++){
            if(symbolTable.get(i).getName().equals(name)) return i;
        }
        return -1;
    }

    // 根据Token查符号表 
    private Symbol searchSymbolByToken(Token token){
        String name = (String) token.getValue();
        for(int i=symbolTable.size()-1; i>=0; i--){
            if(symbolTable.get(i).getName().equals(name)) return symbolTable.get(i);
        }
        return null;
    }

    // 向符号表添加符号 
    private void addSymbol(String name, boolean isConst, String type, boolean isInitialized, int floor, List<Symbol> params, String returnType, Pos curPos, int isParam, Symbol function, int localId, int globalId) throws AnalyzeError {
        int same = searchSymbolByName(name);
        if(same == -1)
            this.symbolTable.add(new Symbol(name, isConst, type, isInitialized, getNextVariableOffset(), floor, params, returnType, isParam, function, localId, globalId));
        else{
            Symbol symbol = symbolTable.get(same);
            //同一层报错
            if(symbol.getFloor() == floor)
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
            this.symbolTable.add(new Symbol(name, isConst, type, isInitialized, getNextVariableOffset(), floor, params, returnType, isParam, function, localId, globalId));
        }
    }

    // 初始化符号 
    private void initializeSymbol(String name, Pos curPos) throws AnalyzeError {
        int position = searchSymbolByName(name);
        if (position == -1) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        }
        else {
            Symbol update = symbolTable.get(position);
            update.setInitialized(true);
        }
    }


    // 分析主程序 
    private void analyseProgram() throws CompileError {
        //分配空间
        instructions = new ArrayList<>();
        // program -> decl_stmt* function*
        // decl_stmt -> let_decl_stmt | const_decl_stmt
        // 分析声明语句
        while(check(TokenType.LET_KW)||check(TokenType.CONST_KW))
            analyseDeclStmt();

        // 记录初始化指令
        List<Instruction> initInstructions = instructions;
        // function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
        // 分析函数
        while(check(TokenType.FN_KW)){
            //清空指令集
            instructions = new ArrayList<>();
            analyseFunction();
            globalCount++;
            functionCount++;
        }

        //寻找主程序入口
        int mainLoca = searchSymbolByName("main");
        if(mainLoca == -1){
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        }
        //在全局符号表里填入入口程序_start
        globalTable.add(new Global(1, 6, "_start"));
        //更新初始化指令集
        Symbol main = symbolTable.get(mainLoca);
        if (!main.getReturnType().equals("void")) {
            initInstructions.add(new Instruction("stackalloc", 1));
            initInstructions.add(new Instruction("call", functionCount-1));
            initInstructions.add(new Instruction("popn", 1));
        }
        else {
            initInstructions.add(new Instruction("stackalloc", 0));
            initInstructions.add(new Instruction("call", functionCount-1));
        }
        // 记录_start函数
        _start = new Function("_start", globalCount, 0, 0, 0, initInstructions, floor, "void");
        globalCount++;
    }

    // 声明语句分析函数 
    private void analyseDeclStmt() throws CompileError{
        // 变量
        if(check(TokenType.LET_KW))
            analyseLetDeclStmt();
        // 常量
        else if(check(TokenType.CONST_KW))
            analyseConstDeclStmt();

        //最外层则全局变量+1，否则局部变量+1
        if(floor == 1) globalCount++;
        else localCount++;
    }

    // let_decl_stmt -> 'let' IDENT ':' ty ('=' expr)? ';'
    // 分析变量声明语句 
    private void analyseLetDeclStmt() throws CompileError{
        String name;
        boolean isConst = false;
        String type;
        boolean isInitialized = false;
        List<Symbol> params = null;
        String exprType = "";
        Token ident;

        // 跳过LET_KW
        expect(TokenType.LET_KW);
        //记录标识符
        ident = expect(TokenType.IDENT);
        name = (String) ident.getValue();
        // 冒号
        expect(TokenType.COLON);
        //分析类型
        type = analyseTy();
        //变量类型不能是void
        if(type.equals("void"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        //等号
        if(check(TokenType.ASSIGN)){
            //初始化
            isInitialized = true;
            Instruction instruction;
            if (floor == 1) {
                // 全局
                instruction = new Instruction("globa", globalCount);
                instructions.add(instruction);
            }
            else {
                // 局部
                instruction = new Instruction("loca", localCount);
                instructions.add(instruction);
            }
            next();
            // 分析表达式
            exprType = analyseExpr();
            // 构造表达式指令
            while (!op.empty())
                UtilFunctions.operatorInstructions(op.pop(), instructions, exprType);
            // 存储
            instruction = new Instruction("store.64", null);
            instructions.add(instruction);
        }

        // 分号
        expect(TokenType.SEMICOLON);

        // 更新符号表
        if((isInitialized && exprType.equals(type)) || !isInitialized)
            if(floor == 1)
                addSymbol(name, false, type, isInitialized, floor, params, "", ident.getStartPos(), -1, null, -1, globalCount);
            else
                addSymbol(name, false, type, isInitialized, floor, params, "", ident.getStartPos(), -1, null, localCount, -1);
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        // 是否更新全局符号表
        if(floor == 1){
            Global global = new Global(0);
            globalTable.add(global);
        }
    }

    // const_decl_stmt -> 'const' IDENT ':' ty '=' expr ';'
    // 分析常量声明语句 
    private void analyseConstDeclStmt() throws CompileError{
        String name;
        String type;
        boolean isInitialized = true;
        List<Symbol> params = null;
        String exprType;
        Token ident;
        Instruction instruction;

        expect(TokenType.CONST_KW);
        ident = expect(TokenType.IDENT);
        name = (String) ident.getValue();
        expect(TokenType.COLON);
        type = analyseTy();
        if(type.equals("void"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        if (floor == 1) {
            instruction = new Instruction("globa", globalCount);
            instructions.add(instruction);
        }
        else {
            instruction = new Instruction("loca", localCount);
            instructions.add(instruction);
        }
        expect(TokenType.ASSIGN);
        exprType = analyseExpr();
        while (!op.empty())
            UtilFunctions.operatorInstructions(op.pop(), instructions, exprType);

        instruction = new Instruction("store.64", null);
        instructions.add(instruction);

        expect(TokenType.SEMICOLON);
        if(exprType.equals(type))
            if(floor == 1)
                addSymbol(name, true, type, isInitialized, floor, params, "", ident.getStartPos(), -1, null, -1, globalCount);
            else
                addSymbol(name, true, type, isInitialized, floor, params, "", ident.getStartPos(), -1, null, localCount, -1);
        else throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        if(floor == 1){
            Global global = new Global(1);
            globalTable.add(global);
        }
    }

    // 判断声明类型分析函数 
    private String analyseTy() throws CompileError {
        Token tt = peek();
        //只有void，int，double
        if(tt.getValue().equals("void") || tt.getValue().equals("int") || tt.getValue().equals("double")){
            next();
        }
        else throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        //返回类型的值
        String type = (String) tt.getValue();
        return type;
    }

    // 分析表达式
    private String analyseExpr() throws CompileError {
        String exprType = "";

        //取反表达式
        //negate_expr -> '-' expr
        if(check(TokenType.MINUS))
            exprType = analyseNegateExpr();
        //赋值表达式，函数调用表达式，标识符表达式
        else if(check(TokenType.IDENT)){
            Token ident = next();
            Symbol symbol = searchSymbolByToken(ident);
            boolean isLibrary = false;
            if(symbol == null){
                symbol = analyseLibrary((String) ident.getValue());
                // 不是库函数
                if(symbol == null)
                    throw new AnalyzeError(ErrorCode.Break, ident.getStartPos());
                isLibrary = true;
            }

            // 赋值表达式
            //assign_expr -> l_expr '=' expr
            if(check(TokenType.ASSIGN))
                //查看赋值表达式左边的标识符是否在符号表里
                exprType = analyseAssignExpr(symbol, ident);
            // 函数调用表达式
            //call_expr -> IDENT '(' call_param_list? ')'
            else if(check(TokenType.L_PAREN))
                exprType = analyseCallExpr(symbol, ident, isLibrary);
            // 标识符表达式
            //ident_expr -> IDENT
            else{
                exprType = analyseIdentExpr(symbol, ident);
            }
        }
        //字面量表达式
        //literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL
        else if(check(TokenType.UINT_LITERAL) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL)){
            exprType = analyseLiteralExpr();
        }
        //括号表达式
        //group_expr -> '(' expr ')'
        else if(check(TokenType.L_PAREN))
            exprType = analyseGroupExpr();
        //类型转换表达式，运算符表达式
        while(check(TokenType.AS_KW) ||
                check(TokenType.PLUS)||
                check(TokenType.MINUS)||
                check(TokenType.MUL)||
                check(TokenType.DIV)||
                check(TokenType.EQ)||
                check(TokenType.NEQ)||
                check(TokenType.LT)||
                check(TokenType.GT)||
                check(TokenType.LE)||
                check(TokenType.GE)){
            //类型转换表达式
            //as_expr -> expr 'as' ty
            if(check(TokenType.AS_KW))
                exprType = analyseAsExpr(exprType);
            //运算符表达式
            //operator_expr -> expr binary_operator expr
            else
                exprType = analyseOperatorExpr(exprType);
        }
        //有表达式成立
        if(!exprType.equals(""))
            return exprType;
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    // 分析取反表达式 
    private String analyseNegateExpr() throws CompileError{
        expect(TokenType.MINUS);
        String type = analyseExpr();
        //不能是void
        if(!type.equals("int") && !type.equals("double"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        instructions.add(new Instruction("neg.i", null));
        return type;
    }

    // 赋值表达式分析函数 
    private String analyseAssignExpr(Symbol symbol, Token ident) throws CompileError{
        // 等式左边ident
        // 函数参数
        if (symbol.getIsParam() != -1) {
            Symbol func = symbol.getFunction();
            if (func.getReturnType().equals("int"))
                instructions.add(new Instruction("arga", 1 + symbol.getIsParam()));
            else if(func.getReturnType().equals("double"))
                instructions.add(new Instruction("arga", 2 + symbol.getIsParam()));
            else
                instructions.add(new Instruction("arga", symbol.getIsParam()));
        }
        // 局部变量
        else if(symbol.getIsParam() == -1 && symbol.getFloor() != 1) {
            instructions.add(new Instruction("loca", symbol.getLocalId()));
        }
        // 全局变量
        else {
            instructions.add(new Instruction("globa", symbol.getGlobalId()));
        }

        // =
        expect(TokenType.ASSIGN);
        String exprType = analyseExpr();
        while (!op.empty())
            UtilFunctions.operatorInstructions(op.pop(), instructions, exprType);
        //常量报错
        if (symbol.isConst)
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        //判断类型是否一致
        else if(symbol.getType().equals(exprType) && (symbol.getType().equals("int") || symbol.getType().equals("double"))){
            initializeSymbol(symbol.getName(), peekedToken.getStartPos());
            instructions.add(new Instruction("store.64", null));
            return "void";
        }
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    // 分析函数调用表达式 
    // call_expr -> IDENT '(' call_param_list? ')'
    private String analyseCallExpr(Symbol symbol, Token ident, boolean isLibrary) throws CompileError{
        Instruction instruction;
        //库函数
        if(isLibrary){
            String name = symbol.getName();
            globalTable.add(new Global(1, name.length(), name));
            instruction = new Instruction("callname", globalCount);
            globalCount++;
        }
        else{
            if(!symbol.getType().equals("function"))
                throw new AnalyzeError(ErrorCode.Break, ident.getStartPos());
            int id = UtilFunctions.getFunctionId(symbol.getName(), functionTable);
            instruction = new Instruction("call", id + 1);
        }
        String name = symbol.getName();
        // (
        expect(TokenType.L_PAREN);
        op.push(TokenType.L_PAREN);
        if (UtilFunctions.functionHasReturn(name, functionTable))
            instructions.add(new Instruction("stackalloc", 1));
        else
            instructions.add(new Instruction("stackalloc", 0));

        if(!check(TokenType.R_PAREN)){
            analyseCallParamList(symbol);
        }
        expect(TokenType.R_PAREN);
        op.pop();

        instructions.add(instruction);
        return symbol.getReturnType();
    }

    // 判断库函数 
    private Symbol analyseLibrary(String name) throws CompileError{
        List<Symbol> params = new ArrayList<>();
        Symbol param = new Symbol();
        String returnType;
        if(name.equals("getint")){
            returnType = "int";
            return new Symbol(name, false, "function", true, 0, floor, params, returnType, -1,null,  -1, -1);
        }
        else if(name.equals("getdouble")){
            returnType = "double";
            return new Symbol(name, false, "function", true, 0, floor, params, returnType, -1,null, -1, -1);
        }
        else if(name.equals("getchar")){
            returnType = "int";
            return new Symbol(name, false, "function", true, 0, floor, params, returnType, -1,null, -1, -1);
        }
        else if(name.equals("putint")){
            returnType = "void";
            param.setType("int");
            params.add(param);
            return new Symbol(name, false, "function", true, 0, floor, params, returnType, -1,null, -1, -1);
        }
        else if(name.equals("putdouble")){
            returnType = "void";
            param.setType("double");
            params.add(param);
            return new Symbol(name, false, "function", true, 0, floor, params, returnType, -1,null, -1, -1);
        }
        else if(name.equals("putchar")){
            returnType = "void";
            param.setType("int");
            params.add(param);
            return new Symbol(name, false, "function", true, 0, floor, params, returnType, -1,null, -1, -1);
        }
        else if(name.equals("putstr")){
            returnType = "void";
            param.setType("string");
            params.add(param);
            return new Symbol(name, false, "function", true, 0, floor, params, returnType, -1,null, -1, -1);
        }
        else if(name.equals("putln")){
            returnType = "void";
            return new Symbol(name, false, "function", true, 0, floor, params, returnType, -1,null, -1, -1);
        }
        else
            return null;
    }

    // 分析调用函数参数列表 
    private void analyseCallParamList(Symbol symbol) throws CompileError{
        int i = 0;
        List<Symbol> params = symbol.getParams();
        int paramNum = params.size();
        String type = analyseExpr();
        while (!op.empty() && op.peek() != TokenType.L_PAREN)
            UtilFunctions.operatorInstructions(op.pop(), instructions, type);

        if(!params.get(i).getType().equals(type))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        i++;
        while(check(TokenType.COMMA)){
            next();
            type = analyseExpr();
            while (!op.empty() && op.peek() != TokenType.L_PAREN)
                UtilFunctions.operatorInstructions(op.pop(), instructions, type);

            if(!params.get(i).getType().equals(type))
                throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
            while (!op.empty() && op.peek() != TokenType.L_PAREN)
                UtilFunctions.operatorInstructions(op.pop(), instructions, type);
            i++;
        }
        if(i != paramNum)
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    // 分析标识符表达式
    private String analyseIdentExpr(Symbol symbol, Token ident) throws CompileError{
        if(!symbol.getType().equals("int") && !symbol.getType().equals("double"))
            throw new AnalyzeError(ErrorCode.Break, ident.getStartPos());
        //参数
        if (symbol.getIsParam() != -1) {
            Symbol func = symbol.getFunction();
            if (func.getReturnType().equals("int"))
                instructions.add(new Instruction("arga", 1 + symbol.getIsParam()));
            else if(func.getReturnType().equals("double"))
                instructions.add(new Instruction("arga", 2 + symbol.getIsParam()));
            else
                instructions.add(new Instruction("arga", symbol.getIsParam()));
        }
        //局部变量
        else if(symbol.getIsParam() == -1 && symbol.getFloor() != 1) {
            instructions.add(new Instruction("loca", symbol.getLocalId()));
        }
        //全局变量
        else {
            instructions.add(new Instruction("globa", symbol.getGlobalId()));
        }
        instructions.add(new Instruction("load.64", null));
        return symbol.getType();
    }

    // 分析字面量表达式 
    private String analyseLiteralExpr() throws CompileError{
        if(check(TokenType.UINT_LITERAL)){
            Token token = next();
            instructions.add(new Instruction("push", (Integer) token.getValue()));
            return "int";
        }
        else if(check(TokenType.DOUBLE_LITERAL)){
            Token token = next();
            String binary = Long.toBinaryString(Double.doubleToRawLongBits((Double) token.getValue()));
            instructions.add(new Instruction("push", toTen(binary)));
            return "double";
        }
        else if(check(TokenType.STRING_LITERAL)){
            Token token = next();
            String name = (String) token.getValue();
            globalTable.add(new Global(1, name.length(), name));
            instructions.add(new Instruction("push", globalCount));
            globalCount++;
            return "string";
        }
        else if(check(TokenType.CHAR_LITERAL)){
            Token token = next();
            instructions.add(new Instruction("push", (Integer) token.getValue()));
            return "int";
        }
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    // 转十进制
    public static Long toTen(String a){
        Long aws = 0L;
        Long xi = 1L;
        for(int i=a.length()-1; i>=0; i--){
            if(a.charAt(i) == '1')
                aws += xi;
            xi *=2;
        }
        return aws;
    }

    // 分析括号表达式 
    private String analyseGroupExpr() throws CompileError{
        expect(TokenType.L_PAREN);
        op.push(TokenType.L_PAREN);
        String exprType = analyseExpr();
        expect(TokenType.R_PAREN);
        while (op.peek() != TokenType.L_PAREN)
            UtilFunctions.operatorInstructions(op.pop(), instructions, exprType);
        op.pop();
        return exprType;
    }

    // 分析类型转换表达式 
    private String analyseAsExpr(String exprType) throws CompileError{
        expect(TokenType.AS_KW);
        String rightType =  analyseTy();
        if(exprType.equals("int") && rightType.equals("double")){
            instructions.add(new Instruction("itof", null));
            return "double";
        }
        else if(exprType.equals("double") && rightType.equals("int")){
            instructions.add(new Instruction("ftoi", null));
            return "int";
        }
        else if(exprType.equals(rightType)){
            return exprType;
        }
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

    }


    // 分析运算符表达式 算符优先 
    private String analyseOperatorExpr(String exprType) throws CompileError{
        Token token = analyseBinaryOperator();
        if (!op.empty()) {
            int in = Operator.getOrder(op.peek());
            int out = Operator.getOrder(token.getTokenType());
            if (Operator.priority[in][out] > 0)
                UtilFunctions.operatorInstructions(op.pop(), instructions, exprType);
        }
        op.push(token.getTokenType());

        String type =  analyseExpr();
        if(exprType.equals(type) && (exprType.equals("int") || exprType.equals("double")))
            return type;
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    // 二元运算符分析函数 
    private Token analyseBinaryOperator() throws CompileError{
        if(check(TokenType.AS_KW) ||
                check(TokenType.PLUS)||
                check(TokenType.MINUS)||
                check(TokenType.MUL)||
                check(TokenType.DIV)||
                check(TokenType.EQ)||
                check(TokenType.NEQ)||
                check(TokenType.LT)||
                check(TokenType.GT)||
                check(TokenType.LE)||
                check(TokenType.GE)){
            return next();
        }
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    // 分析函数声明 
    private void analyseFunction() throws CompileError{
        localCount = 0;
        String name;
        List<Symbol> params = new ArrayList<>();
        String returnType = "";
        Token ident;
        expect(TokenType.FN_KW);
        ident = expect(TokenType.IDENT);
        name = (String) ident.getValue();
        expect(TokenType.L_PAREN);
        addSymbol(name, true, "function", true, floor, params, returnType, ident.getStartPos(), -1,null, -1, globalCount);
        Symbol symbol = searchSymbolByToken(ident);
        if(!check(TokenType.R_PAREN))
            analyseFunctionParamList(params, symbol);
        expect(TokenType.R_PAREN);

        expect(TokenType.ARROW);
        returnType = analyseTy();
        symbol.setParams(params);
        symbol.setReturnType(returnType);
        presentFunction = symbol;

        int retSlot = 0;
        if(returnType.equals("int")) retSlot = 1;
        else if(returnType.equals("double")) retSlot = 1;
        //插入函数表
        Function function = new Function(name, globalCount, retSlot, params.size(), localCount, instructions, floor, returnType);
        functionTable.add(function);

        //分析代码块
        analyseBlockStmt();

        function.setId(globalCount);
        function.setLocSlots(localCount);
        function.setBody(instructions);
        function.setFloor(floor);

        //验证return
        if(symbol.getReturnType().equals("void"))
            instructions.add(new Instruction("ret", null));
        else if(!returnFunction.getName().equals(presentFunction.getName())){
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        }

        //将函数插入全局变量表
        Global global = new Global(1, name.length(), name);
        globalTable.add(global);
    }

    // 分析函数参数列表 
    private void analyseFunctionParamList(List<Symbol> params, Symbol symbol) throws CompileError{
        int i = 0;
        params.add(analyseFunctionParam(i, symbol));
        while(check(TokenType.COMMA)){
            next();
            params.add(analyseFunctionParam(++i, symbol));
        }
    }

    // 分析函数参数 
    private Symbol analyseFunctionParam(int i, Symbol symbol) throws CompileError{
        String name;
        boolean isConst = false;
        String type;
        boolean isInitialized = false;
        Token ident;

        if(check(TokenType.CONST_KW)){
            isConst = true;
            next();
        }
        ident = expect(TokenType.IDENT);
        expect(TokenType.COLON);
        type = analyseTy();
        name = (String) ident.getValue();
        //插入符号表
        addSymbol(name, isConst, type, isInitialized, floor+1, null, "", ident.getStartPos(), i, symbol, -1, -1);
        return searchSymbolByToken(ident);
    }

    // 分析代码块 
    private void analyseBlockStmt() throws CompileError{
        //层数+1
        floor++;
        expect(TokenType.L_BRACE);
        while(!check(TokenType.R_BRACE))
            analyseStmt();
        expect(TokenType.R_BRACE);

        for(int i = symbolTable.size()-1; symbolTable.get(i).getFloor()==floor; i--)
            symbolTable.remove(i);
        floor--;
    }

    // 分析语句
    private void analyseStmt() throws CompileError{
        //声明语句
        if(check(TokenType.LET_KW)||check(TokenType.CONST_KW))
            analyseDeclStmt();
        //if语句
        else if(check(TokenType.IF_KW))
            analyseIfStmt();
        //while语句
        else if(check(TokenType.WHILE_KW))
            analyseWhileStmt();
        //return语句
        else if(check(TokenType.RETURN_KW))
            analyseReturnStmt();
        //代码块
        else if(check(TokenType.L_BRACE))
            analyseBlockStmt();
        //空语句
        else if(check(TokenType.SEMICOLON))
            analyseEmptyStmt();
        //break语句
        else if(check(TokenType.BREAK_KW)){
            analyseBreakStmt();
        }
        //continue语句
        else if(check(TokenType.CONTINUE_KW)){
            analyseContinueStmt();
        }
        //表达式语句
        else
            analyseExprStmt();
    }

    // 分析if语句 
    private void analyseIfStmt() throws CompileError{
        expect(TokenType.IF_KW);
        String type = analyseExpr();
        while (!op.empty())
            UtilFunctions.operatorInstructions(op.pop(), instructions, type);
        if(!type.equals("int") && !type.equals("double"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        instructions.add(new Instruction("br.true", 1));
        Instruction jump = new Instruction("br", 0);
        instructions.add(jump);
        int index = instructions.size();

        analyseBlockStmt();

        int size = instructions.size();

        if (instructions.get(size -1).getOp().equals("ret")) {
            int distance = instructions.size() - index;
            jump.setX(distance);
            if(check(TokenType.ELSE_KW)){
                expect(TokenType.ELSE_KW);
                if(check(TokenType.L_BRACE)){
                    analyseBlockStmt();
                    instructions.add(new Instruction("br", 0));
                }
                else if(check(TokenType.IF_KW))
                    analyseIfStmt();
            }
        }
        else {
            Instruction jumpInstruction = new Instruction("br", null);
            instructions.add(jumpInstruction);
            int j = instructions.size();
            int distance = j - index;
            jump.setX(distance);
            if(check(TokenType.ELSE_KW)){
                expect(TokenType.ELSE_KW);
                if(check(TokenType.L_BRACE)){
                    analyseBlockStmt();
                    instructions.add(new Instruction("br", 0));
                }
                else if(check(TokenType.IF_KW))
                    analyseIfStmt();
            }
            distance = instructions.size() - j;
            jumpInstruction.setX(distance);
        }
    }

    // 分析while语句 
    private void analyseWhileStmt() throws CompileError{
        expect(TokenType.WHILE_KW);

        instructions.add(new Instruction("br", 0));
        int whileStart = instructions.size();

        String type = analyseExpr();
        while (!op.empty())
            UtilFunctions.operatorInstructions(op.pop(), instructions, type);
        if(!type.equals("int") && !type.equals("double"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        instructions.add(new Instruction("br.true", 1));
        Instruction jumpInstruction = new Instruction("br", 0);
        instructions.add(jumpInstruction);
        int index = instructions.size();

        isInWhile++;
        analyseBlockStmt();
        if(isInWhile > 0) isInWhile--;

        Instruction instruction = new Instruction("br", 0);
        instructions.add(instruction);
        int whileEnd = instructions.size();
        instruction.setX(whileStart - whileEnd);

        if(breakInstruction.size()!=0){
            for(BreakAndContinue b:breakInstruction){
                if(b.getWhileNum() == isInWhile+1)
                    b.getInstruction().setX(whileEnd - b.getLocation());
            }
        }

        if(continueInstruction.size() != 0){
            for(BreakAndContinue c:continueInstruction){
                if(c.getWhileNum() == isInWhile+1)
                    c.getInstruction().setX(whileEnd - c.getLocation() - 1);
            }
        }
        jumpInstruction.setX(whileEnd - index);
        System.out.println("第一个r" + (whileEnd - index));
        if(isInWhile == 0){
            continueInstruction = new ArrayList<BreakAndContinue>();
            breakInstruction = new ArrayList<BreakAndContinue>();
        }
    }


    // 分析return语句 
    private void analyseReturnStmt() throws CompileError{
        expect(TokenType.RETURN_KW);
        String type = "void";
        if(!presentFunction.getReturnType().equals("void")){
            instructions.add(new Instruction("arga", 0));
            type = analyseExpr();
            while (!op.empty())
                UtilFunctions.operatorInstructions(op.pop(), instructions, type);

            instructions.add(new Instruction("store.64", null));
        }

        if(!check(TokenType.SEMICOLON))
            type = analyseExpr();

        //是否一致
        if(!type.equals(presentFunction.getReturnType()))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        expect(TokenType.SEMICOLON);
        returnFunction = presentFunction;

        while (!op.empty())
            UtilFunctions.operatorInstructions(op.pop(), instructions, type);
        instructions.add(new Instruction("ret", null));
    }

    // 分析空语句 
    private void analyseEmptyStmt() throws CompileError{
        expect(TokenType.SEMICOLON);
    }

    // 分析break语句 
    private void analyseBreakStmt() throws CompileError{
        expect(TokenType.BREAK_KW);
        if(isInWhile == 0)
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        Instruction instruction = new Instruction("br", 0);
        breakInstruction.add(new BreakAndContinue(instruction, instructions.size()+1, isInWhile));
        instructions.add(instruction);
        expect(TokenType.SEMICOLON);
    }

    // 分析continue语句
    private void analyseContinueStmt() throws CompileError{
        expect(TokenType.CONTINUE_KW);
        if(isInWhile == 0)
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        Instruction instruction = new Instruction("br", 0);
        continueInstruction.add(new BreakAndContinue(instruction, instructions.size()+1, isInWhile));
        instructions.add(instruction);
        expect(TokenType.SEMICOLON);
    }

    // 分析表达式语句 
    private void analyseExprStmt() throws CompileError{
        String exprType = analyseExpr();
        while (!op.empty())
            UtilFunctions.operatorInstructions(op.pop(), instructions, exprType);
        expect(TokenType.SEMICOLON);
    }

    public List<Global> getGlobalTable() {
        return globalTable;
    }

    public void setGlobalTable(List<Global> globalTable) {
        this.globalTable = globalTable;
    }

    public List<Function> getFunctionTable() {
        return functionTable;
    }

    public void setFunctionTable(List<Function> functionTable) {
        this.functionTable = functionTable;
    }

    public Function get_start() {
        return _start;
    }

    public void set_start(Function _start) {
        this._start = _start;
    }

}
