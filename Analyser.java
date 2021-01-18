import java.sql.SQLOutput;
import java.util.*;

public final class Analyser {

    Tokenizer tokenizer;
    /** 当前偷看的 token */
    Token peekedToken = null;
    /** 符号表 */
    List<Symbol> symbolTable = new ArrayList<>();
    /** 全局符号（变量+常量）表 */
    List<Global> globalTable = new ArrayList<>();
    /** 函数表 */
    List<Function> functionTable = new ArrayList<>();
    /** 开始函数 */
    Function _start;
    /** 层数 */
    int floor = 1;
    /** 当前正在声明的函数 */
    Symbol nowFuntion;
    /** 刚刚返回的函数*/
    Symbol returnFunction;
    /** 下一个变量的栈偏移 */
    int nextOffset = 0;
    /** 全局符号（变量+常量）个数 */
    int globalCount = 0;
    /** 函数个数 */
    int functionCount = 1;
    /** 局部变量个数 */
    int localCount = 0;
    /** 操作符号栈 */
    Stack<TokenType> op = new Stack<>();
    /** 指令集合 */
    ArrayList<Instruction> instructions;
    /** 是否在while循环体里面 */
    //如果不在循环里则为0，如果在循环里在几层循环则为几
    int isInWhile = 0;
    List<BreakAndContinue> continueInstruction = new ArrayList<BreakAndContinue>();
    List<BreakAndContinue> breakInstruction = new ArrayList<BreakAndContinue>();


    public Analyser(Tokenizer tokenizer) {
        this.tokenizer = tokenizer;
    }

    public void analyse() throws CompileError {
        analyseProgram();
    }

    /**
     * 查看下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token peek() throws TokenizeError {
        if (peekedToken == null) {
            peekedToken = tokenizer.nextToken();
        }
        return peekedToken;
    }

    /**
     * 获取下一个 Token
     *
     * @return
     * @throws TokenizeError
     */
    private Token next() throws TokenizeError {
        if (peekedToken != null) {
            Token token = peekedToken;
            peekedToken = null;
            System.out.print(token.getValue() + " ");
            return token;
        } else {
            return tokenizer.nextToken();
        }
    }

    /**
     * 如果下一个 token 的类型是 tt，则返回 true
     *
     * @param tt
     * @return
     * @throws TokenizeError
     */
    private boolean check(TokenType tt) throws TokenizeError {
        Token token = peek();
        return token.getTokenType() == tt;
    }


    /**
     * 如果下一个 token 的类型是 tt，则前进一个 token 并返回，否则抛出异常
     *
     * @param tt 类型
     * @return 这个 token
     * @throws CompileError 如果类型不匹配
     */
    private Token expect(TokenType tt) throws CompileError {
        Token token = peek();
        if (token.getTokenType() == tt) {
            return next();
        }
        else {
            throw new ExpectedTokenError(tt, token);
        }
    }

    /**
     * 获取下一个变量的栈偏移
     *
     * @return
     */
    private int getNextVariableOffset() {
        return this.nextOffset++;
    }

    /**
     * 根据符号名字查找当前符号表里是否有该名字，如果有则返回位置，如果没有则返回-1
     * @param name
     * @return
     */
    private int searchSymbolByName(String name){
        //在符号表里查找
        for(int i=0; i<symbolTable.size(); i++){
            //如果遇到名字相同的则返回位置
            if(symbolTable.get(i).getName().equals(name)) return i;
        }
        //符号表里没有名字相同的，返回-1
        return -1;
    }

    /**
     * 根据Token在符号表里查是否有与该token名字一样的符号，如果有则返回该符号，没有返回null
     * @param token
     * @return
     */
    private Symbol searchSymbolByToken(Token token){
        String name = (String) token.getValue();
        //在符号表里查找
        for(int i=symbolTable.size()-1; i>=0; i--){
            //如果遇到名字相同的则返回位置
            if(symbolTable.get(i).getName().equals(name)) return symbolTable.get(i);
        }
        //符号表里没有名字相同的，返回null
        return null;
    }

    /**
     * 添加一个符号
     * @param name  名字
     * @param type  类型
     * @param isInitialized 是否已赋值
     * @param floor 当前层数，遇到函数则+1
     * @param params    函数的参数列表
     * @param curPos    当前 token 的位置（报错用）
     * @param returnType 函数的返回类型
     * @param isParam 该符号是参数吗
     */
    private void addSymbol(String name, boolean isConst, String type, boolean isInitialized, int floor, List<Symbol> params, String returnType, Pos curPos, int isParam, Symbol function, int localId, int globalId) throws AnalyzeError {
        //判断在符号表里有没有与当前符号相同的名字
        int same = searchSymbolByName(name);
        //如果没有一样的名字
        if(same == -1)
            this.symbolTable.add(new Symbol(name, isConst, type, isInitialized, getNextVariableOffset(), floor, params, returnType, isParam, function, localId, globalId));
        //如果有一样的名字
        else{
            //获得该一样名字的符号
            Symbol symbol = symbolTable.get(same);
            //如果他们层数一样则报错
            if(symbol.getFloor() == floor)
                throw new AnalyzeError(ErrorCode.DuplicateDeclaration, curPos);
            //如果层数不一样则加入符号表
            this.symbolTable.add(new Symbol(name, isConst, type, isInitialized, getNextVariableOffset(), floor, params, returnType, isParam, function, localId, globalId));
        }
    }

    /**
     * 设置符号为已赋值
     * @param name   符号名称
     * @param curPos 当前位置（报错用）
     * @throws AnalyzeError 如果未定义则抛异常
     */
    private void initializeSymbol(String name, Pos curPos) throws AnalyzeError {
        int position = searchSymbolByName(name);
        //符号表里没有该名字
        if (position == -1) {
            throw new AnalyzeError(ErrorCode.NotDeclared, curPos);
        }
        //如果有则将他设置为已赋值
        else {
            Symbol update = symbolTable.get(position);
            update.setInitialized(true);
        }
    }


    /**
     * 主程序分析函数
     * @throws CompileError
     */
    private void analyseProgram() throws CompileError {
        //初始化指令集
        instructions = new ArrayList<>();
        // (1)program -> decl_stmt* function*

        // (2)decl_stmt -> let_decl_stmt | const_decl_stmt
        while(check(TokenType.LET_KW)||check(TokenType.CONST_KW))
            analyseDeclStmt();

        //在此前声明的语句都属于初始化操作，接下来函数部分的instruction是自己内部的，_start的相关操作也要加入init
        List<Instruction> initInstructions = instructions;
        // (3)function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
        while(check(TokenType.FN_KW)){
            //更新指令集
            instructions = new ArrayList<>();

            analyseFunction();

            //全局变量个数+一
            globalCount++;
            //函数个数+一
            functionCount++;
        }

        //判断符号表里有没有主程序入口
        //如果没有则报错
        int mainLoca = searchSymbolByName("main");
        if(mainLoca == -1){
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        }


        //在全局符号表里填入入口程序_start
        globalTable.add(new Global(1, 6, "_start"));
        //找到main函数对应的符号
        Symbol main = symbolTable.get(mainLoca);
        if (!main.getReturnType().equals("void")) {
            //加载地址
            initInstructions.add(new Instruction("stackalloc", 1));
            initInstructions.add(new Instruction("call", functionCount-1));
            initInstructions.add(new Instruction("popn", 1));
        }
        else {
            //加载地址
            initInstructions.add(new Instruction("stackalloc", 0));
            initInstructions.add(new Instruction("call", functionCount-1));
        }
        _start = new Function("_start", globalCount, 0, 0, 0, initInstructions, floor, "void");
        globalCount++;
    }

    /**
     * 声明语句分析函数
     * decl_stmt -> let_decl_stmt | const_decl_stmt
     * @throws CompileError
     */
    private void analyseDeclStmt() throws CompileError{
        if(check(TokenType.LET_KW))
            analyseLetDeclStmt();
        else if(check(TokenType.CONST_KW))
            analyseConstDeclStmt();

        //如果是在最外层，全局变量+1；否则局部变量+1
        if(floor == 1) globalCount++;
        else localCount++;
    }

    /**
     * let声明语句分析函数
     * let_decl_stmt -> 'let' IDENT ':' ty ('=' expr)? ';'
     * @throws CompileError
     */
    private void analyseLetDeclStmt() throws CompileError{
        String name;
        boolean isConst = false;
        String type;
        boolean isInitialized = false;
        List<Symbol> params = null;
        String exprType = "";
        Token ident;

        expect(TokenType.LET_KW);
        //遇到标识符，将名字记录
        ident = expect(TokenType.IDENT);
        name = (String) ident.getValue();
        expect(TokenType.COLON);
        //遇到类型，将返回的类型记录
        type = analyseTy();
        //非函数不能声明为void类型
        if(type.equals("void"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());


        //如果遇到等号，则该声明给赋了值
        if(check(TokenType.ASSIGN)){
            isInitialized = true;
            Instruction instruction;

            //加入取地址操作
            if (floor == 1) {
                instruction = new Instruction("globa", globalCount);
                instructions.add(instruction);
            }
            else {
                instruction = new Instruction("loca", localCount);
                instructions.add(instruction);
            }
            next();
            exprType = analyseExpr();
            //将运算符弹栈并计算
            while (!op.empty())
                MyFunctions.operatorInstructions(op.pop(), instructions, exprType);

            //将值存入
            instruction = new Instruction("store.64", null);
            instructions.add(instruction);
        }

        expect(TokenType.SEMICOLON);
        //如果初始化且表达式的返回类型和声明类型一致，或者没有初始化
        if((isInitialized && exprType.equals(type)) || !isInitialized)
            //将该符号加入符号表
            //如果是全局变量
            if(floor == 1)
                addSymbol(name, false, type, isInitialized, floor, params, "", ident.getStartPos(), -1, null, -1, globalCount);
            //如果是局部变量
            else
                addSymbol(name, false, type, isInitialized, floor, params, "", ident.getStartPos(), -1, null, localCount, -1);
        //如果不一致则报错
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        //如果当前声明的是全局变量，存入全局符号表
        if(floor == 1){
            Global global = new Global(0);
            globalTable.add(global);
        }
    }

    /**
     * const声明语句分析函数
     * const_decl_stmt -> 'const' IDENT ':' ty '=' expr ';'
     * @throws CompileError
     */
    private void analyseConstDeclStmt() throws CompileError{
        String name;
        String type;
        boolean isInitialized = true;
        List<Symbol> params = null;
        String exprType;
        Token ident;
        Instruction instruction;

        expect(TokenType.CONST_KW);
        //遇到标识符，将名字记录
        ident = expect(TokenType.IDENT);
        name = (String) ident.getValue();
        expect(TokenType.COLON);
        //遇到类型，将返回的类型记录
        type = analyseTy();
        //非函数不能声明为void类型
        if(type.equals("void"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        //加入取地址操作
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
        //将运算符弹栈并计算
        while (!op.empty())
            MyFunctions.operatorInstructions(op.pop(), instructions, exprType);

        //将值存入
        instruction = new Instruction("store.64", null);
        instructions.add(instruction);

        expect(TokenType.SEMICOLON);
        //如果表达式的返回类型和声明类型一致，则插入符号表
        if(exprType.equals(type))
            //将该符号加入符号表
            //如果是全局变量
            if(floor == 1)
                addSymbol(name, true, type, isInitialized, floor, params, "", ident.getStartPos(), -1, null, -1, globalCount);
                //如果是局部变量
            else
                addSymbol(name, true, type, isInitialized, floor, params, "", ident.getStartPos(), -1, null, localCount, -1);
        //如果不一致则报错
        else throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        //如果当前声明的是全局变量，存入全局符号表
        if(floor == 1){
            Global global = new Global(1);
            globalTable.add(global);
        }
    }

    /**
     * 判断声明类型分析函数
     * @return
     * @throws CompileError
     */
    private String analyseTy() throws CompileError {
        Token tt = peek();
        //如果类型是void，int，double
        if(tt.getValue().equals("void") || tt.getValue().equals("int") || tt.getValue().equals("double")){
            next();
        }
        //如果不是以上类型，抛出编译异常
        else throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        //返回类型的值
        String type = (String) tt.getValue();
        return type;
    }

    /**
     * 表达式分析函数
     * expr ->
     *   operator_expr
     * | negate_expr
     * | assign_expr
     * | as_expr
     * | call_expr
     * | literal_expr
     * | ident_expr
     * | group_expr
     * | group_expr
     * 在operator和as里为了消除左递归，可以将其变为
     * expr -> (binary_operator expr||'as' ty)*
     * @return
     * @throws CompileError
     */
    private String analyseExpr() throws CompileError {
        //当前表达式的类型
        String exprType = "";

        //取反表达式
        //如果首位是负号
        //negate_expr -> '-' expr
        if(check(TokenType.MINUS))
            exprType = analyseNegateExpr();

        //赋值表达式，函数调用表达式，标识符表达式
        //如果首位是标识符
        else if(check(TokenType.IDENT)){
            //获得该ident对应的符号，并传入表达式分析函数
            Token ident = next();
            Symbol symbol = searchSymbolByToken(ident);
            //该符号是否是库函数
            boolean isLibrary = false;
            //如果符号表里没有该ident
            if(symbol == null){
                //且该符号也不是库函数
                symbol = analyseLibrary((String) ident.getValue());
                if(symbol == null)
                    throw new AnalyzeError(ErrorCode.Break, ident.getStartPos());
                isLibrary = true;
            }

            //assign_expr -> l_expr '=' expr
            //l_expr -> IDENT
            if(check(TokenType.ASSIGN))
                //查看赋值表达式左边的标识符是否在符号表里
                exprType = analyseAssignExpr(symbol, ident);

            //call_expr -> IDENT '(' call_param_list? ')'
            else if(check(TokenType.L_PAREN))
                exprType = analyseCallExpr(symbol, ident, isLibrary);

            //ident_expr -> IDENT
            else{
                exprType = analyseIdentExpr(symbol, ident);
            }
        }

        //字面量表达式
        //如果首位是无符号数、浮点数、字符串常量
        //literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL
        else if(check(TokenType.UINT_LITERAL) || check(TokenType.DOUBLE_LITERAL) || check(TokenType.STRING_LITERAL) || check(TokenType.CHAR_LITERAL)){
            exprType = analyseLiteralExpr();
        }



        //括号表达式
        //如果首位是左括号
        //group_expr -> '(' expr ')'
        else if(check(TokenType.L_PAREN))
            exprType = analyseGroupExpr();

        //类型转换表达式，运算符表达式
        //如果依旧有expr
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
        //如果成功给表达式赋了类型，则说明上面至少有表达式成立
        if(!exprType.equals(""))
            return exprType;
        //根本不符合表达式语句
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    /**
     * 取反表达式分析函数
     * negate_expr -> '-' expr
     * @return
     * @throws CompileError
     */
    private String analyseNegateExpr() throws CompileError{
        expect(TokenType.MINUS);
        String type = analyseExpr();
        //取反后面的表达式只能是int或double，否则报错
        if(!type.equals("int") && !type.equals("double"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        instructions.add(new Instruction("neg.i", null));
        return type;
    }

    /**
     * 赋值表达式分析函数
     * assign_expr -> l_expr '=' expr
     * l_expr -> IDENT
     * @param symbol 该赋值表达式左侧的符号
     * @return
     * @throws CompileError
     */
    private String analyseAssignExpr(Symbol symbol, Token ident) throws CompileError{
        //加载等式左边的符号
        //如果该ident是参数
        if (symbol.getIsParam() != -1) {
            //获取该参数的函数
            Symbol func = symbol.getFunction();

            //参数存在ret_slots后面
            if (func.getReturnType().equals("int"))
                instructions.add(new Instruction("arga", 1 + symbol.getIsParam()));
            else if(func.getReturnType().equals("double"))
                instructions.add(new Instruction("arga", 2 + symbol.getIsParam()));
            else
                instructions.add(new Instruction("arga", symbol.getIsParam()));
        }
        //如果该ident是局部变量
        else if(symbol.getIsParam() == -1 && symbol.getFloor() != 1) {
            instructions.add(new Instruction("loca", symbol.getLocalId()));
        }
        //如果该ident是全局变量
        else {
            instructions.add(new Instruction("globa", symbol.getGlobalId()));
        }

        //在符号表里，继续分析
        expect(TokenType.ASSIGN);
        String exprType = analyseExpr();
        //弹栈
        while (!op.empty())
            MyFunctions.operatorInstructions(op.pop(), instructions, exprType);

        //如果等式左边是常量，则报错
        if (symbol.isConst)
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        //如果等式左边是int或者double，判断类型和右边是否一样
        else if(symbol.getType().equals(exprType) && (symbol.getType().equals("int") || symbol.getType().equals("double"))){
            //设置该符号为已赋值
            initializeSymbol(symbol.getName(), peekedToken.getStartPos());
            //存储到地址中
            instructions.add(new Instruction("store.64", null));
            return "void";
        }
        //以上情况都不是，报错
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

    }

    /**
     * 函数调用表达式分析函数
     * call_expr -> IDENT '(' call_param_list? ')'
     * @param symbol    函数符号symbol
     * @param ident 函数名对应的token
     * @return
     * @throws CompileError
     */
    private String analyseCallExpr(Symbol symbol, Token ident, boolean isLibrary) throws CompileError{
        Instruction instruction;

        //如果是库函数或者没有声明该函数
        if(isLibrary){
            //如果是库函数，则将调用操作记录
            //并将库函数加入全局符号表
            String name = symbol.getName();
            globalTable.add(new Global(1, name.length(), name));
            instruction = new Instruction("callname", globalCount);
            globalCount++;
        }

        //如果找到了该symbol，且对应函数不是库函数
        else{
            //如果symbol不是函数类型，则报错
            if(!symbol.getType().equals("function"))
                throw new AnalyzeError(ErrorCode.Break, ident.getStartPos());
            //如果是函数
            int id = MyFunctions.getFunctionId(symbol.getName(), functionTable);
            instruction = new Instruction("call", id + 1);
        }

        String name = symbol.getName();
        expect(TokenType.L_PAREN);
        //将左括号入运算符栈
        op.push(TokenType.L_PAREN);

        //给返回值分配空间
        //分配返回值空间
        if (MyFunctions.functionHasReturn(name, functionTable))
            instructions.add(new Instruction("stackalloc", 1));
        else
            instructions.add(new Instruction("stackalloc", 0));

        if(!check(TokenType.R_PAREN)){
            analyseCallParamList(symbol);
        }
        expect(TokenType.R_PAREN);

        //弹出左括号
        op.pop();

        //此时再将call语句压入
        instructions.add(instruction);
        //返回函数的返回类型
        return symbol.getReturnType();
    }

    /**
     * 库函数判断
     * @param name 传入函数名字，判断改名字是否属于库函数
     * @return 如果确实是库函数则返回该symbol对象，如果不是就返回null
     * @throws CompileError
     */
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

    /**
     * 函数调用参数分析函数
     * call_param_list -> expr (',' expr)*
     * @param symbol 该函数的符号信息
     * @throws CompileError
     */
    private void analyseCallParamList(Symbol symbol) throws CompileError{
        //获取该函数的参数列表params和参数个数paramNum
        int i = 0;
        List<Symbol> params = symbol.getParams();
        int paramNum = params.size();

        //如果对应位置的参数类型不匹配，则报错
        String type = analyseExpr();
        while (!op.empty() && op.peek() != TokenType.L_PAREN)
            MyFunctions.operatorInstructions(op.pop(), instructions, type);

        if(!params.get(i).getType().equals(type))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        i++;

        while(check(TokenType.COMMA)){
            next();
            //如果对应位置的参数类型不匹配，则报错
            type = analyseExpr();
            while (!op.empty() && op.peek() != TokenType.L_PAREN)
                MyFunctions.operatorInstructions(op.pop(), instructions, type);

            if(!params.get(i).getType().equals(type))
                throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
            while (!op.empty() && op.peek() != TokenType.L_PAREN)
                MyFunctions.operatorInstructions(op.pop(), instructions, type);
            i++;
        }
        //如果参数个数不匹配，则报错
        if(i != paramNum)
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    private String analyseIdentExpr(Symbol symbol, Token ident) throws CompileError{
        if(!symbol.getType().equals("int") && !symbol.getType().equals("double"))
            throw new AnalyzeError(ErrorCode.Break, ident.getStartPos());
        //如果该ident是参数
        if (symbol.getIsParam() != -1) {
            //获取该参数的函数
            Symbol func = symbol.getFunction();
            //参数存在ret_slots后面
            if (func.getReturnType().equals("int"))
                instructions.add(new Instruction("arga", 1 + symbol.getIsParam()));
            else if(func.getReturnType().equals("double"))
                instructions.add(new Instruction("arga", 2 + symbol.getIsParam()));
            else
                instructions.add(new Instruction("arga", symbol.getIsParam()));
        }
        //如果该ident是局部变量
        else if(symbol.getIsParam() == -1 && symbol.getFloor() != 1) {
            instructions.add(new Instruction("loca", symbol.getLocalId()));
        }
        //如果该ident是全局变量
        else {
            instructions.add(new Instruction("globa", symbol.getGlobalId()));
        }
        instructions.add(new Instruction("load.64", null));
        return symbol.getType();
    }

    /**
     * 字面量表达式分析函数
     * literal_expr -> UINT_LITERAL | DOUBLE_LITERAL | STRING_LITERAL
     * @return
     * @throws CompileError
     */
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
            //加入全局符号表
            globalTable.add(new Global(1, name.length(), name));

            //加入指令集
            instructions.add(new Instruction("push", globalCount));
            globalCount++;
            return "string";
        }
        else if(check(TokenType.CHAR_LITERAL)){
            Token token = next();
            //加入指令集
            instructions.add(new Instruction("push", (Integer) token.getValue()));
            return "int";
        }
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

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

    /**
     * 括号表达式分析函数
     * group_expr -> '(' expr ')'
     * @return
     * @throws CompileError
     */
    private String analyseGroupExpr() throws CompileError{
        expect(TokenType.L_PAREN);
        //将左括号入栈
        op.push(TokenType.L_PAREN);
        String exprType = analyseExpr();
        expect(TokenType.R_PAREN);

        //弹栈
        while (op.peek() != TokenType.L_PAREN)
            MyFunctions.operatorInstructions(op.pop(), instructions, exprType);

        //弹出左括号
        op.pop();
        return exprType;
    }

    /**
     * 类型转换表达式分析函数
     * as_expr -> expr 'as' ty
     * 消除左递归后变为
     * expr -> ('as' ty)*
     * @return
     * @throws CompileError
     */
    private String analyseAsExpr(String exprType) throws CompileError{
        expect(TokenType.AS_KW);
        String rightType =  analyseTy();
        //如果是将int转为double
        if(exprType.equals("int") && rightType.equals("double")){
            instructions.add(new Instruction("itof", null));
            return "double";
        }
        //如果是将double转为int
        else if(exprType.equals("double") && rightType.equals("int")){
            instructions.add(new Instruction("ftoi", null));
            return "int";
        }
        //如果是类型转换为自己
        else if(exprType.equals(rightType)){
            return exprType;
        }
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

    }


    /**
     * 运算符表达式分析函数
     * operator_expr -> expr binary_operator expr
     * binary_operator -> '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '>' | '<=' | '>='
     * 消除左递归后变为
     * expr -> (binary_operator expr)*
     * @param exprType
     * @return
     * @throws CompileError
     */
    private String analyseOperatorExpr(String exprType) throws CompileError{
        Token token = analyseBinaryOperator();

        //比较终结符优先级，判断要不要计算
        //如果栈内终结符优先级高，则弹出该终结符，并计算
        if (!op.empty()) {
            int in = Operator.getOrder(op.peek());
            int out = Operator.getOrder(token.getTokenType());
            if (Operator.priority[in][out] > 0)
                MyFunctions.operatorInstructions(op.pop(), instructions, exprType);
        }
        op.push(token.getTokenType());

        String type =  analyseExpr();
        //如果运算符左右两侧类型一致，且为double或int
        if(exprType.equals(type) && (exprType.equals("int") || exprType.equals("double")))
            return type;
        //否则报错
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    /**
     * 二元运算符分析函数
     * @throws CompileError
     */
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
        //不是以上类型
        else
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
    }

    /**
     * 函数声明分析函数
     * function -> 'fn' IDENT '(' function_param_list? ')' '->' ty block_stmt
     * @throws CompileError
     */
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
        //在参数符号之前先插入函数符号
        //函数声明时floor还不加，属于上一层
        addSymbol(name, true, "function", true, floor, params, returnType, ident.getStartPos(), -1,null, -1, globalCount);
        Symbol symbol = searchSymbolByToken(ident);
        //开始插入参数符号
        if(!check(TokenType.R_PAREN))
            analyseFunctionParamList(params, symbol);
        expect(TokenType.R_PAREN);

        expect(TokenType.ARROW);
        returnType = analyseTy();
        //将参数列表和返回类型赋值
        symbol.setParams(params);
        symbol.setReturnType(returnType);
        //记录当前正在检查的函数符号，以便在返回时判断类型是否一致
        nowFuntion = symbol;

        int retSlot = 0;
        if(returnType.equals("int")) retSlot = 1;
        else if(returnType.equals("double")) retSlot = 1;
        //将函数插入函数表
        Function function = new Function(name, globalCount, retSlot, params.size(), localCount, instructions, floor, returnType);
        functionTable.add(function);

        //分析函数块
        analyseBlockStmt();

        //更改局部变量个数
        function.setId(globalCount);
        function.setLocSlots(localCount);
        function.setBody(instructions);
        function.setFloor(floor);

        //验证当前函数是否有return语句
        //如果当前函数返回void，则可以没有return语句
        //即判断上一个return的函数和当前函数是否一致
        //不一致则报错
        if(symbol.getReturnType().equals("void"))
            instructions.add(new Instruction("ret", null));
        else if(!returnFunction.getName().equals(nowFuntion.getName())){
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        }


        //将函数插入全局变量表
        Global global = new Global(1, name.length(), name);
        globalTable.add(global);
    }

    /**
     * 函数参数列表分析函数
     * function_param_list -> function_param (',' function_param)*
     * @param params 函数参数列表，填入该参数列表
     * @throws CompileError
     */
    private void analyseFunctionParamList(List<Symbol> params, Symbol symbol) throws CompileError{
        int i = 0;
        params.add(analyseFunctionParam(i, symbol));
        while(check(TokenType.COMMA)){
            next();
            params.add(analyseFunctionParam(++i, symbol));
        }
    }

    /**
     * 函数参数分析函数
     * function_param -> 'const'? IDENT ':' ty
     * @return
     * @throws CompileError
     */
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
        //获取名字
        name = (String) ident.getValue();
        //插入符号表，此时因为还没进入代码块，所以层数还没+1，手动将层数+1
        addSymbol(name, isConst, type, isInitialized, floor+1, null, "", ident.getStartPos(), i, symbol, -1, -1);
        return searchSymbolByToken(ident);
    }

    /**
     * 代码块分析函数
     * block_stmt -> '{' stmt* '}'
     * @throws CompileError
     */
    private void analyseBlockStmt() throws CompileError{
        //在块里就将层数+1
        floor++;
        expect(TokenType.L_BRACE);
        while(!check(TokenType.R_BRACE))
            analyseStmt();
        expect(TokenType.R_BRACE);

        //分析完后开始退栈，只保留函数符号
        for(int i = symbolTable.size()-1; symbolTable.get(i).getFloor()==floor; i--)
            symbolTable.remove(i);
        //出块后就将层数-1
        floor--;
    }

    /**
     * 语句分析函数
     * stmt ->
     *   expr_stmt
     * | decl_stmt
     * | if_stmt
     * | while_stmt
     * | return_stmt
     * | block_stmt
     * | empty_stmt
     * @throws CompileError
     */
    private void analyseStmt() throws CompileError{
        //声明语句
        //decl_stmt -> let_decl_stmt | const_decl_stmt
        if(check(TokenType.LET_KW)||check(TokenType.CONST_KW))
            analyseDeclStmt();

        //if语句
        //if_stmt -> 'if' expr block_stmt ('else' (block_stmt | if_stmt))?
        else if(check(TokenType.IF_KW))
            analyseIfStmt();

        //while语句
        //while_stmt -> 'while' expr block_stmt
        else if(check(TokenType.WHILE_KW))
            analyseWhileStmt();

        //return语句
        //return_stmt -> 'return' expr? ';'
        else if(check(TokenType.RETURN_KW))
            analyseReturnStmt();

        //代码块
        //block_stmt -> '{' stmt* '}'
        else if(check(TokenType.L_BRACE))
            analyseBlockStmt();

        //空语句
        //empty_stmt -> ';'
        else if(check(TokenType.SEMICOLON))
            analyseEmptyStmt();

        //break语句
        //break_stmt -> 'break' ';'
        else if(check(TokenType.BREAK_KW)){
            analyseBreakStmt();
        }

        //continue语句
        //continue_stmt -> 'continue' ';'
        else if(check(TokenType.CONTINUE_KW)){
            analyseContinueStmt();
        }

        //表达式语句
        //expr_stmt -> expr ';'
        else
            analyseExprStmt();
    }

    /**
     * if语句分析函数
     * if_stmt -> 'if' expr block_stmt ('else' (block_stmt | if_stmt))?
     * @throws CompileError
     */
    private void analyseIfStmt() throws CompileError{
        expect(TokenType.IF_KW);
        String type = analyseExpr();
        //弹栈
        while (!op.empty())
            MyFunctions.operatorInstructions(op.pop(), instructions, type);

        //if语句判断里面的表达式返回类型不能是void，只能是int、double
        //如果不是int或者double，则报错
        if(!type.equals("int") && !type.equals("double"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        //如果前面的计算值非0则跳转
        instructions.add(new Instruction("br.true", 1));
        //无条件跳转
        Instruction jump = new Instruction("br", 0);
        instructions.add(jump);
        //当前指令位置
        int index = instructions.size();

        analyseBlockStmt();
        //在执行完if的代码段后的指令位置
        int size = instructions.size();

        //如果该if在函数里，且有return
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

    /**
     * while语句分析函数
     * while_stmt -> 'while' expr block_stmt
     * @throws CompileError
     */
    private void analyseWhileStmt() throws CompileError{
        expect(TokenType.WHILE_KW);

        instructions.add(new Instruction("br", 0));
        int whileStart = instructions.size();

        String type = analyseExpr();
        //弹栈
        while (!op.empty())
            MyFunctions.operatorInstructions(op.pop(), instructions, type);

        //while语句判断里面的表达式返回类型不能是void，只能是int、double
        //如果不是int或者double，则报错
        if(!type.equals("int") && !type.equals("double"))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        //brTrue
        instructions.add(new Instruction("br.true", 1));
        //br
        Instruction jumpInstruction = new Instruction("br", 0);
        instructions.add(jumpInstruction);
        int index = instructions.size();

        isInWhile++;
        analyseBlockStmt();
        if(isInWhile > 0) isInWhile--;


        //跳回while 判断语句
        Instruction instruction = new Instruction("br", 0);
        instructions.add(instruction);
        int whileEnd = instructions.size();
        instruction.setX(whileStart - whileEnd);

        //修改break语句的参数
        if(breakInstruction.size()!=0){
            for(BreakAndContinue b:breakInstruction){
                if(b.getWhileNum() == isInWhile+1)
                    b.getInstruction().setX(whileEnd - b.getLocation());
            }
        }

        //修改continue语句的参数
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


    /**
     * return语句分析函数
     * return_stmt -> 'return' expr? ';'
     * @throws CompileError
     */
    private void analyseReturnStmt() throws CompileError{
        expect(TokenType.RETURN_KW);
        String type = "void";

        //如果返回类型不是void
        if(!nowFuntion.getReturnType().equals("void")){
            //加载返回地址
            instructions.add(new Instruction("arga", 0));

            type = analyseExpr();
            while (!op.empty())
                MyFunctions.operatorInstructions(op.pop(), instructions, type);

            //放入地址中
            instructions.add(new Instruction("store.64", null));
        }

        if(!check(TokenType.SEMICOLON))
            type = analyseExpr();

        //判断返回类型和函数的应有返回类型是否一致
        //一致则返回
        //不一致则报错
        if(!type.equals(nowFuntion.getReturnType()))
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());

        expect(TokenType.SEMICOLON);
        returnFunction = nowFuntion;

        while (!op.empty())
            MyFunctions.operatorInstructions(op.pop(), instructions, type);
        //ret
        instructions.add(new Instruction("ret", null));
    }

    /**
     * 空语句分析函数
     * empty_stmt -> ';'
     * @throws CompileError
     */
    private void analyseEmptyStmt() throws CompileError{
        expect(TokenType.SEMICOLON);
    }

    /**
     * break语句分析函数
     * break_stmt -> 'break' ';'
     * @throws CompileError
     */
    private void analyseBreakStmt() throws CompileError{
        expect(TokenType.BREAK_KW);
        //如果当前语句不在循环体内，则报错
        if(isInWhile == 0)
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        Instruction instruction = new Instruction("br", 0);
        breakInstruction.add(new BreakAndContinue(instruction, instructions.size()+1, isInWhile));
        instructions.add(instruction);
        expect(TokenType.SEMICOLON);
    }

    /**
     * continue语句分析函数
     * continue_stmt -> 'continue' ';'
     * @throws CompileError
     */
    private void analyseContinueStmt() throws CompileError{
        expect(TokenType.CONTINUE_KW);
        if(isInWhile == 0)
            throw new AnalyzeError(ErrorCode.Break, peekedToken.getStartPos());
        Instruction instruction = new Instruction("br", 0);
        continueInstruction.add(new BreakAndContinue(instruction, instructions.size()+1, isInWhile));
        instructions.add(instruction);
        expect(TokenType.SEMICOLON);
    }

    /**
     * 表达式语句
     * expr_stmt -> expr ';
     * @throws CompileError
     */
    private void analyseExprStmt() throws CompileError{
        String exprType = analyseExpr();
        //弹栈
        while (!op.empty())
            MyFunctions.operatorInstructions(op.pop(), instructions, exprType);
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
