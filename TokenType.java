

public enum TokenType {
    //关键字
    //FN_KW     -> 'fn'
    FN_KW,
    //LET_KW    -> 'let'
    LET_KW,
    //CONST_KW  -> 'const'
    CONST_KW,
    //AS_KW     -> 'as'
    AS_KW,
    //WHILE_KW  -> 'while'
    WHILE_KW,
    //IF_KW     -> 'if'
    IF_KW,
    //ELSE_KW   -> 'else'
    ELSE_KW,
    //RETURN_KW -> 'return'
    RETURN_KW,
    //扩展：BREAK_KW  -> 'break'
    BREAK_KW,
    //扩展：CONTINUE_KW -> 'continue'
    CONTINUE_KW,

    //字面量
    //无符号整数
    UINT_LITERAL,
    //字符串常量
    STRING_LITERAL,
    //扩展：浮点数常量
    DOUBLE_LITERAL,
    //扩展：字符常量
    CHAR_LITERAL,

    //标识符
    IDENT,

    //运算符
    //PLUS      -> '+'
    PLUS,
    //MINUS     -> '-'
    MINUS,
    //MUL       -> '*'
    MUL,
    //DIV       -> '/'
    DIV,
    //ASSIGN    -> '='
    ASSIGN,
    //EQ        -> '=='
    EQ,
    //NEQ       -> '!='
    NEQ,
    //LT        -> '<'
    LT,
    //GT        -> '>'
    GT,
    //LE        -> '<='
    LE,
    //GE        -> '>='
    GE,
    //L_PAREN   -> '('
    L_PAREN,
    //R_PAREN   -> ')'
    R_PAREN,
    //L_BRACE   -> '{'
    L_BRACE,
    //R_BRACE   -> '}'
    R_BRACE,
    //ARROW     -> '->'
    ARROW,
    //COMMA     -> ','
    COMMA,
    //COLON     -> ':'
    COLON,
    //SEMICOLON -> ';'
    SEMICOLON,

    //扩展：注释
    COMMENT,

    //空
    None,
    //EOF
    EOF;

    @Override
    public String toString() {
        switch (this) {
            case FN_KW:
                return "fn";
            case LET_KW:
                return "let";
            case CONST_KW:
                return "const";
            case AS_KW:
                return "as";
            case WHILE_KW:
                return "while";
            case IF_KW:
                return "if";
            case ELSE_KW:
                return "else";
            case RETURN_KW:
                return "return";
            case BREAK_KW:
                return "break";
            case CONTINUE_KW:
                return "continue";

            case UINT_LITERAL:
                return "UnsignedInteger";
            case STRING_LITERAL:
                return "string";
            case DOUBLE_LITERAL:
                return "double";
            case CHAR_LITERAL:
                return "char";

            case IDENT:
                return "Identifier";

            case PLUS:
                return "PlusSign";
            case MINUS:
                return "MinusSign";
            case MUL:
                return "MulSign";
            case DIV:
                return "DivSign";
            case ASSIGN:
                return "AssignSign";
            case EQ:
                return "EQSign";
            case NEQ:
                return "NEQSign";
            case LT:
                return "LTSign";
            case GT:
                return "GTSign";
            case LE:
                return "LESign";
            case GE:
                return "GESign";
            case L_PAREN:
                return "LeftBracket";
            case R_PAREN:
                return "RightBracket";
            case L_BRACE:
                return "LeftBigBracket";
            case R_BRACE:
                return "RightBigBracket";
            case ARROW:
                return "ArrowSign";
            case COMMA:
                return "CommaSign";
            case COLON:
                return "ColonSign";
            case SEMICOLON:
                return "SemicolonSign";

            case COMMENT:
                return "comment";

            case None:
                return "NullToken";
            case EOF:
                return "EOF";

            default:
                return "InvalidToken";
        }
    }
}
