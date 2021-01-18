import java.sql.SQLOutput;
import java.util.regex.Pattern;

public class Tokenizer {
    // 字符迭代器
    private StringIter it;

    public Tokenizer(StringIter it) {
        this.it = it;
    }

    // 这里本来是想实现 Iterator<Token> 的，但是 Iterator 不允许抛异常，于是就这样了

    /** 获取下一个 Token */
    public Token nextToken() throws TokenizeError {
        it.readAll();

        // 跳过之前的所有空白字符
        skipSpaceCharacters();

        if (it.isEOF()) {
            return new Token(TokenType.EOF, "", it.currentPos(), it.currentPos());
        }

        char peek = it.peekChar();
        //关键字或标识符
        if (Character.isAlphabetic(peek)) {
            return lexIdentOrKeyword();
        }
        //无符号整数或浮点数常量
        else if (Character.isDigit(peek)) {
            return lexUIntOrDouble();
        }
        //字符串常量
        else if (peek == '"') {
            return lexString();
        }
        //标识符
        else if (peek == '_') {
            return lexIdent();
        }
        //字符常量
        else if (peek == '\'') {
            return lexChar();
        }
        //运算符或者注释
        else {
            return lexOperatorOrAnnotation();
        }
    }

    //无符号整数或者浮点数
    private Token lexUIntOrDouble() throws TokenizeError {
        //如果是无符号整数则为1，如果是浮点数则为2，如果错误则为0
        int type = 1;

        String num = "" ;
        while (Character.isDigit(it.peekChar()) || it.peekChar()=='.' || it.peekChar() == 'e' || it.peekChar() == 'E' || it.peekChar() == '+' || it.peekChar() == '-') {
            num += it.nextChar();
        }
//        String doubleLiteral="[0-9]+ . [0-9]+ ([eE] [+-]? [0-9]+)?";
        //摸鱼版
        String doubleLiteral = "[0-9]+.[0-9]+([eE][-+]?[0-9]+)?";
        String uintLiteral = "[0-9]+";
        if(Pattern.matches(uintLiteral, num))
            return new Token(TokenType.UINT_LITERAL, Integer.parseInt(num), it.previousPos(), it.currentPos());
        else if(Pattern.matches(doubleLiteral, num))
            return new Token(TokenType.DOUBLE_LITERAL, Double.valueOf(num.toString()), it.previousPos(), it.currentPos());
        else
            throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
    }

    private Token lexIdentOrKeyword() throws TokenizeError {
        // 请填空：
        // 直到查看下一个字符不是数字或字母为止:
        // -- 前进一个字符，并存储这个字符
        //
        // 尝试将存储的字符串解释为关键字
        // -- 如果是关键字，则返回关键字类型的 token
        // -- 否则，返回标识符
        //
        // Token 的 Value 应填写标识符或关键字的字符串
        String token = "" ;
        while (Character.isLetterOrDigit(it.peekChar()) || it.peekChar() == '_') {
            token += it.nextChar();
        }

        //test  后面多加了个或
        if (token.equals("fn"))
            return new Token(TokenType.FN_KW, token, it.previousPos(), it.currentPos());

        else if (token.equals("let"))
            return new Token(TokenType.LET_KW, token, it.previousPos(), it.currentPos());

        else if (token.equals("const"))
            return new Token(TokenType.CONST_KW, token, it.previousPos(), it.currentPos());

        else if (token.equals("as"))
            return new Token(TokenType.AS_KW, token, it.previousPos(), it.currentPos());

        else if (token.equals("while"))
            return new Token(TokenType.WHILE_KW, token, it.previousPos(), it.currentPos());

        else if (token.equals("if"))
            return new Token(TokenType.IF_KW, token, it.previousPos(), it.currentPos());

        else if (token.equals("else"))
            return new Token(TokenType.ELSE_KW, token, it.previousPos(), it.currentPos());

        else if (token.equals("return"))
            return new Token(TokenType.RETURN_KW, token, it.previousPos(), it.currentPos());

        else if (token.equals("break"))
            return new Token(TokenType.BREAK_KW, token, it.previousPos(), it.currentPos());

        else if (token.equals("continue"))
            return new Token(TokenType.CONTINUE_KW, token, it.previousPos(), it.currentPos());

        else
            return new Token(TokenType.IDENT, token, it.previousPos(), it.currentPos());
    }


    private Token lexOperatorOrAnnotation() throws TokenizeError {
        switch (it.nextChar()) {
            //+
            case '+':
                return new Token(TokenType.PLUS, '+', it.previousPos(), it.currentPos());

            //-或者->
            case '-':
                if (it.peekChar() == '>') {
                    it.nextChar();
                    return new Token(TokenType.ARROW, "->", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.MINUS, '-', it.previousPos(), it.currentPos());

            case '*':
                return new Token(TokenType.MUL, '*', it.previousPos(), it.currentPos());

            //有可能是注释或者除法 /或者//
            case '/':
                if(it.peekChar() == '/'){
                    it.nextChar();
                    char now = it.nextChar();
                    while(true){
                        if(now == '\n') break;
                        now = it.nextChar();
                    }
                    return nextToken();
                }
                else
                    return new Token(TokenType.DIV, '/', it.previousPos(), it.currentPos());

            //=或者==
            case '=':
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.EQ, "==", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.ASSIGN, '=', it.previousPos(), it.currentPos());

            //!=
            case '!':
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.NEQ, "!=", it.previousPos(), it.currentPos());
                }
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());

                //<或者<=
            case '<':
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.LE, "<=", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.LT, '<', it.previousPos(), it.currentPos());

            //>或者>=
            case '>':
                if (it.peekChar() == '=') {
                    it.nextChar();
                    return new Token(TokenType.GE, ">=", it.previousPos(), it.currentPos());
                }
                return new Token(TokenType.GT, '>', it.previousPos(), it.currentPos());

            case '(':
                return new Token(TokenType.L_PAREN, '(', it.previousPos(), it.currentPos());

            case ')':
                return new Token(TokenType.R_PAREN, ')', it.previousPos(), it.currentPos());

            case '{':
                return new Token(TokenType.L_BRACE, '{', it.previousPos(), it.currentPos());

            case '}':
                return new Token(TokenType.R_BRACE, '}', it.previousPos(), it.currentPos());

            case ',':
                return new Token(TokenType.COMMA, ',', it.previousPos(), it.currentPos());

            case ':':
                return new Token(TokenType.COLON, ':', it.previousPos(), it.currentPos());

            case ';':
                return new Token(TokenType.SEMICOLON, ';', it.previousPos(), it.currentPos());

            default:
                // 不认识这个输入，摸了
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
        }
    }

    private void skipSpaceCharacters() {
        while (!it.isEOF() && Character.isWhitespace(it.peekChar())) {
            it.nextChar();
        }
    }

    //标识符
    private Token lexIdent() throws TokenizeError {
        String token = "" ;
        int i = 100;
        while (Character.isLetterOrDigit(it.peekChar()) && i > 0) {
            token = token + it.nextChar();
            i--;
        }
        return new Token(TokenType.IDENT, token, it.previousPos(), it.currentPos());
    }


    //字符串常量
    private Token lexString() throws TokenizeError {
        String stringLiteral = "" ;
        char pre = it.nextChar();
        int i = 65535;
        char now;
        while (i > 0) {
            now = it.nextChar();
            if (pre == '\\') {
                if (now == '\\') {
                    stringLiteral += '\\';
                    pre = ' ';
                    i--;
                }
                else if (now == 'n') {
                    stringLiteral += '\n';
                    pre = 'n';
                    i--;
                }
                else if (now == '"') {
                    stringLiteral += '"';
                    pre = '"';
                    i--;
                }
                else if(now == '\''){
                    stringLiteral += '\'';
                    pre = '\'';
                    i--;
                }
            }
            else {
                if (now == '"') break;
                else if (now != '\\') stringLiteral += now;
                pre = now;
                i--;
            }
        }
        return new Token(TokenType.STRING_LITERAL, stringLiteral, it.previousPos(), it.currentPos());
    }

    //字符常量
    private Token lexChar() throws TokenizeError{
        char c = it.nextChar();
        if(c == '\''){
            c = it.nextChar();
            //不能是单引号
            if(c == '\'')
                throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
            else if(c == '\\'){
                c = it.nextChar();
                char cc = it.nextChar();
                if(cc != '\'')
                    throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
                if(c == '\'')
                    return new Token(TokenType.CHAR_LITERAL, (int) '\'', it.previousPos(), it.currentPos());
                else if(c == '"')
                    return new Token(TokenType.CHAR_LITERAL, (int) '"', it.previousPos(), it.currentPos());
                else if(c == '\\')
                    return new Token(TokenType.CHAR_LITERAL, (int) '\\', it.previousPos(), it.currentPos());
                else if(c == 't')
                    return new Token(TokenType.CHAR_LITERAL, (int) '\t', it.previousPos(), it.currentPos());
                else if(c == 'r')
                    return new Token(TokenType.CHAR_LITERAL, (int) '\r', it.previousPos(), it.currentPos());
                else if(c == 'n')
                    return new Token(TokenType.CHAR_LITERAL, (int) '\n', it.previousPos(), it.currentPos());
                else
                    throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
            }
            else{
                if(it.nextChar() == '\'')
                    return new Token(TokenType.CHAR_LITERAL, (int) c, it.previousPos(), it.currentPos());
                else
                    throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
            }
        }
        throw new TokenizeError(ErrorCode.InvalidInput, it.previousPos());
    }

}
