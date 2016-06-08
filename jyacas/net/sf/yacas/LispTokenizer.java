package net.sf.yacas;

class LispTokenizer {

    /// Return a string representing the next token
    String NextToken(LispInput aInput, LispHashTable aHashTable) throws Exception {

        char c;

        for (;;) {
            if (aInput.EndOfStream())
                return aHashTable.LookUp("");

            c = aInput.Next();

            if (Character.isWhitespace(c))
                continue;

            // parse comments
            if (c == '/' && aInput.Peek() == '*') {
                aInput.Next();
                for (;;) {
                    while (aInput.Next() != '*' && !aInput.EndOfStream())
                        ;

                    LispError.Check(!aInput.EndOfStream(), LispError.KLispErrCommentToEndOfFile);

                    if (aInput.Peek() == '/') {
                        aInput.Next();
                        break;
                    }
                }

                continue;
            }

            if (c == '/' && aInput.Peek() == '/') {
                aInput.Next();
                while (aInput.Next() != '\n' && !aInput.EndOfStream())
                    ;
                continue;
            }

            break;
        }

        // parse brackets
        if (c == '(' || c == ')' || c == '{' || c == '}' || c == '[' || c == ']')
            return aHashTable.LookUp(Character.toString(c));

        // percent
        if (c == '%')
            return aHashTable.LookUp(Character.toString(c));

        // comma and semicolon
        if (c == ',' || c == ';')
            return aHashTable.LookUp(Character.toString(c));

        // parse . or ..
        if (c == '.' && !Character.isDigit(aInput.Peek())) {
            StringBuilder token = new StringBuilder();
            token.append(c);
            while (aInput.Peek() == '.')
                token.append(aInput.Next());
            return aHashTable.LookUp(token.toString());
        }

        if (c == '\"') {
            StringBuilder str = new StringBuilder();
            str.append(c);
            while (aInput.Peek() != '\"') {
                if (aInput.Peek() == '\\') {
                    aInput.Next();
                    LispError.Check(!aInput.EndOfStream(), LispError.KLispErrParsingInput);
                }

                str.append(aInput.Next());

                LispError.Check(!aInput.EndOfStream(), LispError.KLispErrParsingInput);
            }
            str.append(aInput.Next());
            return aHashTable.LookUp(str.toString());
        }

        // parse atoms
        if (Character.isAlphabetic(c) || c == '\'') {
            StringBuilder atom = new StringBuilder();
            atom.append(c);
            while (Character.isAlphabetic(aInput.Peek()) || aInput.Peek() == '\'' || Character.isDigit(aInput.Peek()))
                atom.append(aInput.Next());
            return aHashTable.LookUp(atom.toString());
        }

        // parse operators
        if (IsSymbolic(c)) {
            StringBuilder op = new StringBuilder();
            op.append(c);
            while (IsSymbolic(aInput.Peek()))
                op.append(aInput.Next());
            return aHashTable.LookUp(op.toString());
        }

        // parse subscripts
        if (c == '_') {
            StringBuilder token = new StringBuilder();
            token.append(c);
            while (aInput.Peek() == '_')
                token.append(aInput.Next());
            return aHashTable.LookUp(token.toString());
        }

        // parse numbers
        if (Character.isDigit(c) || c == '.') {
            StringBuilder number = new StringBuilder();
            number.append(c);

            while (Character.isDigit(aInput.Peek()))
                number.append(aInput.Next());

            if (aInput.Peek() == '.') {
                number.append(aInput.Next());
                while (Character.isDigit(aInput.Peek()))
                    number.append(aInput.Next());
            }

            if (aInput.Peek() == 'e' || aInput.Peek() == 'E') {
                number.append(aInput.Next());
                if (aInput.Peek() == '-' || aInput.Peek() == '+')
                    number.append(aInput.Next());

                while (Character.isDigit(aInput.Peek()))
                    number.append(aInput.Next());
            }

            return aHashTable.LookUp(number.toString());
        }

        LispError.Check(false, LispError.KInvalidToken);

        return aHashTable.LookUp("");
    }

    static boolean IsAlpha(char c) {
        return ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '\''));
    }

    static boolean IsAlNum(char c) {
        return (IsAlpha(c) || Character.isDigit(c));
    }

    static String symbolics = "~`!@#$^&*-=+:<>?/\\|";

    static boolean IsSymbolic(char c) {
        return (symbolics.indexOf(c) >= 0);
    }
};
