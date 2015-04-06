package net.sf.yacas;

class XmlTokenizer extends LispTokenizer {
    public String NextToken(LispInput aInput, LispHashTable aHashTable) throws Exception {

        char c;

        while (Character.isWhitespace(aInput.Peek()))
            aInput.Next();

        if (aInput.EndOfStream())
            return aHashTable.LookUp("");

        StringBuilder token = new StringBuilder();

        c = aInput.Next();
        token.append(c);

        if (c == '<') {
            while (c != '>') {
                c = aInput.Next();
                LispError.Check(!aInput.EndOfStream(), LispError.KLispErrCommentToEndOfFile);
                token.append(c);
            }
        } else {
            while (aInput.Peek() != '<' && !aInput.EndOfStream()) {
                c = aInput.Next();
                token.append(c);
            }
        }
        return aHashTable.LookUp(token.toString());
    }
}
