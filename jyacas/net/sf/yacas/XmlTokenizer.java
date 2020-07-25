package net.sf.yacas;

class XmlTokenizer extends LispTokenizer {
    @Override
    public String NextToken(LispInput aInput, LispHashTable aHashTable) throws Exception {

        char c;

        if (aInput.EndOfStream())
            return aHashTable.LookUp("");

        StringBuilder leadingSpaces = new StringBuilder();
        while (Character.isWhitespace(aInput.Peek()))
            leadingSpaces.append(aInput.Next());

        if (aInput.EndOfStream())
            return aHashTable.LookUp("");

        StringBuilder token = new StringBuilder();

        c = aInput.Next();
        token.append(c);

        if (c == '<') {
            while (c != '>') {
                LispError.Check(!aInput.EndOfStream(), LispError.KLispErrCommentToEndOfFile);
                c = aInput.Next();
                token.append(c);
            }
        } else {
            while (aInput.Peek() != '<' && !aInput.EndOfStream()) {
                c = aInput.Next();
                token.append(c);
            }
            leadingSpaces.append(token);
            token = leadingSpaces;
        }
        return aHashTable.LookUp(token.toString());
    }
}
