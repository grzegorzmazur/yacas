package net.sf.yacas;


/** \class LispInput : pure abstract class declaring the interface
 *  that needs to be implemented by a file (something that expressions
 *  can be read from).
 */
abstract class LispInput
{
    /** Constructor with InputStatus. InputStatus retains the information
     * needed when an error occurred, and the file has already been
     * closed.
     */
    public LispInput(InputStatus aStatus)
  {
    iStatus = aStatus;
  }

    /// Return the next character in the file
    public abstract char Next() throws Exception;

    /** Peek at the next character in the file, without advancing the file
     *  pointer.
     */
    public abstract char Peek() throws Exception;

    public InputStatus Status()
  {
    return iStatus;
  }

    /// Check if the file position is past the end of the file.
    public abstract boolean EndOfStream();

    public abstract int Position();
    public abstract void SetPosition(int aPosition);

    InputStatus iStatus;
}
