/** \file lispio.h
 * definitions of pure input output classes.
 */


#ifndef __lispio_h__
#define __lispio_h__

#include "lisptype.h"

// Hope this forward declaration doesn't screw us over...
class InputDirectories;
class InputStatus
{
public:
    InputStatus() : iFileName("none"), iLineString('\0'), iLineNumber(-1)  {};
	~InputStatus();

    void SetTo(LispCharPtr aFileName);
    void RestoreFrom(InputStatus& aPreviousStatus);
    inline LispInt LineNumber();
    inline LispCharPtr FileName();
    inline void NextLine();

    LispCharPtr Line(InputDirectories& aInputDirectories);
private:
    LispCharPtr iFileName;
    LispCharPtr iLineString;
    LispInt		iLineNumber;
};

inline LispInt InputStatus::LineNumber()
{
    return iLineNumber;
}
inline LispCharPtr InputStatus::FileName()
{
    return iFileName;
}
inline void InputStatus::NextLine()
{
    iLineNumber++;
}

/** \class LispInput : pure abstract class declaring the interface
 *  that needs to be implemented by a file (something that expressions
 *  can be read from).
 */
class LispInput
{
public:
    /** Constructor with InputStatus. InputStatus retains the information
     * needed when an error occurred, and the file has already been
     * closed.
     */
    LispInput(InputStatus& aStatus) : iStatus(aStatus) {};
    virtual ~LispInput();

    /// Return the next character in the file
    virtual LispChar Next() = 0;

    /** Peek at the next character in the file, without advancing the file
     *  pointer.
     */
    virtual LispChar Peek() = 0;

    /// Check if the file position is past the end of the file.
    virtual LispBoolean EndOfStream() = 0;
    /** StartPtr returns the start of a buffer, if there is one.
     * Implementations of this class can keep the file in memory
     * as a whole, and return the start pointer and current position.
     * Especially the parsing code requires this, because it can then
     * efficiently look up a symbol in the hash table without having to
     * first create a buffer to hold the symbol in. If StartPtr is supported,
     * the whole file should be in memory for the whole period the file
     * is being read.
     */
    virtual LispCharPtr StartPtr() = 0;
    virtual LispInt Position() = 0;
protected:
    InputStatus& iStatus;
};

/** \class LispOutput : interface an output object should adhere to.
 */
class LispOutput
{
public:
    virtual ~LispOutput();
    /// Write out one character.
    virtual void PutChar(LispChar aChar) = 0;

public:
    void Write(LispCharPtr aString);
};

#endif

