/** \file lispio.h
 * definitions of pure input output classes.
 */


#ifndef YACAS_LISPIO_H
#define YACAS_LISPIO_H

#include "yacasbase.h"

#include <cstddef>

// Hope this forward declaration doesn't screw us over...
class InputDirectories;
class InputStatus : public YacasBase
{
public:
  InputStatus() : iFileName("none") , iLineNumber(-1)  {}

  void SetTo(const LispChar * aFileName);
  void RestoreFrom(InputStatus& aPreviousStatus);
  inline LispInt LineNumber();
  inline const LispChar * FileName();
  inline void NextLine();

  inline InputStatus(const InputStatus& aOther) : iFileName(aOther.iFileName) , iLineNumber(aOther.iLineNumber)
  {
  }

  inline InputStatus& operator=(const InputStatus& aOther)
  {
    iFileName   = aOther.iFileName;
    iLineNumber = aOther.iLineNumber;
    return *this;
  }

private:
  const LispChar * iFileName;
  LispInt  iLineNumber;
};

inline LispInt InputStatus::LineNumber()
{
  return iLineNumber;
}
inline const LispChar * InputStatus::FileName()
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
class LispInput : public YacasBase
{
public:
  /** Constructor with InputStatus. InputStatus retains the information
   * needed when an error occurred, and the file has already been
   * closed.
   */
  LispInput(InputStatus& aStatus) : iStatus(aStatus) {};
  virtual ~LispInput() = default;

  /// Return the next character in the file
  virtual LispChar Next() = 0;

  /** Peek at the next character in the file, without advancing the file
   *  pointer.
   */
  virtual LispChar Peek() = 0;

  virtual const InputStatus& Status() const {return iStatus;};

  /// Check if the file position is past the end of the file.
  virtual bool EndOfStream() const = 0;
  /** StartPtr returns the start of a buffer, if there is one.
   * Implementations of this class can keep the file in memory
   * as a whole, and return the start pointer and current position.
   * Especially the parsing code requires this, because it can then
   * efficiently look up a symbol in the hash table without having to
   * first create a buffer to hold the symbol in. If StartPtr is supported,
   * the whole file should be in memory for the whole period the file
   * is being read.
   */
  virtual const LispChar* StartPtr() = 0;
  virtual std::size_t Position() const = 0;
  virtual void SetPosition(std::size_t aPosition) = 0;
protected:
  InputStatus& iStatus;
};

#endif

