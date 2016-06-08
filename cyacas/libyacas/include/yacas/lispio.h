/** \file lispio.h
 * definitions of pure input output classes.
 */


#ifndef YACAS_LISPIO_H
#define YACAS_LISPIO_H

#include <cstddef>
#include <string>

#include "lisptype.h"

class InputStatus
{
public:
  InputStatus() : iFileName("none") , iLineNumber(-1)  {}

  void SetTo(const std::string& aFileName);
  void RestoreFrom(InputStatus& aPreviousStatus);
  LispInt LineNumber();
  const std::string& FileName();
  void NextLine();

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
  std::string iFileName;
  LispInt  iLineNumber;
};

inline
LispInt InputStatus::LineNumber()
{
  return iLineNumber;
}

inline
const std::string& InputStatus::FileName()
{
  return iFileName;
}

inline
void InputStatus::NextLine()
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
  virtual ~LispInput() = default;

  /// Return the next character in the file
  virtual char32_t Next() = 0;

  /** Peek at the next character in the file, without advancing the file
   *  pointer.
   */
  virtual char32_t Peek() = 0;

  virtual const InputStatus& Status() const {return iStatus;};

  /// Check if the file position is past the end of the file.
  virtual bool EndOfStream() const = 0;

  virtual std::size_t Position() const = 0;
  virtual void SetPosition(std::size_t aPosition) = 0;
protected:
  InputStatus& iStatus;
};

#endif

